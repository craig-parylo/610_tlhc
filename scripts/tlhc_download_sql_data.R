#'------------------------------------------------------------------------------
#' TLHC DOWNLOAD SQL DATA
#' 
#' Download data from MLCSU SQL tables and prepare for subsequent use.
#' 
#' Assumptions:
#' 1. A patient can only have 1 valid record in each table EXCEPT
#'  1a. LDCT table (patient can have multiple scans), so take a valid record for each scan date
#'  1b. Diagnoses table (patient can have multiple diagnoses as a result of the scan)
#' 2. If a patient has multiple records then take the latest one (based on submission date)
#' 3. Save the tables as RDS files
#' 
#'------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(tidyverse)     # tidy data processing
library(here)          # localise file references
library(DBI)           # database connections
library(dbplyr)        # tidy database processing
library(lubridate)     # date functions
library(zoo)           # date functions (yearmonths)
# library(purrr)         # processing
# library(future.apply)
# library(furrr)         # parallel processing
# library(future)        # parallel processing
library(progressr)     # progress bars
library(tictoc)        # timing processes
library(readxl)        # read xlsx reference files

source(here('scripts', 'tlhc_general_functions.R'))

# Notify user 
update_user(stage = 'start', message = 'tlhc_latest_submissions.R')
update_user(icon = '⚠️', message = 'Please allow up to 30 minutes for this process to run')
tic()

# Setup ------------------------------------------------------------------------
# set up connection to the tlhc sql database
con <- dbConnect(
  odbc::odbc(),
  .connection_string = "Driver={SQL Server};SERVER=MLCSU-BI-SQL;DATABASE=TLHC_Reporting"
)
# set up connection to the Strategic Social Care database (for lsoa details)
con2 <- dbConnect(
  odbc::odbc(),
  .connection_string = "Driver={SQL Server};SERVER=MLCSU-BI-SQL;DATABASE=StrategicSocialCare"
)

# load a list of participantid to exclude from processing
df_participants_exclude <- read_excel(
  path = file.path(Sys.getenv('base_365'), 'Monthly MI reporting', 'Data processing procedure', 'tlhc_participant_ignore_list.xlsx'),
  sheet = 'participant_ignore'
)
# get as a vector
df_participants_exclude_list <- df_participants_exclude |> 
  filter(active == T) |> # only exclude participants where we actively want to
  select(ParticipantID) |> 
  unique() |> 
  as_vector()

# prepare a list of transaction ids that should be excluded for all tables
invalid_transid_all <- c(
  # 2023-11-29 Hull - agreed to cancel this submission as contains East Riding patients and may have been pseudonymised using the wrong salt key
  227941,
  
  # 2024-01-24 - Tameside and Glossop to exclude TransactionID due to error submission. 
  235983,
  
  # 2024-04-08 - Doncaster - combined data with Blackburn Darwen and Blackpool - data will be resubmitted
  245570, 244465, 250372, 250370
)

# UDF --------------------------------------------------------------------------

#' Download TLHC table ---------------------------------------------------------
#' 
#' Downloads a nominated TLHC table from the SQL database and saves it locally
#' as an RDS file
#' 
#' @param str_table String name of the table to be downloaded from SQL
#' @param str_name String name for the resulting local file
#'
#' @return NULL
download_tlhc_table <- function(str_table = '') {
  
  # create a connection
  con <- dbConnect(
    odbc::odbc(),
    # Driver = 'SQL Server',
    # Server = 'MLCSU-BI-SQL',
    # Database = 'TLHC_Reporting',
    # Port = 1433
    .connection_string = "Driver={SQL Server};SERVER=MLCSU-BI-SQL;DATABASE=TLHC_Reporting"
  )
  
  # decide how to process
  if(str_table == 'tbTLHCTLHC_Pathway_LDCT') {
    ## LDCT ----
    # We need a record per patient per scan
    
    # define transactions to ignore
    invalid_transid <- c(
      
      # transactions to be removed from all tables
      invalid_transid_all,
      
      # 2023-03-15 Mansfield and Ashfield - conflicting submissions from John Taylor (M&A analyst) when they've agreed to use InHealth
      193337, 190088, 187407, 184381, 181572, 178642, 174358, 171666,
      166818, 162700, 159250, 156519, 154427, 150593, 148604, 146601,
      144689, 142285, 136238, 134308, 131832,
      
      # 2023-03-27 Bradford and North Kirklees - agreed with Kerrie Massey and Graham Bowmer today to exclude all submissions before March 2023 as they confirm their recent re-submissions are the most up-to-date and accurate data
      189740, 187617, 185024, 181645, 178639, 175549, 171166, 165345, 158882,
      
      # 2023-11-30 Luton South Bedfordshire - cancelling out previous ldct submissions as we have a more recent full resubmission
      228893, 224675, 219030, 214625, 210196, 206768, 203130, 199980, 199915,
      194296, 194294, 193531, 193311, 190117, 188038, 188037, 188036, 188027,
      187314, 184291, 181436, 178659, 175824, 171780, 167021, 164032, 164031,
      163970, 163969, 159609, 157274, 156987, 156986, 154628, 153946, 150441,
      146728, 144819, 142388, 140174, 137385, 134553,
      
      # 2023-12-19 Hull - agreed to exclude older the transactionID
      156613,158862,178204,182807,
      154350,156613,162770,166772,171187,174474,181011,186013,188743,191715,194036,199747,199753,
      201521,204048,207752,211447,217873,221067,227941,
      
      # 2024-01-22 - Thurrock - agreed to exclude older the transactionID with no demographic record because it had already populated in the new submission on December 2023
      207145
    )
    
    df <- tbl(con, in_schema('dbo', str_table)) |> # lazy load
      #head(n=100000) |> # temporary - for testing
      filter(!TransactionId %in% invalid_transid) |> # ignore invalid transactions
      filter(!ParticipantID %in% df_participants_exclude_list) |> # ignore invalid participantIDs
      group_by(ParticipantID, LDCT_Date) |> # get one record for each participant on each day:
      slice_max(ReceivedDate) |> # get record(s) with the latest datetime received
      slice_max(CSURowNumber) |> # get record(s) with the highest CSU row number
      filter(row_number(ParticipantID)==1) |>  # get the first row where multiples still exist
      collect() # download the data
    
    rm(invalid_transid)
    
  } else if (str_table == 'tbTLHCTLHC_Pathway_Diagnostics'){
    ## Diagnostics ----
    # We need a record per patient per full dose CT date
    
    # define transactions to ignore
    invalid_transid <- c(
      
      # transactions to be removed from all tables
      invalid_transid_all,
      
      # 2023-03-15 Mansfield and Ashfield - conflicting submissions from John Taylor (M&A analyst) when they've agreed to use InHealth
      193337, 190088, 187407, 184381, 181572, 178642, 174358, 171666,
      166818, 159250, 156519, 154427, 150593, 148604, 146601, 144689,
      142285, 136238, 134308, 131832,
      
      #2023-12-19 Hull - agreed to exclude older the transactionID
      204048,207752,211447,217873,221067,227941,
      145405,146378,148303,150483,154350,156613,158862,162770,166772,171187,
      174474,178204,181011,182807,186013,188743,191715,194036,199747,199753,
      201521,204048

    )
    
    df <- tbl(con, in_schema('dbo', str_table)) |> # lazy load
      filter(!TransactionId %in% invalid_transid) |> # ignore invalid transactions
      filter(!ParticipantID %in% df_participants_exclude_list) |> # ignore invalid participantIDs
      group_by(ParticipantID, Full_Dose_CT_Date) |> # get one record for each participant on each day:
      slice_max(ReceivedDate) |> # get record(s) with the latest datetime received
      slice_max(CSURowNumber) |> # get record(s) with the highest CSU row number
      filter(row_number(ParticipantID)==1) |> # get the first row where multiples still exist
      collect() # download the data
    
    rm(invalid_transid)
    
  } else if (str_table == 'tbTLHCTLHC_SmokingCessation'){
    ## Smoking cessation ----
    # We need to explicitly exclude some transactions

    # define transactions to ignore
    invalid_transid <- c(
      
      # transactions to be removed from all tables
      invalid_transid_all,
      
      # 2023-07-31 Doncaster and Blackburn Darwen Blackpool submissions with date formatting issues
      213175, 213218,
      
      # 2023-03-15 Doncaster transactions leading to over-reported 8b (offered SC) agreed to be removed today
      130374, 131748, 134772, 137383, 140163, 142235, 142270, 144804,
      146557, 148934, 150726, 154626, 156928, 159567, 163913, 167075,
      169080, 169267, 171092, 175619, 178824, 181324
      
    )
    
    df <- tbl(con, in_schema('dbo', 'tbTLHCTLHC_SmokingCessation')) |> # lazy load
      filter(!TransactionId %in% invalid_transid) |> # ignore invalid transactions
      filter(!ParticipantID %in% df_participants_exclude_list) |> # ignore invalid participantIDs
      group_by(ParticipantID) |> # get one record for each participant
      slice_max(ReceivedDate) |> # get record(s) with the latest datetime received
      slice_max(CSURowNumber) |> # get record(s) with the highest CSU row number
      filter(row_number(ParticipantID)==1) |> # get the first row where multiples still exist
      collect() # download the data
    
    rm(invalid_transid)
  
  } else if (str_table == 'tbTLHCTLHC_Pathway_Invite'){
    ## Invites ----
    # We need to explicitly exclude some transactions
    
    # define transactions to ignore
    invalid_transid <- c(
      
      # transactions to be removed from all tables
      invalid_transid_all,
      
      # 2023-07-31 Doncaster and Blackburn Darwen Blackpool submissions with date formatting issues
      213175, 213218,
      
      # 2023-04-05 Southampton newer submissions overwrite First_Letter dates and need to be managed separately
      # NNB, this list will need adding to each month to exclude the latest submission (RHM00)
      244394, 241097, 236620, 232593, 228258, 224003, 217935, 214302, 210541,     
      210540, 209188, 206415, 201904, 197086, 194098, 192749, 190354, 187373,    
      186011, 184105, 181295, 181294,
      
      #2023-12-19 Hull - agreed to exclude older the transactionID
      140089,142110,145405,146378,148303,150483,154350,156613,158862,162770,166772,171187,
      174474,178204,181011,182807,186013,188743,191715,194036,199747,199753,201521,
      204048,207752,211447,217873,221067,227941

    )
    
    df <- tbl(con, in_schema('dbo', 'tbTLHCTLHC_Pathway_Invite')) |> # lazy load
      filter(!TransactionId %in% invalid_transid) |> # ignore invalid transactions
      filter(!ParticipantID %in% df_participants_exclude_list) |> # ignore invalid participantIDs
      group_by(ParticipantID) |> # get one record for each participant
      slice_max(ReceivedDate) |> # get record(s) with the latest datetime received
      slice_max(CSURowNumber) |> # get record(s) with the highest CSU row number
      filter(row_number(ParticipantID)==1) |> # get the first row where multiples still exist
      collect() # download the data
    
    rm(invalid_transid)
    
  } else if (str_table == 'tbTLHCTLHC_LungHealthCheck'){
    ## LHC ----
    # We need to explicitly exclude some transactions
    
    # define transactions to ignore
    invalid_transid <- c(
      
      # transactions to be removed from all tables
      invalid_transid_all,
      
      # 2023-07-31 Doncaster and Blackburn Darwen Blackpool submissions with date formatting issues
      213175, 213218,
      
      #2023-12-19 Hull - agreed to exclude older the transactionID
      140089,142110,145405,146378,148303,150483,154350,156613,158862,162770,166772,171187,174474,178204,181011,182807,186013,
      188743,191715,182807,194036,199747,199753,201521,204048,207752,211447,217873,221067,227941

    )
    
    df <- tbl(con, in_schema('dbo', 'tbTLHCTLHC_LungHealthCheck')) |> # lazy load
      filter(!TransactionId %in% invalid_transid) |> # ignore invalid transactions
      filter(!ParticipantID %in% df_participants_exclude_list) |> # ignore invalid participantIDs
      group_by(ParticipantID, LHC_Date) |> # get one record for each participant for each LHC date (to account for non-attendances)
      slice_max(ReceivedDate) |> # get record(s) with the latest datetime received
      slice_max(CSURowNumber) |> # get record(s) with the highest CSU row number
      filter(row_number(ParticipantID)==1) |> # get the first row where multiples still exist
      collect() # download the data
    
    rm(invalid_transid)
  
  } else if (str_table == 'tbTLHCTLHC_Demographics'){
    ## Demographics ----
    # We need to explicitly exclude some transactions
    
    # define transactions to ignore
    invalid_transid <- c(
      
      # transactions to be removed from all tables
      invalid_transid_all,
      
      # Luton South Bedfordshire - contains invalid ParticipantIDs
      194295,
      
      #2023-12-19 Hull - agreed to exclude older the transactionID
      140089,142110,145405,146378,148303,150483,154350,156613,158862,162770,166772,171187,174474,
      178204,181011,188743,191715,194036,199747,199753,201521,204048,207752,211447,217873,221067,
      182807,186013,227941

    )
    
    df <- tbl(con, in_schema('dbo', 'tbTLHCTLHC_Demographics')) |> # lazy load
      filter(!TransactionId %in% invalid_transid) |> # ignore invalid transactions
      filter(!ParticipantID %in% df_participants_exclude_list) |> # ignore invalid participantIDs
      group_by(ParticipantID) |> # get one record for each participant
      slice_max(ReceivedDate) |> # get record(s) with the latest datetime received
      slice_max(CSURowNumber) |> # get record(s) with the highest CSU row number
      filter(row_number(ParticipantID)==1) |> # get the first row where multiples still exist
      collect() # download the data
    
    rm(invalid_transid)
  
  } else if (str_table == 'NCRAS_Cancer_Tumour_Data_TLHC'){
    
    # Tumour data table
    # downloading all data - been filtered by DSCRO team already
    df <- tbl(con, in_schema('dbo', str_table)) |> # lazy load
      collect()
  
  } else if (str_table == 'NCRAS_National_Cancer_Pathway_Data_TLHC'){
    
    # Cancer pathway data
    # downloading all data - been filtered by DSCRO team already
    df <- tbl(con, in_schema('dbo', str_table)) |> # lazy load
      collect()
    
  } else if (str_table == 'NCRAS_National_Cancer_Rapid_Registration_TLHC'){
    
    # Rapid Registration data
    # downloading all data - been filtered by DSCRO team already
    df <- tbl(con, in_schema('dbo', str_table)) |> # lazy load
      collect()
    
  } else {
    ## All other tables ----
    # We need a record per patient
    
    # define transactions to ignore
    invalid_transid <- c(
      
      # transactions to be removed from all tables
      invalid_transid_all
    )
    
    df <- tbl(con, in_schema('dbo', str_table)) |> # lazy load
      filter(!TransactionId %in% invalid_transid) |> # ignore invalid transactions
      filter(!ParticipantID %in% df_participants_exclude_list) |> # ignore invalid participantIDs
      group_by(ParticipantID) |> # get one record for each participant:
      slice_max(ReceivedDate) |> # get record(s) with the latest datetime received
      slice_max(CSURowNumber) |> # get record(s) with the highest CSU row number
      filter(row_number(ParticipantID)==1) |>  # get the first row where multiples still exist
      collect() # download the data
    
    rm(invalid_transid)
  }
  
  # signal progress - update the user
  p()
  
  # save it to file
  future:::save_rds(
    object = df,
    pathname = here('data', 'tlhc', paste0(str_table, '.Rds')),
    compress = T # compress the data for ease of storage
  )
  
  # housekeeping
  dbDisconnect(con)
  rm(df)
}

# notify the user
update_user(message = 'Setup complete and UDFs loaded')

# Reference data ---------------------------------------------------------------

# ensure there is a tlhc folder within the data folder
if(!dir.exists(here('data', 'tlhc'))){dir.create(here('data', 'tlhc'))}

# initiate tlhc file read process with progress indicator
update_user(message = 'Loading SQL tables, please wait ...', icon = '⏱️')

# submitting organisations ----
df_projectlu <- tbl(con, in_schema('dbo', 'dboProjectLookup')) |> 
  collect() 

df_projectlu |> 
  saveRDS(file = here('data', 'tlhc', 'dboProjectLookup.Rds'))

# lsoa lookup ----
df_lsoalu <- tbl(con2, in_schema('REF','lkp_LSOADeprivation2019')) |> 
  collect()

df_lsoalu |> 
  saveRDS(file = here('data', 'tlhc', 'lkp_LSOADeprivation2019.Rds'))

# rurality lookup ----
df_ruralitylu <- tbl(con, in_schema('dbo', 'Rural_Urban_Classification')) |> 
  collect()

df_ruralitylu |> 
  saveRDS(file = here('data', 'tlhc', 'Rural_Urban_Classification.Rds'))

# ethnicity lookup ----
df_ethnicitylu <- tbl(con, in_schema('dbo', 'EthnicityLookup')) |> 
  collect()

df_ethnicitylu |> 
  saveRDS(file = here('data', 'tlhc', 'EthnicityLookup.Rds'))

# housekeeping
dbDisconnect(con)
dbDisconnect(con2)

# Download data ----------------------------------------------------------------

## list tables to download
df_table_details <- tibble(
  table = c(
    'tbTLHCTLHC_Demographics',
    'tbTLHCTLHC_LungHealthCheck',
    'tbTLHCTLHC_Measurements',
    'tbTLHCTLHC_OtherHistory',
    'tbTLHCTLHC_Pathway_Diagnostics',
    'tbTLHCTLHC_Pathway_Invite',
    'tbTLHCTLHC_Pathway_LDCT',
    'tbTLHCTLHC_SmokingCessation',
    'NCRAS_Cancer_Tumour_Data_TLHC',
    'NCRAS_National_Cancer_Pathway_Data_TLHC',
    'NCRAS_National_Cancer_Rapid_Registration_TLHC'
  )
)

# NOTE: repeated error messages referring to 'general network error' means the 
# parallel processing approach seems to be failing, so have worked out that lapply
# in a serial process is more reliable.

# Set up the progress bar and parallel processing
# handlers(handler_progress(format='[:bar] :percent :eta :message')) # set up the progress indicator
# plan('multisession') # set up the future.apply package
# 
# # begin parallel processes
# with_progress({
#   
#   # set up progress bar  
#   p <- progressor(steps = length(df_table_details$table))
#   
#   # call function to download table data
#   df_table_details <- df_table_details |>
#     mutate(
#       data = future_pmap(
#         .l = list(table),
#         .f = download_tlhc_table,
#         .options = furrr_options(seed=NULL)
#       )
#     )
#   
# })


# not parallel processed
handlers(handler_progress(format='[:bar] :percent :eta :message')) # set up the progress indicator

# begin serial download processes
with_progress({
  
  # set up progress bar  
  p <- progressor(steps = length(df_table_details$table))
  
  # call function to download table data
  # this works but its quite slow ------------------------
  lapply(
    X = df_table_details$table,
    FUN = download_tlhc_table
  )
  
})




# notify the user
update_user(message = 'SQL tables loaded')

# housekeeping
rm(df_table_details)

# done!
update_user(icon = '🔚', stage = 'end')
toc()