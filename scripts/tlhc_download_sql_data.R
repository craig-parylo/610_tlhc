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
      
      # 2023-03-15 Mansfield and Ashfield - conflicting submissions from John Taylor (M&A analyst) when they've agreed to use InHealth
      193337, 190088, 187407, 184381, 181572, 178642, 174358, 171666,
      166818, 162700, 159250, 156519, 154427, 150593, 148604, 146601,
      144689, 142285, 136238, 134308, 131832,
      
      # 2023-03-27 Bradford and North Kirklees - agreed with Kerrie Massey and Graham Bowmer today to exclude all submissions before March 2023 as they confirm their recent re-submissions are the most up-to-date and accurate data
      189740, 187617, 185024, 181645, 178639, 175549, 171166, 165345, 158882
      
    )
    
    df <- tbl(con, in_schema('dbo', str_table)) |> # lazy load
      #head(n=100000) |> # temporary - for testing
      filter(!TransactionId %in% invalid_transid) |> # ignore invalid transactions
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
      
      # 2023-03-15 Mansfield and Ashfield - conflicting submissions from John Taylor (M&A analyst) when they've agreed to use InHealth
      193337, 190088, 187407, 184381, 181572, 178642, 174358, 171666,
      166818, 159250, 156519, 154427, 150593, 148604, 146601, 144689,
      142285, 136238, 134308, 131832
      
    )
    
    df <- tbl(con, in_schema('dbo', str_table)) |> # lazy load
      filter(!TransactionId %in% invalid_transid) |> # ignore invalid transactions
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
      
      # 2023-03-15 Doncaster transactions leading to over-reported 8b (offered SC) agreed to be removed today
      130374, 131748, 134772, 137383, 140163, 142235, 142270, 144804,
      146557, 148934, 150726, 154626, 156928, 159567, 163913, 167075,
      169080, 169267, 171092, 175619, 178824, 181324
    )
    
    df <- tbl(con, in_schema('dbo', 'tbTLHCTLHC_SmokingCessation')) |> # lazy load
      filter(!TransactionId %in% invalid_transid) |> # ignore invalid transactions
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
      
      # 2023-04-05 Southampton newer submissions overwrite First_Letter dates and need to be managed separately
      # NNB, this list will need adding to each month to exclude the latest submission (RHM00)
      206415, 201904, 197086, 194098, 192749, 190354, 187373, 186011, 184105, 
      181295, 181294
    )
    
    df <- tbl(con, in_schema('dbo', 'tbTLHCTLHC_Pathway_Invite')) |> # lazy load
      filter(!TransactionId %in% invalid_transid) |> # ignore invalid transactions
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
    invalid_transid <- c('')
    
    df <- tbl(con, in_schema('dbo', 'tbTLHCTLHC_LungHealthCheck')) |> # lazy load
      filter(!TransactionId %in% invalid_transid) |> # ignore invalid transactions
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
      # Luton South Bedfordshire - contains invalid ParticipantIDs
      194295
    )
    
    df <- tbl(con, in_schema('dbo', 'tbTLHCTLHC_Demographics')) |> # lazy load
      filter(!TransactionId %in% invalid_transid) |> # ignore invalid transactions
      group_by(ParticipantID) |> # get one record for each participant
      slice_max(ReceivedDate) |> # get record(s) with the latest datetime received
      slice_max(CSURowNumber) |> # get record(s) with the highest CSU row number
      filter(row_number(ParticipantID)==1) |> # get the first row where multiples still exist
      collect() # download the data
    
    rm(invalid_transid)
    
  } else {
    ## All other tables ----
    # We need a record per patient
    
    df <- tbl(con, in_schema('dbo', str_table)) |> # lazy load
      group_by(ParticipantID) |> # get one record for each participant:
      slice_max(ReceivedDate) |> # get record(s) with the latest datetime received
      slice_max(CSURowNumber) |> # get record(s) with the highest CSU row number
      filter(row_number(ParticipantID)==1) |>  # get the first row where multiples still exist
      collect() # download the data
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
    'tbTLHCTLHC_SmokingCessation'
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
  # this works but it quite slow ------------------------
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