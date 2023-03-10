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

# Notify user 
cat(rep('\n', 50)) # 50 blank lines to clear the console
cat('== tlhc_download_sql_data.R ===============================================\n')
cat(paste('⚠️', Sys.time(), 'Please allow up to 20 minutes for this process to run...\n'))
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
    # LDCT - we need a record per patient per scan
    
    df <- tbl(con, in_schema('dbo', str_table)) |> # lazy load
      #head(n=100000) |> # temporary - for testing
      group_by(ParticipantID, LDCT_Date) |> # get one record for each participant on each day:
      slice_max(ReceivedDate) |> # get record(s) with the latest datetime received
      #slice_max(LoadDate) |> # get record(s) with the latest load datetime loaded
      #slice_max(DatasetId) |> # get record(s) with the latest dataset id
      #slice_max(FileId) |> # get record(s) with the latest file id
      slice_max(CSURowNumber) |> # get record(s) with the highest CSU row number
      filter(row_number(ParticipantID)==1) |>  # get the first row where multiples still exist
      collect() # download the data
    
  } else if (str_table == 'tbTLHCTLHC_Pathway_Diagnostics'){
    # Diagnostics - we need a record per patient per full dose CT date
    
    df <- tbl(con, in_schema('dbo', str_table)) |> # lazy load
      #head(n=100000) |> # temporary - for testing
      group_by(ParticipantID, Full_Dose_CT_Date) |> # get one record for each participant on each day:
      slice_max(ReceivedDate) |> # get record(s) with the latest datetime received
      #slice_max(LoadDate) |> # get record(s) with the latest load datetime loaded
      #slice_max(DatasetId) |> # get record(s) with the latest dataset id
      #slice_max(FileId) |> # get record(s) with the latest file id
      slice_max(CSURowNumber) |> # get record(s) with the highest CSU row number
      filter(row_number(ParticipantID)==1) |> # get the first row where multiples still exist
      collect() # download the data
    
  } else {
    # All other tables - we need a record per patient
    
    df <- tbl(con, in_schema('dbo', str_table)) |> # lazy load
      #head(n=100000) |> # temporary - for testing
      group_by(ParticipantID) |> # get one record for each participant:
      slice_max(ReceivedDate) |> # get record(s) with the latest datetime received
      #slice_max(LoadDate) |> # get record(s) with the latest load datetime loaded
      #slice_max(DatasetId) |> # get record(s) with the latest dataset id
      #slice_max(FileId) |> # get record(s) with the latest file id
      slice_max(CSURowNumber) |> # get record(s) with the highest CSU row number
      #slice_max(ParticipantID, n = 1) |> # get the last record in the group (handles any possible remaining duplicates)
      filter(row_number(ParticipantID)==1) |>  # get the first row where multiples still exist
      collect() # download the data
  }
  
  # signal progress - update the user
  p()
  
  # save it to file
  future:::save_rds(
    object = df,
    pathname = here('data', 'tlhc', paste0(str_table, '.Rds')
    )
  )
  
  # housekeeping
  dbDisconnect(con)
  rm(df)
}

# notify the user
cat(paste('☑️', Sys.time(), 'Setup complete and UDFs loaded\n', sep = ' '))

# Reference data ---------------------------------------------------------------

# ensure there is a tlhc folder within the data folder
if(!dir.exists(here('data', 'tlhc'))){dir.create(here('data', 'tlhc'))}

# initiate tlhc file read process with progress indicator
cat(paste('⏱️', Sys.time(), 'Loading SQL tables, please wait ...\n', sep = ' '))

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
cat(paste('☑️', Sys.time(), 'SQL tables loaded\n', sep = ' '))

# housekeeping
rm(df_table_details)

# done!
cat(paste('🔚', Sys.time(), '== Script complete ================================\n', sep = ' '))
toc()