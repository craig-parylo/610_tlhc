#'------------------------------------------------------------------------------
#' TLHC GENERAL FUNCTIONS
#' 
#' Small, general-purpose functions
#'------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(tidyverse)     # tidy data processing
library(here)          # localise file references
library(fs)            # file system management
library(clock)         # datetime functions
library(DBI)           # sql connection
library(dbplyr)        # tidy sql processing

#' Archive current data files
#' 
#' Moves all .Rds files within the tlhc data folder to a new folder named for
#' todays date.
#'
#' @return
#' @export
#'
#' @examples
archive_current_data_files <- function() {
  
  # create a destination folder named with today's date
  dest <- dir_create(path = here('data', 'tlhc', Sys.Date()))
  
  # list .Rds files in the current folder then identify their new paths
  files <- dir_ls(path = here('data', 'tlhc'), regexp = '*.Rds') |> 
    as_tibble() |> 
    rename(source_filepath = value) |> 
    mutate(
      # extract file name
      filename = path_file(source_filepath),
      # create a destination path
      dest_filepath = path(dest, filename)
    )
  
  # move the files
  file_move(path = files$source_filepath, new_path = files$dest_filepath)

}

#archive_current_data_files()

#' Update the user on script completion with a message
#' 
#' Prints a timestamped message to the console to update the user on script completion.
#'
#' @param icon String emoji icon
#' @param message String message to be printed
#'
#' @return
#' @export
#'
#' @examples
update_user <- function(message = '', stage = '', icon = '☑️') {
  
  # tidy the input
  stage <- trimws(tolower(stage))

  # prepare time
  time <- clock::date_format(date_now(zone = ''), format = '%H:%M:%S')

  # update the icon if end of script
  icon <- case_when(
    stage == 'end' ~ '🔚', # if end of script then use this
    !icon == '☑️' ~ icon,  # if non-standard icon then use that
    .default = '☑️'        # default to check
  )

  # add blank lines if start of script
  spacing <- case_when(
    stage == 'start' ~ strrep('\n', 10),
    .default = ''
  )

  # determine if start/end output or a general message
  str_print <- case_when(
    stage == 'start' ~ paste('==', message),
    stage == 'end' ~ paste(icon, time, 'Script complete'),
    .default = paste(icon, time, message),
  )

  # adorn with lines if start or end
  str_print <- case_when(
    stage %in% c('start', 'end') ~ paste(str_print, strrep(x = '=', times = 80 - nchar(str_print))),
    TRUE ~ str_print
  )

  # output the message
  cat(paste0(spacing, str_print, '\n'))
}


#' Get Southampton's TransactionIDs for invites
#' 
#' To be used to update the tlhc_download_sql process to ensure we avoid
#' downloading new Southampton invite records - because these have been repurposed
#' and need downloading and processing separately
#'
#' @return
#' @export
#'
#' @examples
get_southampton_invite_transaction_ids <- function() {
  # Setup ------------------------------------------------------------------------
  # set up connection to the tlhc sql database
  con <- dbConnect(
    odbc::odbc(),
    .connection_string = "Driver={SQL Server};SERVER=MLCSU-BI-SQL;DATABASE=TLHC_Reporting"
  )
  
  # update the user
  #update_user(message = 'Setup complete')
  
  # Download the data ------------------------------------------------------------
  df_southampton_new_invite_transactions <- tbl(con, in_schema('dbo', 'tbTLHCTLHC_Pathway_Invite')) |> # lazy load
    filter(substr(SubmittedZipFile, 1, 5) == 'RHM00') |> # only use Southampton's data
    select(TransactionId, ReceivedDate, UserEmail) |> # limit to required fields
    distinct() |> # remove duplication
    arrange(desc(TransactionId)) |> # sort so the latest transaction is at the top
    filter(TransactionId > 181294) |> # limit to the newer transactions that we want to adjust for
    collect() # download the data
  
  dbDisconnect(con)
  
  # update the user
  #update_user(message = 'Southampton new invite data downloaded')
  
  # show the results
  view(df_southampton_new_invite_transactions)
}