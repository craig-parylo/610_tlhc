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
update_user <- function(message = '', stage = '', icon = '✔️️') {
  
  # tidy the input
  stage <- trimws(tolower(stage))

  # prepare time
  time <- clock::date_format(date_now(zone = ''), format = '%H:%M:%S')

  # update the icon if end of script
  icon <- case_when(
    stage == 'end' ~ '🔚', # if end of script then use this
    !icon == '✔️️' ~ icon,  # if non-standard icon then use that
    .default = '✔️️'        # default to check
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


# summarise a single df
#' Describe a dataframe
#' 
#' Produces summary figures for a given dataframe including a list of the variables
#' the class of these variables, the total number of rows, how many missing values
#' each variable has and the rate, the level of unique values (cardinality) per 
#' variable and the rate.
#'
#' @param df Tibble - the dataframe to be described
#'
#' @return Tibble with summary figures
#' @examples describe_df(df_demo) |> clipr::write_clip()
describe_df <- function(df) {
  
  # take a subset of the df, excluding calculated columns and specified cols from db
  cols_ignore <- c(
    'CSURowNumber',
    'DatasetId',
    'SubmittedZipFile',
    'SubmittedFile',
    'FileId',
    'TransactionId',
    'ReceivedDate',
    'LoadDate',
    'UserEmail'
  )
  
  df <- df |> 
    # ignore calculated columns
    select(!contains('calc_') & !any_of(cols_ignore))
  
  # convert junk values to NA
  df <- df |>
    mutate(across(where(is.character), ~na_if(., ''))) |>
    mutate(across(where(is.character), ~na_if(., 'NULL'))) |>
    mutate(across(where(is.character), ~na_if(., 'null'))) |>
    mutate(across(where(is.character), ~na_if(., 'Null')))
  
  # create an empty tibble to hold our results
  df_return <- tibble(
    variable = NA,
    class = NA,
    rows = NA,
    missing = NA,
    missing_rate = NA,
    cardinality = NA,
    cardinality_rate = NA
  )
  
  # loop over each variable and work out some useful statistics
  for (var in names(df)) {
    df_return <- df_return |> 
      add_row(
        variable = var,
        class = class(df[[var]])[1],
        rows = length(df[[var]]),
        missing = length(df[[var]][is.na(df[[var]])]),
        missing_rate = missing / rows,
        cardinality = length(unique(df[[var]][!is.na(df[[var]])])),
        cardinality_rate = cardinality / rows
      )
  }
  
  # ignore the first row of NAs
  df_return <- df_return |> 
    filter(!is.na(variable))
  
  # return the result
  return(df_return)
}
