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
