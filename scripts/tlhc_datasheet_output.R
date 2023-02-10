#'------------------------------------------------------------------------------
#' PRODUCE MONTHLY DATASHEETS FOR RECORD-LEVEL PROJECTS
#'
#' OBJECTIVES
#' Output the results of the SQL data collation process into separate datasheets
#' ready to go into DATA NEW folders
#' -----------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(tidyverse)   # tidy data processing
library(openxlsx)    # output the results into Excel files
library(here)        # localised filepaths
library(furrr)       # parallel processing
library(progressr)   # progress bar
library(lubridate)   # working with dates
library(janitor)     #

# config -----------------------------------------------------------------------
months_backwards <- 2 # the number of months backwards to generate reports

# use this if refreshing from the start of the programme:
#months_backwards <- abs(interval(as.Date('2019-04-01'), floor_date(today(), unit = 'month') %m-% months(2)) %/% months(1))


# Notify user 
cat(rep('\n', 50)) # 50 blank lines to clear the console
cat('== tlhc_datasheet_output ==================================================\n')

# Load data --------------------------------------------------------------------

# read our aggregated metric data
df <- read_rds(file = here('data', 'tlhc', 'tlhc_metrics.Rds')) |> 
  ungroup() |> 
  mutate(
    # adjust metric ids to match the report template
    metric_id = case_when(
      metric_id == '4' ~ '4a', 
      metric_id == '14a' ~ '14',
      TRUE ~ metric_id
    )
  )

# read manual adjustments
manual_adjustments <- readxl::read_excel(
  path = file.path(Sys.getenv('base_365'), 'Monthly MI reporting', 'Data processing procedure', 'tlhc_manual_adjustments.xlsx')
) |> 
  # filter out retired adjustments
  filter(active == 'TRUE', is.na(date_removed)) |> 
  # convert month to a date
  #  mutate(month = as.numeric(format(floor_date(month), '%Y%m'))) |> 
  mutate(month = as.yearmon(floor_date(month))) |> 
  # select required details and alias names
  select(
    project,
    month,
    metric_id,
    adjusted_numerator = numerator
  )

# read project lookup
ref_project <- readxl::read_excel(
  path = here('data', 'reference', 'project_reference.xlsx')
)

# open the template workbook
wb <- loadWorkbook(
  file = here(
    'data',
    'templates',
    'tlhc_datasheet_template.xlsx'
  )
)

# read the template details to a dataframe
df_template <- read.xlsx(xlsxFile = wb, cols = c(1:2,6)) |> 
  rename(
    data_item_id = `Data.Item.Id`,
    data_item_name = `Data.Item.Name`,
    #denominator_source = `Denominator.source`
  )

# notify user
cat(paste('☑️', Sys.time(), 'Variables initialised and reference data loaded\n', sep = ' '))

# Process data -----------------------------------------------------------------

#' function to save output file
#' 
#' @param par_project_code String The project code this file relates to
#' @param par_project String The name of the project this file relates to
#' @param par_month Yearmon The month this file relates to
#' 
save_output_file <- function(par_project_code, par_project, par_month) {
  
  par_month <- as.yearmon(par_month)
  
  # Get a summary for the project at the specified month
  df_pm <- df |> 
    # filter for our data
    filter(project == par_project, month == par_month)
  
  # calculate denominator and value
  # df_pm <- df_pm |>
  #   mutate(
  #     denominator = case_when(
  #       metric_id %in% c('3a', '3b', '3c', '3d') ~ df_pm$numerator[match('2', df_pm$metric_id)],
  #       metric_id %in% c('4b', '5a', '6f') ~ df_pm$numerator[match('4', df_pm$metric_id)],
  #       metric_id %in% c('2') ~ df_pm$numerator[match('1a', df_pm$metric_id)],
  #       metric_id %in% c('1a', '1c') ~ df_pm$numerator[match('1b', df_pm$metric_id)],
  #       metric_id %in% c('4', '4a', '7', '7a', '7b', '7c', '7d', '7e', '7f', '7g', '7h', '7i', '7j', '7k', '7l', '7m', '7o', '7p', '7q', '7r', '7s', '7t', '7u', '8b') ~ (df_pm$numerator[match('3a', df_pm$metric_id)] + df_pm$numerator[match('3b', df_pm$metric_id)]),
  #       metric_id %in% c('5d', '5e', '5f', '14') ~ df_pm$numerator[match('5a', df_pm$metric_id)],
  #       metric_id %in% c('5b') ~ df_pm$numerator[match('5c', df_pm$metric_id)],
  #       metric_id %in% c('6a', '6b', '6c', '6d', '6e') ~ (df_pm$numerator[match('6a', df_pm$metric_id)] + df_pm$numerator[match('6b', df_pm$metric_id)] + df_pm$numerator[match('6c', df_pm$metric_id)] + df_pm$numerator[match('6d', df_pm$metric_id)]),
  #       metric_id %in% c('9') ~ df_pm$numerator[match('8a', df_pm$metric_id)],
  #       metric_id %in% c('8a') ~ df_pm$numerator[match('8b', df_pm$metric_id)]
  #     ),
  # 
  #     value = case_when(
  #       denominator > 0 ~ numerator / denominator
  #     )
  #   )
  
  # add to output structure
  df_pm <- left_join(
    x = df_template,
    y = df_pm |> rename(data_item_id = metric_id),
    by = 'data_item_id'
  ) |> 
    # ensure items are in the right order
    select(
      data_item_id,
      data_item_name,
      numerator,
      # denominator,
      # value,
      # denominator_source,
      comment
    )
  
  # ensure NAs are reported as zeroes
  df_pm <- df_pm |> 
    mutate(
      numerator = replace_na(numerator, 0),
      # denominator = replace_na(denominator, 0),
      # value = replace_na(value, 0)
    )
  
  # add to the template file
  wb_output <- wb
  
  writeData(
    wb = wb_output,
    sheet = 'Template',
    x = df_pm,
    startCol = 1,
    startRow = 2,
    colNames = F, # just output the data
  )
  
  # update progress
  p()
  
  # get a filename compatible with DATA NEW - i.e. Bradford_202210_36J_Oct_2022.xlsx
  # this allows these files to be copied into DATA NEW quickly
  # the existing file archived manually
  # the first part of the filename can be deleted manually and be compatible with the 07 process
  fn <- paste(
    par_project_code,
    format(par_month, '%b_%Y'),
    sep = '_'
  )
  
  # save this file
  saveWorkbook(
    wb = wb_output,
    file = here(
      'data',
      'datasheets',
      paste0(par_project, '_', format(par_month, '%Y%m'), '_', fn, '.xlsx')
    ),
    overwrite = TRUE,
    returnValue = TRUE
  )
  
}

## prepare data ----

### manual adjustments ----

# summarise metric performance by project, month and metric
df <- df |> 
  # ensure a full set of metric_ids for each month
  complete(nesting(project, month), nesting(metric_id, metric_name))

# add in manual adjustments
df <- left_join(
  x = df,
  y = manual_adjustments,
  by = c('project', 'month', 'metric_id')
) |>
  # rename the numerator to make way for the coalesced version
  rename(numerator_old = numerator) |> 
  mutate(
    # keep only the best data
    numerator = coalesce(adjusted_numerator, numerator_old),
    
    # annotate when we've overwritten data with a manual adjustment
    comment = case_when(
      !is.na(adjusted_numerator) ~ paste('Manual adjustment, original data was', numerator_old)
    )
  ) |> 
  # select fields for output
  select(-numerator_old)

#### testing --------------
# # see what the comparisons are showing
# df_pm |> 
#   filter(!is.na(adjusted_numerator)) |> 
#   view()
# 
# # view specific examples
# df_pm |> 
#   filter(
#     !is.na(manual_adjustment),
#     project == 'Corby',
#     metric_id == '5e'
#   ) |> 
#   adorn_totals('row') |> 
#   view()

# corby 1a - go with manual adjustment ***
# corby 3a - get more from numerator
# corby 4 - get slightly more from numerator
# corby 5a - manual adjustment has more months' data ***
# corby 5d - manual adjustment has more months' data ***
# corby 5e - numerator has more data


### project codes ----
# add in project code reference (helps with adding to DATA NEW)
df <- left_join(
  x = df,
  y = ref_project |> select(project, project_code),
  by = 'project'
)


### define period ----
# get first of the current month
#base_date <- Sys.Date() - as.numeric(format(Sys.Date(), format = '%d')) + 1

# get the date of the first of last month
base_date <- floor_date(today(), unit = 'month') %m-% months(2)

# generate a sequence of months backwards
rolling_months <- as_tibble(seq(base_date, length=months_backwards, by = '-1 month')) |> 
  mutate(yearmon = as.yearmon(value))

# get a list of projects with data in month sequence
proj_month <- df |>
  group_by(project_code, project, month) |> 
  summarise() |>
  ungroup() |> 
  filter(month %in% rolling_months$yearmon)

# notify user
cat(paste('☑️', Sys.time(), 'Data prepared\n', sep = ' '))


## parallel processing ----

# notify re: long process
cat(paste('⏱️', Sys.time(), 'Generating datasheets, please wait ...\n', sep = ' '))

# open the folder (or create if it doesn't exist)
if(!file.exists(here('data', 'datasheets')){dir.create(path = here('data', 'datasheets'))})
browseURL(url = here('data', 'datasheets'))

# generate datasheet files for these projects for these months
handlers(handler_progress(format='[:bar] :percent :eta :message')) # set up the progress indicator
plan('multisession') # set up the future.apply package

# begin parallel processes
with_progress({
  p <- progressor(steps = length(proj_month$month))
  proj_month <- proj_month |> 
    mutate(
      data = future_pmap(
        .l = list(project_code, project, month),
        .f = save_output_file,
        .options = furrr_options(seed=NULL)
      )
    )
})

# update the user
cat(paste('☑️', Sys.time(), 'Datasheets generated\n', sep = ' '))

# update the user
cat(paste('☑️', Sys.time(), 'Script complete ===================================\n', sep = ' '))