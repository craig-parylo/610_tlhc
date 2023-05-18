#' -----------------------------------------------------------------------------
#' TLHC COLLATE DATA FROM DATASHEETS
#' 
#' Authors: Craig Parylo
#' Started: 2022-09-05
#' 
#' PROJECT
#' 610 Targeted Lung Health Checks
#' 
#' AIM
#' To provide a source of data for Management Information (MI) reporting for 
#' the TLHC project.
#' 
#' OBJECTIVES
#' 1. Collate submitted data (in the form of .xls* datasheets) from participating 
#'    projects to a single data source 
#' 2. Output the consolidated data as an .Rds file - for use in generating the TLHC report
#' 3. Output a flat file for sharing with NHSE   
#' -----------------------------------------------------------------------------

# Libraries ----
library(tidyverse)        # tidy data wrangling
library(here)             # localised file references
library(zoo)              # working with dates (yearmon)
library(unglue)           # extract details from strings
library(lubridate)        # working with dates
library(readxl)           # reading from excel
library(parallel)         # parallel processing
library(future.apply)     # parallel processing
library(progressr)        # progress bar
library(tictoc)           # process timing

source(here('scripts', 'tlhc_general_functions.R'))

# Notify user 
update_user(stage = 'start', message = 'tlhc_collate_datasheets.R')
tic()

## Initialise variables ----
#end_month   <- as_date('2022-11-01') # NB, update this each month
end_month   <- floor_date(today(), unit = 'month') %m-% months(2) # default to two months ago (in line with typical report cycle)
start_month <- as_date('2019-04-01')

# Functions ----

#' read_tlhc_file --------------------------------------------------------------
#' Set up parallel processing
#' Load files and append data to files df as a list object
#' 
#' NB, parallel processing approach based the following thread:
#' https://stackoverflow.com/questions/62916053/r-asynchronous-parallel-lapply

#' Function to read and process data from excel files
#' 
#' @description 
#' Reads and returns details from an Excel (.xls/x) file.
#' 
#' @param folder_file String relative path to the file to be processed.
read_tlhc_file <- function(folder_file) {
  
  # read the file to a temp variable
  xl <- read_excel(
    path = file.path(Sys.getenv('base_azure'), 'reporting_monthly', 'DATA NEW', folder_file),
    sheet = 'Template',
    range = 'A2:C5000',
    #col_names = c('Data_ID', 'Data_Name', 'Numerator', 'Denominator', 'Value'),
    col_names = c('Data_ID', 'Data_Name', 'Numerator'),
    #col_types = c('text', 'text', 'numeric', 'numeric', 'numeric'),
    col_types = c('text', 'text', 'numeric'),
    trim_ws = TRUE
  ) |> 
    # drop rows that don't have a metric ID
    drop_na(Data_ID) |> 
    # add additional columns
    mutate(
      # add a key to the folder_file (to allow for subsequent joins)
      folder_file = folder_file,
      
      # dq tests ---
      # flag metric ids not in official list
      dq_metric_id = Data_ID %in% ref_metrics$Data_ID,
      
      # flag NAs in Numerator
      dq_numerator_na = is.na(Numerator),
      
      # flag zeroes in Numerator
      dq_numerator_zero = (Numerator == 0),
      
      # overall dq flag
      dq_flag = dq_metric_id & dq_numerator_na & dq_numerator_zero
    )
  
  # signal progress - update the user
  p()
  
  # return the df
  return(xl)
  
  # housekeeping
  rm(xl)
}

# Preparation ------------------------------------------------------------------

## Reference data ----

# Project details
ref_projects = read_excel(
  path = here('data', 'reference', 'project_reference.xlsx')
)

# metric details
ref_metrics <- read_excel(
  path = here('data', 'templates', 'tlhc_datasheet_template.xlsx')
) |> 
  select(!Numerator) # drop the 'numerator' column as not necessary

# report months - sequence of months from start to end date
ref_months <- seq.Date(
  from = start_month,
  to = end_month,
  by = 'month'
) |> 
  # convert to tibble
  as_tibble() |> 
  # name the column 'month_date'
  select(month_date = value) |>
  # sort months newest first
  arrange(desc(month_date)) |> 
  # convert date to character representation of month
  mutate(month = as.yearmon(month_date))

# update the user
update_user(message = 'Variables initialised and reference data loaded')

# Process files ----------------------------------------------------------------

## Identify files ----
#' Identify submission files and extract key information about each

# what files are stored?
file_list <- list.files(
  path = file.path(Sys.getenv('base_azure'), 'reporting_monthly', 'DATA NEW'), 
  recursive = T,
  pattern = '.xls*'
) |> 
  # convert to tibble
  as_tibble() |> 
  # name the column 'folder_file' - as shows the sub-folder and files
  select(folder_file = value)

# process the list a little
file_list <- file_list |> 
  mutate(
    # Extract details ---
    # extract CCG code, month name and year from the folder_file string
    unglue_data(
      x = folder_file,
      patterns = '{ccg_name}/{ccg_code}_{month_name}_{year}.xlsx'
    ),
    
    # Calculate a date for the file based on month and year
    file_date = as_date(paste(1, month_name, year), format = '%d %b %Y'),
    
    # Calculate a YearMonth for the file
    file_month = as.yearmon(file_date),
    
    # data quality checks ---
    # flag if CCG code is in our pre-defined list
    dq_ccg_code = ccg_code %in% ref_projects$project_code,
    
    # flag if month is in our pre-defined list
    dq_file_month = file_month %in% ref_months$month,
    
    # flag if working.xlsx or template file
    #dq_file_working_xlsx = folder_file != 'Working.xlsx',
    dq_file_working_xlsx = !folder_file %in% c('Working.xlsx', 'ProjCode_Month_Year.xlsx'),
    
    # flag if has a '~' in the name e.g. (~$myfile.xlsx) - these are backups and not needed
    dq_file_backup = str_detect(
      string = folder_file,
      pattern = '~',
      negate = TRUE
    ),
    
    # flag if is an archived file
    dq_file_archived = str_detect(
      string = folder_file,
      pattern = 'archive|Archive',
      negate = TRUE
    ),
    
    # flag if fits pattern 'folder_name/ccgcode_month_year.xlsx' - exclude others that snuck into the list
    dq_filename = str_detect(
      string = folder_file,
      pattern = '.*/.*_.*_\\d{4}\\.xlsx',
      negate = FALSE
    ),
    
    # produce a single overall dq_flag
    dq_flag = dq_ccg_code & dq_file_month & dq_file_working_xlsx & dq_file_backup & dq_filename & dq_file_archived,
    
    # display a text reason for exclusion
    dq_message = case_when(
      !dq_file_working_xlsx ~ 'Excluded',
      !dq_file_backup ~ 'Excluded - backup file',
      !dq_file_archived ~ 'Archived',
      !dq_filename ~ 'DQ - folder/file not in expected format',
      !dq_ccg_code ~ 'DQ - CCG code not in accepted list',
      !dq_file_month ~ 'DQ - month is not in reporting period',
      TRUE ~ 'Unknown'
    )
  )

# pick out the files of interest and prepare details
files <- file_list |> 
  filter(dq_flag == TRUE) # these files all pass the dq tests


# identify files that have been excluded - for DQ checks
files_excluded <- file_list |> 
  filter(dq_flag == FALSE) |> 
  select(
    folder_file,
    dq_flag,
    dq_message
  )

# housekeeping
rm(file_list)

# update the user
update_user('Files identified')

## Read files ----
#' Read files and gather details for metric/numerator/denominator/values.
#' 
#' Using parallel processing to speed up this process (Z:/ drive 'feels' slow)
#' For each identified file, read the data to a tibble and store this as
#' a list object within the files_data tibble.

# set up for parallel processing
handlers(handler_progress(format='[:bar] :percent :eta :message')) # set up the progress indicator
plan('multisession') # set up the future.apply package

# initiate tlhc file read process with progress indicator
update_user(message = 'Reading files ...', icon = '⏱️')
with_progress({
  p <- progressor(steps = length(files$folder_file))
  files_data <- files |> 
    mutate(
      data = future_lapply(
        X = folder_file,
        FUN = read_tlhc_file
      )
    )
})
update_user(message = paste('...', length(files$folder_file), 'files read'))

# housekeeping
rm(p)

# Display a list of excluded files (ignore archived files)
update_user(message = paste('...', length(files_excluded$folder_file), 'files excluded - displaying for review'), icon = '⚠️')
view(files_excluded |> filter(str_detect(folder_file, 'archive|Archive', negate = T)))


## Extract data ----
#' Consolidate individual datasets (stored as list elements within files_data) 
#' to a single tibble - replacing files_data in the process

files_data <- bind_rows(files_data$data)

# coerce columns to known data types
files_data <- files_data |> 
  mutate(
    Data_ID = as.character(Data_ID),
    Data_Name = as.character(Data_Name),
    Numerator = as.integer(Numerator),
    folder_file = as.character(folder_file)
  )

## Data cleansing ----

files_data <- files_data |> 
  # metric 6e
  # NB, there are two versions of metric 6e throughout project history, which is confusing.
  # The old measure counted 'patients with a lung cancer diagnosis' whereas the new
  # one is interested in lung cancers that cannot be staged. This step removes any
  # old 6e values from the totals so we can be sure we're reporting just the new one.
  filter(!Data_Name == 'Patients with a Lung Cancer diagnosis') |> 
  
  # metric 4 - referred for ldct
  # NB, metric 4a was previously identified by metric ID '4', this was changed with
  # the addition of metric 4b (risk score for ldct but ineligible for scan). Many
  # of the data collection templates still refer to metric 4, however. This process
  # casts them all as '4a'.
  mutate(
    Data_ID = case_when(
      Data_ID %in% c('4', 4) ~ '4a',
      TRUE ~ Data_ID
    )
  )

update_user(message = 'Data collated from individual submissions')

## Submitted data ----
#' Process the submitted data to combine the file meta data with the metric 
#' details recorded in each of the files

# get a simpler list of files (don't need the dq fields at this point, also
# don't need the ccg_name as will get official version from the template)
temp_files <- files |> 
  select(
    folder_file,
    #ccg_name,
    ccg_code,
    file_date,
    month = file_month # need this as a key for subsequent join
  )

# get a simpler list of completed metrics (don't need the metric name)
temp_data <- files_data |> 
  select(
    folder_file,
    Data_ID,
    Numerator,
  )

# left-join files_data to the files table keyed on folder_file
submitted_data <- left_join(
  x = temp_files,
  y = temp_data,
  by = "folder_file"
)

# housekeeping
rm(temp_files, temp_data)
update_user(message = 'Combined submission and metadata information')


## MI template ----
#' Construct a template detailing which projects, metrics and months are 
#' expected in the MI report.
#' Populate this template with the data received (submitted_data)

# get a temporary list of projects and alias a few fields
temp_projects <- ref_projects |> 
  select(
    ccg_code = project_code,
    ccg_name = project,
    ccg_phase = phase,
    start_date,
    eligible_population,
    CACode,
    CAName
  )

# get a simple list of metrics
temp_metrics <- ref_metrics |> 
  rename(
    Data_ID = `Data Item Id`,
    Data_Name = `Data Item Name`
  )

# get a simple list of months
temp_months <- ref_months |> 
  select(month, month_date)

# get project/metric template by doing a full outer join using the 'by = character()'
mi_template <- left_join(
  x = temp_projects,
  y = temp_metrics,
  by = character()
)

# add months to project/metric template by doing another full outer join
mi_template <- left_join(
  x = mi_template, 
  y = temp_months,
  by = character()
)

# add submitted data to the template
# keyed on ccg_code, Data_ID, and month
mi_data <- left_join(
  x = mi_template,
  y = submitted_data,
  by = c("ccg_code", "month", 'Data_ID')
) |> 
  # sort by project, metric and month
  arrange(
    ccg_code,
    Data_ID,
    desc(month_date)
  )

# update the user
update_user(message = 'Populated MI template with submitted data')


# -- Outputs --------------------------------------------------------------------
#' Output the results from this process

#' Save the flat file for subsequent use
write_rds(
  x = mi_data,
  file = here('data', 'tlhc', 'tlhc_mi_data.Rds')
)


# copy the excluded files to clipboard for checking
#write.table(files_excluded, 'clipboard', sep='\t', row.names = F, col.names = T)

# update the user
#cat(paste('☑️', Sys.time(), 'Submission data consolidated\n', sep = ' '))


# -- Flat file -----------------------------------------------------------------
# get a copy of the mi_data minus median metrics
df_flatfile <- mi_data |> 
  filter(!Data_ID %in% c(10, 11, 12, 13)) |>  # don't want the median metrics
  select(-Data_Name) # metric names will be replaced by the list accepted by NHSE

# read the metric name specification (they specified no changes to metric name)
df_metric_name <- read_excel(
  path = here('data', 'reference', 'flatfile_metric_map.xlsx')
)

# add in the metric names
df_flatfile <- left_join(
  x = df_flatfile,
  y = df_metric_name |> select(Data_ID = metric_id, Data_Name = metric_name),
  by = 'Data_ID'
)

# finalise the file
df_flatfile <- df_flatfile |> 
  # order and alias fields correctly
  select(
    CACode,
    CAName,
    ProjectCode = ccg_code,
    Project = ccg_name,
    MetricId =Data_ID,
    Metric = Data_Name,
    month,
    Numerator
  )

# output the flatfile
if(!file.exists(here('outputs'))){dir.create(path = here('outputs'))} # create the outputs folder if it doesn't already exist
write_csv(
  x = df_flatfile,
  file = here('outputs', paste0('tlhc_midata_flatfile_', today(), '.csv'))
)


# done!
update_user(stage = 'end')
toc()