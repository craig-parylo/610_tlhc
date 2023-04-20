#' -----------------------------------------------------------------------------
#' TLHC Latest Submissions - version 2
#' 
#' Provide information on the latest submissions to each table from each 
#' submitting organisation
#' 
#' -----------------------------------------------------------------------------

# Libraries ----
library(DBI)
library(dbplyr)
library(tidyverse)
library(lubridate)
library(furrr)
library(progressr)
library(here)

# Notify user 
cat(rep('\n', 50)) # 50 blank lines to clear the console
cat('== tlhc_latest_submissions_v2.R ===========================================\n')

# Functions ----
#' Get Transaction Data
#' @param tbl_label Character Human-friendly name for the table.
#' @param tbl_name Character SQL table name.
#' @param con SQL table connection.
#' @param pattern Character regex pattern to help extract project code from file data
get_transaction_data <- function(tbl_label, tbl_name, pattern) {
  
  # create a connection
  con <- dbConnect(
    odbc::odbc(),
    .connection_string = "Driver={SQL Server};SERVER=MLCSU-BI-SQL;DATABASE=TLHC_Reporting"
  )
  
  # set a lower date for the sql query to speed it up a bit
  temp_date_lower <- (today() - months(4))
  
  # download the details from SQL
  sql_table <- tbl(con, in_schema('dbo', tbl_name)) |> 
    filter(ReceivedDate >= temp_date_lower) |> 
    select(SubmittedFile, ReceivedDate, TransactionId, UserEmail) |> 
    distinct() |> 
    collect()
  
  rm(temp_date_lower)
  
  # extract project code and filter for latest submission per project
  sql_table <- sql_table |> 
    mutate(
      # label the table
      table_name = eval(tbl_label),
      # extract the project code from the submitted file
      project_code = str_extract(string = toupper(SubmittedFile), pattern = paste0('_?(\\w{3,5})', pattern)),
      project_code = str_remove(project_code, pattern = pattern),
      project_code = str_remove(project_code, pattern = '_')
    ) |> 
    group_by(project_code) |> 
    mutate(latest_date = max(ReceivedDate)) |> 
    ungroup() |> 
    group_by(project_code, TransactionId, UserEmail) |> 
    filter(ReceivedDate == latest_date)
  
  # signal progress - update the user
  p()
  
  # return the table
  return(sql_table)
  
  # housekeeping
  dbDisconnect(con)
  rm(sql_table)
}

# update the user
cat(paste('☑️', Sys.time(), 'Functions defined\n', sep = ' '))

# Setup ----
# define the connection
con <- dbConnect(
  odbc::odbc(),
  .connection_string = "Driver={SQL Server};SERVER=MLCSU-BI-SQL;DATABASE=TLHC_Reporting"
)

# Gather data ----
# Project lookup
sql_proj_lu <- tbl(con, in_schema('dbo', 'dboProjectLookup')) |>
  collect() |> 
  select(
    project_code = ProjectCode,
    Project_name = ProjectName
  )

cat(paste('☑️', Sys.time(), 'Project lookup downloaded\n', sep = ' '))


## Define input table ----
# create a tibble of variables
tbl_label <- c(
  'Demographics',
  'Lung Health Checks',
  'Measurements',
  'Other History',
  'Diagnostics',
  'Invites',
  'LDCT',
  'Smoking cessation'
)

tbl_name <- c(
  'tbTLHCTLHC_Demographics',
  'tbTLHCTLHC_LungHealthCheck',
  'tbTLHCTLHC_Measurements',
  'tbTLHCTLHC_OtherHistory',
  'tbTLHCTLHC_Pathway_Diagnostics',
  'tbTLHCTLHC_Pathway_Invite',
  'tbTLHCTLHC_Pathway_LDCT',
  'tbTLHCTLHC_SmokingCessation'
)

pattern <- c(
  '_TLHC_DEMOGRAPHICS',
  '_TLHC_PATHWAY',
  '_TLHC_MEASURE',
  '_TLHC_OTHER',
  '_TLHC_PATHWAY',
  '_TLHC_PATHWAY',
  '_TLHC_PATHWAY',
  '_TLHC_PATHWAY'
)

table_details <- tibble(
  tbl_label,
  tbl_name,
  pattern
)
rm(tbl_label, tbl_name, pattern)

cat(paste('☑️', Sys.time(), 'Input table generated\n', sep = ' '))
# 
# test <- table_details |>
#   head(1) |> 
#   mutate(
#     data = future_pmap(
#       .l = list(tbl_label, tbl_name, pattern), 
#       .f = get_transaction_data,
#       .options = furrr_options(seed=NULL)
#     )
#   )


## Parallel processing ----
# notify re: long process
cat(paste('⏱️', Sys.time(), 'Loading SQL tables, please wait ...\n', sep = ' '))

# download details from SQL
plan(multisession)
with_progress({
  p <- progressor(steps = length(table_details$tbl_label))
  
  table_details_data <- table_details |> 
    mutate(
      data = future_pmap(
        .l = list(tbl_label, tbl_name, pattern), 
        .f = get_transaction_data,
        .options = furrr_options(seed=NULL)
      )
    )
})

# extract data and combine to a single tibble
df <- bind_rows(
  table_details_data$data
)
rm(table_details_data)

# update the user
cat(paste('☑️', Sys.time(), 'Data gathered from SQL\n', sep = ' '))

## Data tidy ----
# add in project names
df <- left_join(
  x = df,
  y = sql_proj_lu,
  by = 'project_code'
)

# tidy data a little more
df <- df |> 
  mutate(
    # order in the same was as TransactionID_Master
    table_name = factor(
      x = table_name,
      levels = c('Demographics', 'Lung Health Checks', 'Measurements', 'Other History', 'Diagnostics', 'Invites', 'LDCT', 'Smoking cessation')
    ),
    # add an include/exclude label
    include = case_when(
#      ReceivedDate >= make_date(year = year(today()), month = month(today())-1, day = 15) ~ 'Include',
      ReceivedDate >= make_date(year = year(today()), month = month(today()), day = 15) %m-% months(1) ~ 'Include',
      TRUE ~ 'Exclude'
    )
  )

# create a summary pivot with dates
df_pivot <- df |> 
  ungroup() |> 
  select(
    TransactionId,
    UserEmail,
    project_code,
    Project_name,
    table_name,
    ReceivedDate
  ) |> 
  arrange(table_name) |> 
  pivot_wider(
    names_from = table_name,
    values_from = ReceivedDate
  ) |> 
  arrange(Project_name)

# create a summary pivot for the TransactionID process
df_pivot_transid <- df |> 
  ungroup() |> 
  select(
    TransactionId,
    project_code,
    Project_name,
    table_name,
    include
  ) |> 
  arrange(table_name) |> 
  pivot_wider(
    names_from = table_name,
    values_from = include
  ) |> 
  arrange(Project_name) |> 
  mutate(month = '') |> 
  select(
    TransactionId,
    project_code,
    `Demographics`:`Smoking cessation`,
    month,
    Project_name
  ) |> 
  replace_na(list(
    `Demographics` = 'Exclude',
    `Lung Health Checks` = 'Exclude',
    `Measurements` = 'Exclude',
    `Other History` = 'Exclude',
    `Diagnostics` = 'Exclude',
    `Invites` = 'Exclude',
    `LDCT` = 'Exclude',
    `Smoking cessation` = 'Exclude'
    )
  )

# update the user
cat(paste('☑️', Sys.time(), 'Data tidied\n', sep = ' '))


# store for future use
saveRDS(
  object = df,
  file = here('data', 'tlhc', 'df_latest_transact.Rds')
)
saveRDS(
  object = df_pivot,
  file = here('data', 'tlhc', 'df_latest_transact_pivot.Rds')
)
saveRDS(
  object = df_pivot_transid,
  file = here('data', 'tlhc', 'df_latest_transact_pivot_transid.Rds')
)


# update the user
cat(paste('☑️', Sys.time(), 'Tables stored as RDS\n', sep = ' '))

# update the user
cat(paste('☑️', Sys.time(), 'Script complete ===================================\n', sep = ' '))