#'------------------------------------------------------------------------------
#' SWITCHBOARD
#' 
#' -----------------------------------------------------------------------------

# Libraries -----
library(here)

# Preparation ------------------------------------------------------------------

# Archive current downloaded files - not always required, but useful if needed
source(here('scripts', 'tlhc_general_functions.R'))
archive_current_data_files()
rm(list=ls())

# Download tables from SQL server (estimated time = 1 hour)
source(here('scripts', 'tlhc_general_functions.R'))
get_southampton_invite_transaction_ids() # list out Southampton's newer transaction IDs
file.edit(here('scripts', 'tlhc_download_sql_data.R')) # to update Southampton's transactionIDs (line #157)
source(here('scripts', 'tlhc_download_sql_data.R'))
rm(list=ls())

# Process the downloaded files (estimated time = 1 mins)
source(here('scripts', 'tlhc_process_sql_data.R'))
rm(list=ls())

# Calculate metrics (estimated time = 4 mins)
source(here('scripts', 'tlhc_metrics.R'))
rm(list=ls())


# DQ / ad-hoc analysis ---------------------------------------------------------
file.edit(here('scripts', 'tlhc_ad_hoc_analysis.R'))

file.edit(here('scripts', 'tlhc_validation.R'))


# Reporting --------------------------------------------------------------------

# southampton manual adjustment for follow-up invites (estimated time = 1 mins)
source(here('scripts', 'tlhc_create_manual_adjustment_southampton.R'))
rm(list=ls())

# output datasheets (estimated time = 1 mins)
file.edit(here('scripts', 'tlhc_datasheet_output.R')) # to edit how many months back to produce the datasheets
source(here('scripts', 'tlhc_datasheet_output.R'))
rm(list=ls())

# transfer datasheets (estimated time = 2 mins)
source(here('scripts', 'tlhc_datasheet_transfer.R'))
rm(list=ls())

# collate from DATA NEW (estimated time = 5 mins)
file.edit(here('scripts', 'tlhc_collate_datasheets.R')) # in case you need to edit the end date (when doing an out-of-sync report run)
source(here('scripts', 'tlhc_collate_datasheets.R'))
rm(list=ls())

# Produce TLHC report (estimated time = 2 mins)
file.edit(here('scripts', 'tlhc_report.R'))
source(here('scripts', 'tlhc_report.R'))
rm(list=ls())

# Produce demographic report (estimated time = 1 minute)
# NB, output needs copying to the demographics report template
source(here('scripts', 'tlhc_demographics.R'))
rm(list=ls())

# Produce TLHC report comparison (estimated time = 2 mins)
# please change the report production date in the script 
file.edit(here('scripts', 'tlhc_compare_reports.R'))
source(here('scripts', 'tlhc_compare_reports.R'))
rm(list=ls())

# Checking submissions ---------------------------------------------------------

# TransactionID generation (estimated time = 1 minute)
file.edit(here('scripts', 'tlhc_latest_submissions.R')) # to edit the date range for 'Include' submissions (if required)
source(here('scripts', 'tlhc_latest_submissions.R'))

# View the results of the transactions v2 - using Shiny app (nb, to stop this process click the 'stop' button in the console)
library(shiny)
runApp(here('scripts', 'transid', 'app.R'))

# View the results of the transactions v1 - using quarto doc
file.edit(here('scripts', 'tlhc_transaction_id.qmd')) # NB need to render this manually
browseURL(here('scripts', 'tlhc_transaction_id.html')) # to view a previously generated html file
