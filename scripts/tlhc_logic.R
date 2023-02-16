#'------------------------------------------------------------------------------
#' SWITCHBOARD
#' 
#' -----------------------------------------------------------------------------

# Libraries -----
library(here)

# Preparation ------------------------------------------------------------------

# Download tables from SQL server (estimated time = 30 mins)
source(here('scripts', 'tlhc_download_sql_data.R'))
rm(list=ls())

# Process the downloaded files (estimated time = 30 seconds)
source(here('scripts', 'tlhc_process_sql_data.R'))
rm(list=ls())

# Calculate metrics (estimated time = 2 mins)
source(here('scripts', 'tlhc_metrics.R'))
rm(list=ls())


# DQ / ad-hoc analysis ---------------------------------------------------------
file.edit(here('scripts', 'tlhc_ad_hoc_analysis.R'))

file.edit(here('scripts', 'tlhc_validation.R'))


# Reporting --------------------------------------------------------------------

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

# Checking submissions ---------------------------------------------------------

# TransactionID generation
file.edit(here('scripts', 'tlhc_latest_submissions.R')) # to edit the date range for 'Include' submissions (if required)
source(here('scripts', 'tlhc_latest_submissions.R'))

# View the results of the transactions
file.edit(here('scripts', 'tlhc_transaction_id.qmd')) # NB need to render this manually
browseURL(here('scripts', 'tlhc_transaction_id.html')) # to view a previously generated html file
