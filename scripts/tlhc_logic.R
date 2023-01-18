#'------------------------------------------------------------------------------
#' SWITCHBOARD
#' 
#' -----------------------------------------------------------------------------

# Libraries -----
library(here)

# Preparation ------------------------------------------------------------------

# Download tables from SQL server (estimated time = 17 mins)
source(here('scripts', 'tlhc_scripts', 'tlhc_download_sql_data.R'))
rm(list=ls())

# Process the downloaded files (estimated time = 30 seconds)
source(here('scripts', 'tlhc_scripts', 'tlhc_process_sql_data.R'))
rm(list=ls())

# Calculate metrics (estimated time = 2 mins)
source(here('scripts', 'tlhc_scripts', 'tlhc_metrics.R'))
rm(list=ls())


# DQ / ad-hoc analysis ---------------------------------------------------------
file.edit(here('scripts', 'tlhc_scripts', 'tlhc_ad_hoc_analysis.R'))

file.edit(here('scripts', 'tlhc_scripts', 'tlhc_validation.R'))


# Reporting --------------------------------------------------------------------

# output datasheets
source(here('scripts', 'tlhc_scripts', 'tlhc_datasheet_output.R'))
rm(list=ls())

# transfer datasheets

# collate from DATA NEW

# Produce TLHC report


# Produce demographic report (needs copying to the template)
source(here('scripts', 'tlhc_scripts', 'tlhc_demographics.R'))
rm(list=ls())

# Produce flat file output