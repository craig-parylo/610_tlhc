# 610 Targeted Lung Health Check Programme
This project contains the R code to produce metric reports as part of the [Targeted Lung Health Check Programme](https://www.england.nhs.uk/contact-us/privacy-notice/how-we-use-your-information/our-services/evaluation-of-the-targeted-lung-health-check-programme/).

To run this code you need to clone the repository in Rstudio.

Then run <code>renv::install()</code> to set up your project environment with all the required dependencies to run the code

## Typical workflow for The Strategy Unit
The Strategy Unit uses this project to download data submissions from Data Central (a SQL database) to local .Rds files and process them before calculating performance metrics. This workflow involves three primary steps:

### 1. Download data
<code>scripts/tlhc_download_sql.R</code>

Use this to download data from the SQL server hosted by [Midlands and Lancashire CSU](https://www.midlandsandlancashirecsu.nhs.uk/). This data comprises project submissions to the Data Central DSCRO platform.

*NB, this database is not accessible to staff outside the CSU*. 

For NHS organisations looking to run these scripts on your own data the expected output from this process is a set of eight .Rds files within the 'data' folder - each file corresponds to one of the .csv files uploaded as part of the monthly submissions as specified in the [patient level dataset](https://future.nhs.uk/canc/view?objectID=33919568), (NB, access to the dataset requires a NHS Futures account).

* tbTLHCTLHC_Demographics.Rds,
* tbTLHCTLHC_LungHealthCheck.Rds,
* tbTLHCTLHC_Measurements.Rds,
* tbTLHCTLHC_OtherHistory.Rds,
* tbTLHCTLHC_Pathway_Diagnostics.Rds,
* tbTLHCTLHC_Pathway_Invite.Rds,
* tbTLHCTLHC_Pathway_LDCT.Rds,
* tbTLHCTLHC_Pathway_SmokingCessation.Rds

### 2. Process the data
<code>scripts/tlhc_process_sql_data.R</code>

Prepares the data in each table for analysis by converting text dates (from the CSV upload) to recognised date formats, produces standardised values for key fields where data provided doesn't match with the above dataset standards.

### 3. Calculate metric performance
<code>scripts/tlhc_metrics.R</code> (relies on <code>scripts/tlhc_metric_functions.R</code>).

Applies business processing rules to the datasets and calculates metric performance, typically at monthly intervals for each project.
