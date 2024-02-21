#'------------------------------------------------------------------------------
#' TLHC METRIC CALCULATION FUNCTIONS
#' 
#' Provides a range of functions to facilitate the staging and calculation of 
#' TLHC metric information. 
#' 
#' These functions require downloaded files which have been processed.
#' 
#' They fall into three categories of function:
#' 1. loading base data,
#' 2. prepare sets of records that meet metric criteria,
#' 3. calculate aggregated metric performance
#'------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(tidyverse)     # tidy data processing
library(here)          # localise file references
library(dtplyr)        # faster table processing
library(lubridate)     # date functions
library(zoo)           # date functions
library(progressr)     # progress bar
library(tictoc)        # timing monitor
library(fuzzyjoin)     # joining on fuzzy word matches
source(here('scripts', 'func_name_projects.R')) # naming projects

# Notify user 

# UDF --------------------------------------------------------------------------
## load data -------------------------------------------------------------------
# These functions take care of loading and preparing data ready for metric
# calculations

#' Load Demographics
#' 
#' Load the demographics table to a variable
#'
#' @return df Demographics
load_df_demo <- function() {
  
  p('loading demographics')
  
  return(
    df <- readRDS(here('data', 'tlhc', 'calc_tbTLHCTLHC_Demographics.Rds')) |> 
      ungroup()
  )
}

#' Load Demographics for joining to other tables
#' 
#' Load the demographics table and select a subset of fields for joining to
#' other tables.
#'
#' @return df Demographics (selected subset)
load_df_demo_join <- function() {
  
  p('loading demographics subset')
  
  # load the demographics file if not already loaded
  if(!exists('df_demo')){df_demo <- load_df_demo()}
  
  # get a subset of demographics for joining to other tables
  df_demo_join <- df_demo |> 
    select(
      ParticipantID,                       # used for joining to data
      ccg_code = CCG_Code,                 # used to work out the project name
      calc_age_tlhc_valid,                 # used to confirm valid for tlhc project
      calc_age_group_report,               # demographic
      calc_age_group_report_v2,            # demographic
      calc_age_group_ipsos,                # demographic
      calc_sex,                            # demographic
      calc_ethnic_group,                   # demographic
      calc_lsoa_imd_decile,                # demographic
      calc_lsoa_rurality_group_category,   # demographic
    )
}

#' Load LHC invitations
#' 
#' Load the invitations table, add in some demographic information and calculate
#' the name of the project.
#' 
#' @return df Invitations
load_df_invites <- function() {
  
  p('loading invites')
  
  return(
    df <- readRDS(here('data', 'tlhc', 'calc_tbTLHCTLHC_Pathway_Invite.Rds')) |> 
      ungroup() |> 
      calculate_project_name()
  )
}

#' Load Lung Health Check data
#' 
#' Load the LHC table, add in some demographic information, calculate the
#' name of the project and identify participants identified as eligible from the
#' invites table.
#' 
#' @return df Lung Health Checks
load_df_lhc <- function() {
  
  p('loading lhc')
  
  return(
    df <- readRDS(here('data', 'tlhc', 'calc_tbTLHCTLHC_LungHealthCheck.Rds')) |> 
      ungroup() |> 
      calculate_project_name() |> 
      identify_eligible_participants()
  )
}

#' Load Measurements data
#' 
#' Load the Measurements table, add in some demographic information, calculate
#' the name of the project and identify participants identified as eligible from
#' the invites table
#' 
#' @return df Measurements
load_df_measurements <- function() {
  
  p('loading measurements')
  
  return(
    df <- readRDS(here('data', 'tlhc', 'calc_tbTLHCTLHC_Measurements.Rds')) |> 
      ungroup() |> 
      calculate_project_name() |> 
      identify_eligible_participants() |> 
      identify_lhc_yearmon()
  )
}

#' Load Low Dose CT (LDCT) data
#' 
#' Load the LDCT table, add in some demographic information and calculate the
#' name of the project.
#' 
#' @return df Lung Health Checks
load_df_ldct <- function() {
  
  p('loading ldct')
  
  return(
    df <- readRDS(here('data', 'tlhc', 'calc_tbTLHCTLHC_Pathway_LDCT.Rds')) |> 
      ungroup() |> 
      calculate_project_name() |> 
      identify_eligible_participants() |>
      identify_participant_risk_groups() |> 
      identify_lhc_yearmon()
  )
}

#' Load Diagnostics data
#' 
#' Load the Diagnostics table, add in some demographic information and 
#' calculate the name of the project.
#' 
#' @return df Diagnostics
load_df_diagnostics <- function() {
  
  p('loading diagnostics')
  
  return(
    df <- readRDS(here('data', 'tlhc', 'calc_tbTLHCTLHC_Pathway_Diagnostics.Rds')) |>
      ungroup() |> 
      calculate_project_name() |> 
      identify_eligible_participants()
  )
}

#' Load Other History data
#' 
#' Load the Other History table, add in some demographic information and 
#' calculate the name of the project.
#' 
#' @return df Diagnostics
load_df_other <- function() {
  
  p('loading history')
  
  return(
    df <- readRDS(here('data', 'tlhc', 'calc_tbTLHCTLHC_OtherHistory.Rds')) |> 
      ungroup() |> 
      calculate_project_name()
  )
}

#' Load Smoking Cessation data
#' 
#' Load the Smoking Cessation table, add in some demographic information and 
#' calculate the name of the project.
#' 
#' @return df Smoking Cessation
load_df_smoking <- function() {
  
  p('loading diagnostics')
  
  return(
    df <- readRDS(here('data', 'tlhc', 'calc_tbTLHCTLHC_SmokingCessation.Rds')) |> 
      ungroup() |> 
      calculate_project_name() |> 
      identify_eligible_participants()
  )
}

#' Load Lung Cancer data from aggregate
#' 
#' Load the Lung Cancer dataset manually collated from aggregate submissions.
#' 
#' @return df lung cancers
load_df_lung_cancers_from_aggregate <- function() {
  
  # NB, relies on an environment variable 'base_365' to supply the path to the 
  # MLCSU 365 server.
  df_cancer_filepath <- file.path(Sys.getenv('base_365'), 'Monthly MI reporting', 'Data processing procedure', 'TLHC lung cancer diagnosis.xlsx')
  df_cancer <- read_excel(
    path = df_cancer_filepath,
    sheet = 'AGGREGATE DATA',
    col_names = TRUE,
    col_types = 'text'
  ) |> 
    # convert to handy names
    select(
      project = `Place`,
      month = `Date`,
      metric_name = `Metric`,
      metric_id = `MetricID`,
      numerator = `Number`
    ) |> 
    # data type conversions
    mutate(
      month = as.yearmon(parse_date_time(as.character(month), '%y%m')),
      numerator = as.numeric(numerator)
    ) |> 
    # exclude projects no longer wanted (i.e. have been replaced by others)
    filter(
      !project %in% c(
        'Liverpool',
        'Halton',
        'Knowsley',
        'Blackburn with Darwen',
        'Blackpool'
      )
    )
  # NOTE - will get error messages from above as tries to coerce NA data to numeric
  # Reason: we have gaps in our lc data submissions so no data for some projects-months.
  
  return(df_cancer)
}

#' Load accepted other incidental findings
#' 
#' Load an accepted list of other incidental findings (as defined from the MDS)
#' 
#' @return df Tibble of accepted incidental findings
load_other_incidental_findings <- function() {
  
  # load our official list
  df_oif <- tibble(
    calc_other_incidental_finding = c(
      'abdominal aortic aneurysm',
      'adrenal lesions',
      'aortic valve calcification',
      'bone abnormalities',
      'consolidation',
      'coronary calcification',
      'fractures with no trauma history',
      'liver or splenic lesions',
      'mediastinal mass',
      'osteoporosis',
      'other',
      'pleural effusions/thickening',
      'renal lesions',
      'suspicious breast lesion',
      'thoracic aortic aneurysm',
      'thyroid lesion',
      'tuberculosis',
      'other cancers'
    )
  )
  
  return(df_oif)
}

#' Load accepted pulmonary incidental findings
#' 
#' Load an accepted list of pulmonary incidental findings (as defined from the MDS)
#' 
#' @return df Tibble of accepted incidental findings
load_pulmonary_incidental_findings <- function() {
  
  # load our official list
  df_pif <- tibble(
    calc_pulmonary_incidental_finding = c(
      'bronchiectasis',
      'respiratory bronchiolitis',
      'interstitial lung abnormalities (ilas)',
      'ila' # NB added as the SQL metric uses multiple match types to get figures up
    )
  )
  
  return(df_pif)
}

#' Load cancer tumour data
#' 
#' Load an df of cancer tumour
#' 
#' @return df Tibble of cancer tumour data
load_df_cancer_tumour <- function() {
  
  p('loading cancer tumour')
  
  return(
    df <- readRDS(here('data', 'tlhc', 'NCRAS_Cancer_Tumour_Data_TLHC.Rds')) |> 
      ungroup()
  )
}

#' Load cancer pathway data
#' 
#' Load an df of cancer pathways
#' 
#' @return df Tibble of cancer pathway data
load_df_cancer_pathway <- function() {
  
  p('loading cancer pathways')
  
  return(
    df <- readRDS(here('data', 'tlhc', 'NCRAS_National_Cancer_Pathway_Data_TLHC.Rds')) |> 
      ungroup()
  )
}

#' Load cancer rapid registration data
#' 
#' Load an df of cancer rapid registration
#' 
#' @return df Tibble of cancer rapid registration data
load_df_cancer_reg <- function() {
  
  p('loading cancer rapid registration')
  
  return(
    df <- readRDS(here('data', 'tlhc', 'NCRAS_National_Cancer_Rapid_Registration_TLHC.Rds')) |> 
      ungroup()
  )
}


## helper functions ------------------------------------------------------------
# These functions process data to add additional details required for
# metric calculations


#' Calculate the name of projects for a given dataframe
#'
#' @param df Tibble of data to add project name to
#'
#' @return df Tibble with project name added
calculate_project_name <- function(df) {
  
  # get demographic details
  if(!exists('df_demo_join')){df_demo_join <- load_df_demo_join()}
  
  # add demographic details to the df
  df <- left_join(
    x = df,
    y = df_demo_join,
    by = "ParticipantID"
  )
  
  # add the place_code field required by the name function
  # NB, using the UDF 'name_projects()'
  df <- df |> 
    mutate(place_code = calc_submitting_organisation_code) |> 
    name_projects()
  
  # return the result
  return(df)
}

#' Identify eligible participants
#' 
#' For participants of a given df identify their eligibility for lung health 
#' checks, i.e. they have not been removed from the GP list because of their 
#' age, smoking status or other requirement.
#' 
#' @param df Tibble of data to add eligible status to
#' 
#' @return df with participant's eligibility added
identify_eligible_participants <- function(df) {
  
  # get invite details
  if(!exists('df_invites')){df_invites <- load_df_invites()}
  
  # create a lookup between participant id and eligibility and initial invite date (for metric 10)
  df_eligible <- df_invites |> 
    select(ParticipantID, calc_eligible, calc_first_letter_date) |> 
    unique()
  
  # add eligiblity to the supplied df
  df <- left_join(
    x = df,
    y = df_eligible,
    by = "ParticipantID"
  )
  
  # return the result
  return(df)
}

#' Identify risk group of participants based on their lhc screening scores
#' 
#' For participants of a given df identify their risk status based on either of
#' the two risk scores, (PLCOm2012 or LLPv2).
#' 
#' @param df Tibble of data to add risk status to
#' 
#' @return df with participant's risk groups added
identify_participant_risk_groups <- function(df) {
  
  # get measurement details
  if(!exists('df_meas')){df_meas <- load_df_measurements()}
  
  # create a lookup between participant id and risk scores
  df_risk <- df_meas |> 
    select(ParticipantID, calc_PLCOm2012_risk_group, calc_LLPv2_risk_group) |> 
    unique()
  
  # add eligibility to the supplied df
  df <- left_join(
    x = df,
    y = df_risk,
    by = "ParticipantID"
  )
  
  # return the result
  return(df)
}

#' Identify LHC yeamonth for measurement table
#' 
#' For participants of a given measurement df identify the yearmonth the LHC took
#' place. This is the time dimension against which the measurement-based metrics
#' (LDCT referral) are reported against.
#' 
#' @param df Tibble of measurement data to add lhc yearmonth to
#' 
#' @return df with lhc yearmonth added
identify_lhc_yearmon <- function(df) {
  
  # # get invite details
  # if(!exists('df_lhc')){df_lhc <- load_df_lhc()}
  # 
  # # create a lookup between participant id and lhc date
  # df_lhc_yearmon <- df_lhc |> 
  #   # limit the dataframe to valid, attended LHC
  #   filter(
  #     calc_valid_transactionid == 'Valid', # valid transactions
  #     calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
  #     calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
  #     !is.na(calc_lhc_date_yearmon), # exclude records without a LHC date
  #     calc_lhc_attendance_category == 'Attended', # participant is recorded as attending
  #   ) |>
  #   # take the first attended lhc for a given participant
  #   group_by(ParticipantID) |> 
  #   slice_min(calc_lhc_date) |> 
  #   ungroup() |> 
  #   # simplify to required fields
  #   select(ParticipantID, calc_lhc_date_yearmon, calc_lhc_date) |> 
  #   unique()
  
  # get lhc details
  if(!exists('df_lhc_first_attended')){df_lhc_first_attended <- get_first_attended_lhc_per_participant()}
  
  # create a lookup between participant and lhc date
  df_lhc_yearmon <- df_lhc_first_attended |> 
    # simplify to required fields
    select(ParticipantID, calc_lhc_date_yearmon, calc_lhc_date) |>
    unique()
  
  # add yearmonth to the supplied df
  df <- left_join(
    x = df,
    y = df_lhc_yearmon,
    by = "ParticipantID"
  )
  
  # return the result
  return(df)
}


#' Standard aggregation of single metric performance
#' 
#' Calculates the number of unique participants per month within a supplied 
#' dataframe.
#' 
#' @param df Tibble of data to aggregate
#' 
#' @return df of aggregated metric performance
aggregate_metric_standard <- function(df) {
  
  #p(paste0('aggregating metric ', first(df$metric_id)))
  
  return(
    # summarise per month
    df |> 
      group_by(
        project, 
        month,
        metric_id,
        metric_name
      ) |> 
      summarise(numerator = n_distinct(ParticipantID), .groups = 'drop') 
  )
}

#' Standard aggregation of single metric performance - median times
#' 
#' Calculates the median number of days for a given metric within a supplied
#' dataframe
#'  
#' @param df Tibble of data to aggregate
#' @param df String name of the field to aggregate
#' 
#' @return df of aggregated metric performance
aggregate_metric_standard_median <- function(df, str_median_field) {
  
  median_field <- enquo(str_median_field)
  
  return(
    # summarise per month
    df |> 
      group_by(
        project, 
        month,
        metric_id,
        metric_name
      ) |> 
      summarise(numerator = median(as.numeric(!!median_field), na.rm = T), .groups = 'drop') 
  )
}

#' Extract data from Other_Incidental_Findings
#' 
#' This process identifies incidental findings from the field [Other_Incidental_Findings],
#' which is a single, open text field into which projects are asked to submit findings
#' in a simple list. 
#' 
#' This process splits the field into a list of components and then unnests
#' these to a row each (elongating the table) then fuzzy joining with an accepted
#' list of findings from the MDS, with the closest fuzzy match selected per 
#' incidental finding.
#' 
#' The result is an elongated table with incidental findings on their own row
#' (so ldct details are duplicated too), a closest matched finding and a measure 
#' for how close a match it is on a 0 to 1 scale.
#' 
#' @return df LCDT data elongated with each incidental finding on a new row
extract_other_incidental_data <- function() {
  
  # get ldct details
  if(!exists('df_ldct')){df_lhc <- load_df_ldct()}
  
  # get a list of accepted other incidental findings
  if(!exists('df_incid_other')){df_incid_other <- load_other_incidental_findings()}
  
  # split ldct other incidental findings by separator (dash or full stop)
  df_ldct_incid_other <- df_ldct |> 
    filter(!is.na(Other_Incidental_Findings)) |> # only work with records containing data
    # split ldct other incidental findings by separator (dash or full stop or capitalisation in 'lowerUpper')
    mutate(
      calc_other_incidental_finding = str_split(
      string = Other_Incidental_Findings,
      pattern = '-| - |\\. |(?<=[[:lower:]])(?=[[:upper:]][[:lower:]])'
      )
    ) |> 
    # stretch the data so each incidental finding is on its own row
    unnest(calc_other_incidental_finding) |> 
    mutate(calc_other_incidental_finding = tolower(calc_other_incidental_finding)) |> 
    filter(!is.na(calc_other_incidental_finding))
  
  # fuzzy match the expanded other incidentals to the official list
  df_ldct_incid_other <- stringdist_join(
    x = df_ldct_incid_other,
    y = df_incid_other,
    by = 'calc_other_incidental_finding', # the name of the field we are joining on
    method = 'jw', # use the jw distance metric
    max_dist = 99, # allow all matches (will set filters per metric later)
    distance_col = 'dist' # call the column 'dist' for the fuzziness of the match
  ) |> 
    # keep only the best match per incidental finding
    group_by(calc_other_incidental_finding.x) |> 
    slice_min(dist, n = 1)
  
  # NB, we now have an additional couple of fields:
  # calc_other_incidental_finding.y (the best matched result from the official list)
  # dist (the difference between the original calc_other_incidental_finding and the official one)
  # where 0 = perfect match, 1 = completely different
  
  # return the result
  return(df_ldct_incid_other)
}

#' Get a dataframe for metric 7 - participants who had an other incidental finding 
#' for a given incidental finding and distance value
#' 
#' Return a record-level tibble in support of calculating a metric regarding 
#' incidental finding.
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @param str_finding String name of the incidental finding to return
#' @param dec_list Decimal value (0 to 1) for the distance between matched incidental finding (fuzzy matching)
#' @param str_metric_id String identifying the metric id
#' @param str_metric_name String identifying the metric name
#' 
#' @return Tibble
get_other_incidental_data_for_item <- function(str_finding, 
                                               dec_dist = 0.2,
                                               str_metric_id,
                                               str_metric_name) {
  
  # load the data if not already loaded
  if(!exists('df_ldct_incid_other')){df_ldct_incid_other <- extract_other_incidental_data()}
  
  # prepare parameters
  dec_dist <- as.numeric(dec_dist)
  
  # processing
  df <- df_ldct_incid_other |> 
    # add fields identifying the metric
    mutate(
      metric_id = str_metric_id,
      metric_name = str_metric_name,
      month = calc_ldct_date_corrected_yearmon,
    ) |> 
    # limit the dataframe for valid metric
    filter(
      calc_valid_transactionid == 'Valid', # valid transactions
      calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_ldct_date_corrected_yearmon), # exclude records without a LDCT date
      #(calc_PLCOm2012_risk_group == 'High risk' | calc_LLPv2_risk_group == 'High risk'), # identified as high risk using either score in LHC
      calc_ldct_outcome_corrected_groups == 'LDCT performed', # we have confirmation the scan took place (i.e. exclude future booked)
      !is.na(calc_other_incidental_finding.x), # we have an extracted incidental finding
      dist <= dec_dist, # the matched incidental finding is within the specified distance
      calc_other_incidental_finding.y == str_finding # the finding to return
    ) 
  
  return(df)
}

#' Extract data from Pulmonary_Incidental_Findings
#' 
#' This process identifies incidental findings from the field [Pulmonary_Incidental_Findings],
#' which is a single, open text field into which projects are asked to submit findings
#' in a simple list. 
#' 
#' This process splits the field into a list of components and then unnests
#' these to a row each (elongating the table) then fuzzy joining with an accepted
#' list of findings from the MDS, with the closest fuzzy match selected per 
#' incidental finding.
#' 
#' The result is an elongated table with incidental findings on their own row
#' (so ldct details are duplicated too), a closest matched finding and a measure 
#' for how close a match it is on a 0 to 1 scale.
#' 
#' @return df LCDT data elongated with each incidental finding on a new row
extract_pulmonary_incidental_data <- function() {
  
  # get ldct details
  if(!exists('df_ldct')){df_lhc <- load_df_ldct()}
  
  # get a list of accepted pulmonary incidental findings
  if(!exists('df_incid_pulm')){df_incid_pulm <- load_pulmonary_incidental_findings()}

  # split ldct other incidental findings by separator (dash or full stop)
  df_ldct_incid_pulmonary <- df_ldct |> 
    filter(!is.na(Pulmonary_Incidental_Findings)) |> # only work with records containing data
    # split ldct other incidental findings by separator (dash or full stop or capitalisation in 'lowerUpper')
    mutate(
      calc_pulmonary_incidental_finding = str_split(
        string = Pulmonary_Incidental_Findings,
        pattern = '-| - |\\. |(?<=[[:lower:]])(?=[[:upper:]][[:lower:]])'
      )
    ) |> 
    # stretch the data so each incidental finding is on its own row
    unnest(calc_pulmonary_incidental_finding) |> 
    mutate(calc_pulmonary_incidental_finding = tolower(calc_pulmonary_incidental_finding)) |> 
    filter(!is.na(calc_pulmonary_incidental_finding))
  
  # fuzzy match the expanded incidentals to the official list
  df_ldct_incid_pulmonary <- stringdist_join(
    x = df_ldct_incid_pulmonary,
    y = df_incid_pulm,
    by = 'calc_pulmonary_incidental_finding', # the name of the field we are joining on
    method = 'jw', # use the jw distance metric
    max_dist = 99, # allow all matches (will set filters per metric later)
    distance_col = 'dist' # call the column 'dist' for the fuzziness of the match
  ) |> 
    # keep only the best match per incidental finding
    group_by(calc_pulmonary_incidental_finding.x) |> 
    slice_min(dist, n = 1)
  
  # NB, we now have an additional couple of fields:
  # calc_pulmonary_incidental_finding.y (the best matched result from the official list)
  # dist (the difference between the original calc_pulmonary_incidental_finding and the official one)
  # where 0 = perfect match, 1 = completely different
  
  # return the result
  return(df_ldct_incid_pulmonary)
}

#' Get a dataframe for metric 7 - participants who had a pulmonary incidental finding 
#' for a given incidental finding and distance value
#' 
#' Return a record-level tibble in support of calculating a metric regarding 
#' incidental finding.
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @param str_finding String name of the incidental finding to return
#' @param dec_list Decimal value (0 to 1) for the distance between matched incidental finding (fuzzy matching)
#' @param str_metric_id String identifying the metric id
#' @param str_metric_name String identifying the metric name
#' 
#' @return Tibble
get_pulmonary_incidental_data_for_item <- function(str_finding, 
                                                   dec_dist = 0.2,
                                                   str_metric_id,
                                                   str_metric_name) {
  
  # load the data if not already loaded
  if(!exists('df_ldct_incid_pulm')){df_ldct_incid_pulm <- extract_pulmonary_incidental_data()}
  
  # prepare parameters
  dec_dist <- as.numeric(dec_dist)
  
  # processing
  df <- df_ldct_incid_pulm |> 
    # add fields identifying the metric
    mutate(
      metric_id = str_metric_id,
      metric_name = str_metric_name,
      month = calc_ldct_date_corrected_yearmon,
    ) |> 
    # limit the dataframe for valid metric
    filter(
      calc_valid_transactionid == 'Valid', # valid transactions
      calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_ldct_date_corrected_yearmon), # exclude records without a LDCT date
      #(calc_PLCOm2012_risk_group == 'High risk' | calc_LLPv2_risk_group == 'High risk'), # identified as high risk using either score in LHC
      calc_ldct_outcome_corrected_groups == 'LDCT performed', # we have confirmation the scan took place (i.e. exclude future booked)
      !is.na(calc_pulmonary_incidental_finding.x), # we have an extracted incidental finding
      dist <= dec_dist, # the matched incidental finding is within the specified distance
      calc_pulmonary_incidental_finding.y == str_finding # the finding to return
    ) 
  
  return(df)
}

#' Get a dataframe for metric 10 - median time from invite to lhc
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' median time (days) between initial invite and the lhc date
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_first_attended_lhc_per_participant <- function() {
  
  # load the data if not already loaded
  if(!exists('df_lhc')){df_lhc <- load_df_lhc()}
  
  return(
    df_lhc |> 
      lazy_dt() |> 
      # limit the dataframe for valid metric
      filter(
        calc_valid_transactionid == 'Valid', # valid transactions
        calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
        calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
        !is.na(calc_lhc_date_yearmon), # exclude records without a LHC date
        calc_lhc_attendance_category == 'Attended', # participant is recorded as attending
        #!is.na(calc_first_letter_date) # exclude records without an invite date
      ) |> 
      # rank dates, so that we can prefer those that occur within the TLHC programme lifespan
      mutate(
        calc_rank_programme_dates = if_else(
          calc_lhc_date > as.Date('2019-03-31'), 0, 1
        )
      ) |> 
      # take the first attended lhc for a given participant (but prefer dates within TLHC programme lifespan)
      group_by(ParticipantID) |> 
      arrange(calc_rank_programme_dates, calc_lhc_date) |> 
      slice(1) |> 
      ungroup() |> 
      as_tibble()
  )
}


## load metric df --------------------------------------------------------------

### based on invites ---------------------------------------------------

# These functions load record-level data in support for the calculation of metrics.
# They serve several purposes:
# 1. a convenience function as a first step to calculate aggregate metrics, 
# 2. a way of providing record-level data as evidence for metric performance,
# 3. providing a convenient platform for non-standard aggregate metric 
#    calculation - e.g. calculating metrics by different groups (age, gender, 
#    ethnicity, etc).

#' Get a dataframe for metric 1a - first invites
#' 
#' Return a record-level tibble in support of calculating the metric for first
#' invites.
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_1a_invites_first <- function() {
  
  p('df for metric 1a')
  
  # load the data if not already loaded
  if(!exists('df_invites')){df_invites <- load_df_invites()}
  
  return(
    df_invites |> 
      # add fields identifying the metric
      mutate(
        metric_id = '1a',
        metric_name = 'Number of people invited to a Targeted Lung Health Check (First Invites)',
        month = calc_first_letter_date_yearmonth,
      ) |> 
      # limit the dataframe for valid metric
      filter(
        calc_valid_transactionid == 'Valid', # valid transactions
        calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
        calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
        !is.na(calc_first_letter_date_yearmonth)
      )
  )
}

#' Get a dataframe for metric 1c - follow-up invites
#' 
#' Return a record-level tibble in support of calculating the metric for follow-
#' up invites.
#' As this metric depends on multiple columns as the time dimension, the data is 
#' pivoted so each participant can have multiple entries per the month dimension.
#' Additional processing to filter for non-null months.
#' 
#' @return Tibble
get_df_metric_1c_invites_followup <- function() {
  
  p('df for metric 1c')
  
  # load the data if not already loaded
  if(!exists('df_invites')){df_invites <- load_df_invites()}
  
  # NB, this metric is based on all invites other than first_letter, which means the same
  # patient can be re-invited in multiple months.
  # This requires a pivot-longer table to put the follow-up contacts on the same
  # month dimension
  df <- df_invites |> 
    # add fields identifying the metric
    mutate(
      metric_id = '1c',
      metric_name = 'Number of people invited to a Targeted Lung Health Check (Follow-up Invites)',
    ) |> 
    # limit the dataframe for valid metric
    filter(
      calc_valid_transactionid == 'Valid', # valid transactions
      calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
    ) |> 
    # put our date fields into the same dimension (NB, this increases row count)
    pivot_longer(
      cols = c(calc_second_letter_date_yearmonth, calc_follow_up_call_date_yearmonth),
      names_to = 'followup_type',
      values_to = 'month'
    ) |> 
    # remove rows without a month to report against
    filter(!is.na(month))
  
  # return the dataframe
  return(df)
}

#' Get a dataframe for metric 2 - accepted invites
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' accepted invites.
#' The report month is assumed to be the month of the latest date based on the 
#' following fields:
#' calc_contact_date, 
#' calc_follow_up_call_date, 
#' calc_second_letter_date, 
#' calc_first_letter_date
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_2_invites_accepted <- function() {
  
  p('df for metric 2')
  
  # load the data if not already loaded
  if(!exists('df_invites')){df_invites <- load_df_invites()}
  
  return(
    df_invites |> 
      # add fields identifying the metric
      mutate(
        metric_id = '2',
        metric_name = 'Number of patients who have accepted a Targeted Lung Health Check invitation',
        month = calc_invite_outcome_date_yearmon,
      ) |> 
      # limit the dataframe for valid metric
      filter(
        calc_valid_transactionid == 'Valid', # valid transactions
        calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
        calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
        calc_invite_accepted == 'Accepted', # confirmation the invite was accepted
        !is.na(calc_invite_outcome_date_yearmon)
      )
  )
}

### based on lhc -------------------------------------------------------

#' Get a dataframe for metric 3a - face-to-face lung health checks
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' lung health checks performed face-to-face.
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_3a_attend_f2f <- function() {
  
  p('df for metric 3a')
  
  # load the data if not already loaded - this is a helper function that has the metric logic included
  if(!exists('df_lhc_first_attended')){df_lhc_first_attended <- get_first_attended_lhc_per_participant()}
  
  return(
    df_lhc_first_attended |> 
      # add fields identifying the metric
      mutate(
        metric_id = '3a',
        metric_name = 'Number of patients who attended a face-to-face Lung Health Check',
        month = calc_lhc_date_yearmon,
      ) |> 
      # limit to just F2F contacts
      filter(calc_lhc_delivery_method_category == 'F2F') # the method of delivery is face-to-face
  )
}

#' Get a dataframe for metric 3b - telephone lung health checks
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' lung health checks performed on the telephone (or virtual).
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_3b_attend_tel <- function() {
  
  p('df for metric 3b')
  
  # load the data if not already loaded - this is a helper function that has the metric logic included
  if(!exists('df_lhc_first_attended')){df_lhc_first_attended <- get_first_attended_lhc_per_participant()}
    
  return(
    df_lhc_first_attended |> 
      # add fields identifying the metric
      mutate(
        metric_id = '3b',
        metric_name = 'Number of patients who attended a telephone Lung Health Check',
        month = calc_lhc_date_yearmon,
      ) |> 
      # limit to just virtual contacts
      filter(calc_lhc_delivery_method_category == 'Virtual') # the method of delivery is face-to-face
  )
}

#' Get a dataframe for metric 3c - face-to-face lung health check non-attendances
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' lung health checks intended to be face-to-face but the patient didn't attend.
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_3c_dna_f2f <- function() {
  
  p('df for metric 3c')
  
  # load the data if not already loaded
  if(!exists('df_lhc')){df_lhc <- load_df_lhc()}
  
  return(
    df_lhc |> 
      # add fields identifying the metric
      mutate(
        metric_id = '3c',
        metric_name = 'Number of patients who did not attend their face-to-face Lung Health Check',
        month = calc_lhc_date_yearmon,
      ) |> 
      # limit the dataframe for valid metric
      filter(
        calc_valid_transactionid == 'Valid', # valid transactions
        calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
        calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
        !is.na(calc_lhc_date_yearmon), # exclude records without a LHC date
        calc_lhc_attendance_category == 'DNA', # participant is recorded as not attending
        calc_lhc_delivery_method_category == 'F2F' # the method of delivery is face-to-face
      )
  )
}

#' Get a dataframe for metric 3d - telephone/virtual lung health check non-attendances
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' lung health checks intended to be telephone / virtual but the patient didn't attend.
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_3d_dna_tel <- function() {
  
  p('df for metric 3d')
  
  # load the data if not already loaded
  if(!exists('df_lhc')){df_lhc <- load_df_lhc()}
  
  return(
    df_lhc |> 
      # add fields identifying the metric
      mutate(
        metric_id = '3d',
        metric_name = 'Number of patients who did not attend their telephone Lung Health Check',
        month = calc_lhc_date_yearmon,
      ) |> 
      # limit the dataframe for valid metric
      filter(
        calc_valid_transactionid == 'Valid', # valid transactions
        calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
        calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
        !is.na(calc_lhc_date_yearmon), # exclude records without a LHC date
        calc_lhc_attendance_category == 'DNA', # participant is recorded as not attending
        calc_lhc_delivery_method_category == 'Virtual' # the method of delivery is not face-to-face
      )
  )
}

### based on measurements ----------------------------------------------

#' Get a dataframe for metric 4a - participants referred for a low dose ct
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' lung health checks resulting in the patient being considered high risk and 
#' therefore eligible for an ldct.
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_4a_ldct_referral <- function() {
  
  p('df for metric 4')
  
  # load the data if not already loaded
  if(!exists('df_meas')){df_meas <- load_df_measurements()}
  
  return(
    df_meas |> 
      # add fields identifying the metric
      mutate(
        metric_id = '4a',
        metric_name = 'Number of participants who are referred for a Low Dose CT',
        month = calc_lhc_date_yearmon,
      ) |> 
      # limit the dataframe for valid metric
      filter(
        calc_valid_transactionid == 'Valid', # valid transactions
        calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
        calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
        !is.na(calc_lhc_date_yearmon), # exclude records without a LHC date
        (calc_PLCOm2012_risk_group == 'High risk' | calc_LLPv2_risk_group == 'High risk') # identified as high risk using either score
      )
  )
}

#' Get a dataframe for metric 4b - participants with high risk score but ineligible for a low dose ct
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' lung health checks resulting in the patient being considered high risk and 
#' but identified as ineligible for a scan.
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_4b_ldct_ineligible <- function() {
  
  p('df for metric 4b')
  
  # load the data if not already loaded
  if(!exists('df_meas')){df_meas <- load_df_measurements()}
  
  return(
    df_meas |> 
      # add fields identifying the metric
      mutate(
        metric_id = '4b',
        metric_name = 'Number of participants who triggered a risk score for referral but were ineligible for an initial LDCT scan',
        month = calc_lhc_date_yearmon,
      ) |> 
      # limit the dataframe for valid metric
      filter(
        calc_valid_transactionid == 'Valid', # valid transactions
        calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
        calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
        !is.na(calc_lhc_date_yearmon), # exclude records without a LHC date
        (calc_PLCOm2012_risk_group == 'High risk' | calc_LLPv2_risk_group == 'High risk'), # identified as high risk using either score
        !is.na(calc_exclusion_criteria) # include records where an exclusion criteria was recorded
      )
  )
}

### based on ldct --------------------------------------------------------------

#' Get a dataframe for metric 5a - participants who had an initial Low Dose CT scan performed
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' initial low dose CT scans performed
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_5a_ldct_initial <- function() {
  
  p('df for metric 5a')
  
  # load the data if not already loaded
  if(!exists('df_ldct')){df_ldct <- load_df_ldct()}
  
  # processing
  df_ldct <- df_ldct |> 
    # add fields identifying the metric
    mutate(
      metric_id = '5a',
      metric_name = 'Number of participants who had an initial Low Dose CT scan performed',
      month = calc_ldct_date_corrected_yearmon,
    ) |> 
    # limit the dataframe for valid metric
    filter(
      calc_valid_transactionid == 'Valid', # valid transactions
     # calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_ldct_date_corrected_yearmon), # exclude records without a LDCT date
     # (calc_PLCOm2012_risk_group == 'High risk' | calc_LLPv2_risk_group == 'High risk'), # identified as high risk using either score in LHC
      calc_ldct_outcome_corrected_groups == 'LDCT performed', # we have confirmation the scan took place (i.e. exclude future booked)
      calc_ldct_date_corrected_category == 'Initial scan' # we only want initial scans
    ) 
  
  return(df_ldct)
}

#' Get a dataframe for metric 5b - participants who did not attend their initial Low Dose CT scan
#' 
#' Return a record-level tibble in support of calculating the metric for non-attendance at
#' initial low dose CT scan
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_5b_ldct_initial_dna <- function() {
  
  p('df for metric 5b')
  
  # load the data if not already loaded
  if(!exists('df_ldct')){df_ldct <- load_df_ldct()}
  
  # processing
  df_ldct <- df_ldct |> 
    # add fields identifying the metric
    mutate(
      metric_id = '5b',
      metric_name = 'Number of participants who did not attend their initial Low Dose CT scan',
      month = calc_ldct_date_corrected_yearmon,
    ) |> 
    # limit the dataframe for valid metric
    filter(
      calc_valid_transactionid == 'Valid', # valid transactions
      #calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_ldct_date_corrected_yearmon), # exclude records without a LDCT date
      # (calc_PLCOm2012_risk_group == 'High risk' | calc_LLPv2_risk_group == 'High risk'), # identified as high risk using either score in LHC
      calc_ldct_outcome_corrected_groups == 'LDCT did not attend', # we have confirmation the patient DNA'd
      calc_ldct_date_corrected_days_from_first < 0 # count appointments before the initial scan
    ) 
  
  return(df_ldct)
}

#' Get a dataframe for metric 5d - participants who had a 3 month follow-up Low Dose CT scan performed
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' 3 month follow-up low dose CT scans performed
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_5d_ldct_3_month <- function() {
  
  p('df for metric 5d')
  
  # load the data if not already loaded
  if(!exists('df_ldct')){df_ldct <- load_df_ldct()}
  
  # processing
  df_ldct <- df_ldct |> 
    # add fields identifying the metric
    mutate(
      metric_id = '5d',
      metric_name = 'Number of participants who have had a 3 month follow up nodule surveillance LDCT scan performed',
      month = calc_ldct_date_corrected_yearmon,
    ) |> 
    # limit the dataframe for valid metric
    filter(
      calc_valid_transactionid == 'Valid', # valid transactions
      #calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_ldct_date_corrected_yearmon), # exclude records without a LDCT date
      #(calc_PLCOm2012_risk_group == 'High risk' | calc_LLPv2_risk_group == 'High risk'), # identified as high risk using either score in LHC
      calc_ldct_outcome_corrected_groups == 'LDCT performed', # we have confirmation the scan took place (i.e. exclude future booked)
      calc_ldct_date_corrected_category == '3 month follow-up scan' # we only want 3 month scans
    ) 
  
  return(df_ldct)
}

#' Get a dataframe for metric 5e - participants who had a 12 month follow-up Low Dose CT scan performed
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' 12 month follow-up low dose CT scans performed
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_5e_ldct_12_month <- function() {
  
  p('df for metric 5e')
  
  # load the data if not already loaded
  if(!exists('df_ldct')){df_ldct <- load_df_ldct()}
  
  # processing
  df_ldct <- df_ldct |> 
    # add fields identifying the metric
    mutate(
      metric_id = '5e',
      metric_name = 'Number of participants who have had a 12 month follow up nodule surveillance LDCT scan performed',
      month = calc_ldct_date_corrected_yearmon,
    ) |> 
    # limit the dataframe for valid metric
    filter(
      calc_valid_transactionid == 'Valid', # valid transactions
      #calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_ldct_date_corrected_yearmon), # exclude records without a LDCT date
      #(calc_PLCOm2012_risk_group == 'High risk' | calc_LLPv2_risk_group == 'High risk'), # identified as high risk using either score in LHC
      calc_ldct_outcome_corrected_groups == 'LDCT performed', # we have confirmation the scan took place (i.e. exclude future booked)
      calc_ldct_date_corrected_category == '12 month follow-up scan' # we only want 12 month scans
    ) 
  
  return(df_ldct)
}

#' Get a dataframe for metric 5f - participants who had a 24 month follow-up Low Dose CT scan performed
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' 12 month follow-up low dose CT scans performed
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_5f_ldct_24_month <- function() {
  
  p('df for metric 5f')
  
  # load the data if not already loaded
  if(!exists('df_ldct')){df_ldct <- load_df_ldct()}
  
  # processing
  df_ldct <- df_ldct |> 
    # add fields identifying the metric
    mutate(
      metric_id = '5f',
      metric_name = 'Number of participants who have had a 24 month incident screening round LDCT scan performed',
      month = calc_ldct_date_corrected_yearmon,
    ) |> 
    # limit the dataframe for valid metric
    filter(
      calc_valid_transactionid == 'Valid', # valid transactions
      #calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_ldct_date_corrected_yearmon), # exclude records without a LDCT date
      #(calc_PLCOm2012_risk_group == 'High risk' | calc_LLPv2_risk_group == 'High risk'), # identified as high risk using either score in LHC
      calc_ldct_outcome_corrected_groups == 'LDCT performed', # we have confirmation the scan took place (i.e. exclude future booked)
      calc_ldct_date_corrected_category == '24 month follow-up scan' # we only want 12 month scans
    ) 
  
  return(df_ldct)
}

#' Get a dataframe for metric 5g - participants who had a 48 month follow-up Low Dose CT scan performed
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' 48 month follow-up low dose CT scans performed
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_5g_ldct_48_month <- function() {
  
  p('df for metric 5g')
  
  # load the data if not already loaded
  if(!exists('df_ldct')){df_ldct <- load_df_ldct()}
  
  # processing
  df_ldct <- df_ldct |> 
    # add fields identifying the metric
    mutate(
      metric_id = '5g',
      metric_name = 'Number of participants who have had a 48 month incident screening round LDCT scan performed',
      month = calc_ldct_date_corrected_yearmon,
    ) |> 
    # limit the dataframe for valid metric
    filter(
      calc_valid_transactionid == 'Valid', # valid transactions
      #calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_ldct_date_corrected_yearmon), # exclude records without a LDCT date
      #(calc_PLCOm2012_risk_group == 'High risk' | calc_LLPv2_risk_group == 'High risk'), # identified as high risk using either score in LHC
      calc_ldct_outcome_corrected_groups == 'LDCT performed', # we have confirmation the scan took place (i.e. exclude future booked)
      calc_ldct_date_corrected_category == '48 month follow-up scan' # we only want 48 month scans
    ) 
  
  return(df_ldct)
}

#' Get a dataframe for metric 5h - participants who had a nodule surveillance scan 
#' from a 24-month incident screening round scan onwards.
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_5h_ldct_24_month_surveillance <- function() {
  
  p('df for metric 5h')
  
  # load the data if not already loaded
  if(!exists('df_ldct')){df_ldct <- load_df_ldct()}
  
  # processing
  df_ldct <- df_ldct |> 
    # add fields identifying the metric
    mutate(
      metric_id = '5h',
      metric_name = 'Number of participants who had a nodule surveillance LDCT scan performed from a 24 month incident screening round scan onwards',
      month = calc_ldct_date_corrected_yearmon,
    ) |> 
    # limit the dataframe for valid metric
    filter(
      calc_valid_transactionid == 'Valid', # valid transactions
      #calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_ldct_date_corrected_yearmon), # exclude records without a LDCT date
      #(calc_PLCOm2012_risk_group == 'High risk' | calc_LLPv2_risk_group == 'High risk'), # identified as high risk using either score in LHC
      calc_ldct_outcome_corrected_groups == 'LDCT performed', # we have confirmation the scan took place (i.e. exclude future booked)
      calc_ldct_date_corrected_category == '2-year+ nodule surveillance' # we only want surveillance scans
    ) 
  
  return(df_ldct)
}


#' Get a dataframe for metric 7 - participants who had any incidental finding
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental findings (any).
#' 
#' This data is produced by combining dataframes for metrics 7a-7u and will likely
#' contain duplicate patients (as the same patient can have multiple findings).
#' The data will need to be processed with this in mind.
#' 
#' @return Tibble
get_df_metric_7_incidental_any <- function() {
  
  p('df for metric 7')
  
  # ensure we have data
  if(!exists('df_metric_7a_incidental_consolidation')){df_metric_7a_incidental_consolidation <- get_df_metric_7a_incidental_consolidation()}
  if(!exists('df_metric_7b_incidental_tuberculosis')){df_metric_7b_incidental_tuberculosis <- get_df_metric_7b_incidental_tuberculosis()}
  if(!exists('df_metric_7c_incidental_mediastinalmass')){df_metric_7c_incidental_mediastinalmass <- get_df_metric_7c_incidental_mediastinal_mass()}
  if(!exists('df_metric_7d_incidental_coronarycalcification')){df_metric_7d_incidental_coronarycalcification <- get_df_metric_7d_incidental_coronary_calcification()}
  if(!exists('df_metric_7e_incidental_aorticvalvecalcification')){df_metric_7e_incidental_aorticvalvecalcification <- get_df_metric_7e_incidental_aortic_valve_calcification()}
  if(!exists('df_metric_7f_incidental_thoracicaorticaneurysm')){df_metric_7f_incidental_thoracicaorticaneurysm <- get_df_metric_7f_incidental_thoracic_aortic_aneurysm()}
  if(!exists('df_metric_7g_incidental_pleuraleffusions')){df_metric_7g_incidental_pleuraleffusions <- get_df_metric_7g_incidental_pleural_effusions_thickening()}
  if(!exists('df_metric_7h_incidental_suspiciousbreastlesion')){df_metric_7h_incidental_suspiciousbreastlesion <- get_df_metric_7h_incidental_suspicious_breast_lesion()}
  if(!exists('df_metric_7i_incidental_tyroidlesion')){df_metric_7i_incidental_tyroidlesion <- get_df_metric_7i_incidental_thyroid_lesion()}
  if(!exists('df_metric_7j_incidental_liverspleniclesions')){df_metric_7j_incidental_liverspleniclesions <- get_df_metric_7j_incidental_liver_or_splenic_lesions()}
  if(!exists('df_metric_7k_incidental_renallesions')){df_metric_7k_incidental_renallesions <- get_df_metric_7k_incidental_renal_lesions()}
  if(!exists('df_metric_7l_incidental_adrenallesions')){df_metric_7l_incidental_adrenallesions <- get_df_metric_7l_incidental_adrenal_lesions()}
  if(!exists('df_metric_7m_incidental_abdominalaorticaneurysm')){df_metric_7m_incidental_abdominalaorticaneurysm <- get_df_metric_7m_incidental_abdominal_aortic_aneurysm()}
  if(!exists('df_metric_7n_incidental_boneabnormalities')){df_metric_7n_incidental_boneabnormalities <- get_df_metric_7n_incidental_bone_abnormalities()}
  if(!exists('df_metric_7o_incidental_osteoporosis')){df_metric_7o_incidental_osteoporosis <- get_df_metric_7o_incidental_osteoporosis()}
  if(!exists('df_metric_7p_incidental_fracturesnotrauma')){df_metric_7p_incidental_fracturesnotrauma <- get_df_metric_7p_incidental_fractures_with_no_trauma_history()}
  if(!exists('df_metric_7q_incidental_bronchiectasis')){df_metric_7q_incidental_bronchiectasis <- get_df_metric_7q_bronchiectasis()}
  if(!exists('df_metric_7r_incidental_respiratorybronchiolitis')){df_metric_7r_incidental_respiratorybronchiolitis <- get_df_metric_7r_respiratory_bronchiolitis()}
  if(!exists('df_metric_7s_incidental_interstitiallungabnormalities')){df_metric_7s_incidental_interstitiallungabnormalities <- get_df_metric_7s_interstitial_lung_abnormalities()}
  if(!exists('df_metric_7t_incidental_othercancers_alt')){df_metric_7t_incidental_othercancers_alt <- get_df_metric_7t_incidental_other_cancers_alternate()}
  if(!exists('df_metric_7u_incidental_emphysema')){df_metric_7u_incidental_emphysema <- get_df_metric_7u_incidental_emphysema()}
  
  # processing to a single table
  df <- bind_rows(
    df_metric_7a_incidental_consolidation,
    df_metric_7b_incidental_tuberculosis,
    df_metric_7c_incidental_mediastinalmass,
    df_metric_7d_incidental_coronarycalcification,
    df_metric_7e_incidental_aorticvalvecalcification,
    df_metric_7f_incidental_thoracicaorticaneurysm,
    df_metric_7g_incidental_pleuraleffusions,
    df_metric_7h_incidental_suspiciousbreastlesion,
    df_metric_7i_incidental_tyroidlesion,
    df_metric_7j_incidental_liverspleniclesions,
    df_metric_7k_incidental_renallesions,
    df_metric_7l_incidental_adrenallesions,
    df_metric_7m_incidental_abdominalaorticaneurysm,
    df_metric_7n_incidental_boneabnormalities,
    df_metric_7o_incidental_osteoporosis,
    df_metric_7p_incidental_fracturesnotrauma,
    df_metric_7q_incidental_bronchiectasis,
    df_metric_7r_incidental_respiratorybronchiolitis,
    df_metric_7s_incidental_interstitiallungabnormalities,
    df_metric_7t_incidental_othercancers_alt, # nb, using the alternate approach as it has data
    df_metric_7u_incidental_emphysema
  ) |> 
    select(-metric_id, -metric_name) |>  # don't want the names/ids for metrics 7a-7u
    unique() |>  # will remove some duplicates but not all
    mutate(
      metric_id = '7',
      metric_name = 'Number of patients with incidental findings'
    )
  
  return(df)
}

#' Get a dataframe for metric 7t - participants who had an incidental finding of
#' other cancer (alternative approach using [Referral_To_Secondary_Care_Other_Cancer_MDT])
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of other cancer
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7t_incidental_other_cancers_alternate <- function() {
  
  p('df for metric 7t')
  
  # load the data if not already loaded
  if(!exists('df_ldct')){df_ldct <- load_df_ldct()}
  
  # processing
  df <- df_ldct |> 
    # add fields identifying the metric
    mutate(
      metric_id = '7t',
      metric_name = 'Number of patients with incidental findings - other cancers',
      month = calc_ldct_date_corrected_yearmon,
    ) |> 
    # limit the dataframe for valid metric
    filter(
      calc_valid_transactionid == 'Valid', # valid transactions
      calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_ldct_date_corrected_yearmon), # exclude records without a LDCT date
      #(calc_PLCOm2012_risk_group == 'High risk' | calc_LLPv2_risk_group == 'High risk'), # identified as high risk using either score in LHC
      calc_ldct_outcome_corrected_groups == 'LDCT performed', # we have confirmation the scan took place (i.e. exclude future booked)
      !is.na(calc_date_referralothercancer), # we have a non-null date
    ) 
  
  return(df)
}

#' Get a dataframe for metric 7u - participants who had an incidental finding of
#' emphysema (moderate or severe)
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of emphysema (moderate or severe)
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7u_incidental_emphysema <- function() {
  
  p('df for metric 7u')
  
  # load the data if not already loaded
  if(!exists('df_ldct')){df_ldct <- load_df_ldct()}
  
  # processing
  df <- df_ldct |> 
    # add fields identifying the metric
    mutate(
      metric_id = '7u',
      metric_name = 'Number of patients with incidental findings - emphysema',
      month = calc_ldct_date_corrected_yearmon,
    ) |> 
    # limit the dataframe for valid metric
    filter(
      calc_valid_transactionid == 'Valid', # valid transactions
      calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_ldct_date_corrected_yearmon), # exclude records without a LDCT date
      #(calc_PLCOm2012_risk_group == 'High risk' | calc_LLPv2_risk_group == 'High risk'), # identified as high risk using either score in LHC
      calc_ldct_outcome_corrected_groups == 'LDCT performed', # we have confirmation the scan took place (i.e. exclude future booked)
      str_detect(string = toupper(Emphysema_Extent), pattern = 'MODERATE|SEVERE') # reported emphysema as moderate or severe
    ) 
  
  return(df)
}

#' Get a dataframe for metric 14a - participants referred to the Lung Cancer
#' Pathway following their LDCT scan
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_14a_lc_pathway_following_ldct <- function() {
  
  p('df for metric 14a')
  
  # load the data if not already loaded
  if(!exists('df_ldct')){df_ldct <- load_df_ldct()}
  
  # processing
  df <- df_ldct |> 
    # add fields identifying the metric
    mutate(
      metric_id = '14a',
      metric_name = 'Number of participants referred to the Lung Cancer pathway following their Low Dose CT scan',
      month = calc_date_referral_lung_cancer_yearmon,
    ) |> 
    # limit the dataframe for valid metric
    filter(
      calc_valid_transactionid == 'Valid', # valid transactions
      calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_ldct_date_corrected_yearmon), # exclude records without a LDCT date
      !is.na(calc_date_referral_lung_cancer_yearmon), # exclude records without a lung cancer referral date
      (calc_PLCOm2012_risk_group == 'High risk' | calc_LLPv2_risk_group == 'High risk'), # identified as high risk using either score in LHC
      calc_ldct_outcome_corrected_groups == 'LDCT performed' # we have confirmation the scan took place (i.e. exclude future booked)
    ) 
  
  return(df)
}


#### based on ldct other incidental --------------------------------------------
#' Get a dataframe for metric 7a - participants who had an incidental finding of
#' consolidation.
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of consolidation
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7a_incidental_consolidation <- function() {
  
  p('df for metric 7a')
  
  # processing
  df <- get_other_incidental_data_for_item(
    str_finding = 'consolidation',
    dec_dist = 0.2,
    str_metric_id = '7a',
    str_metric_name = 'Number of patients with incidental findings - consolidation'
  )
  
  return(df)
}

#' Get a dataframe for metric 7b - participants who had an incidental finding of
#' tuberculosis.
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of tuberculosis.
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7b_incidental_tuberculosis <- function() {
  
  p('df for metric 7b')
  
  # processing
  df <- get_other_incidental_data_for_item(
    str_finding = 'tuberculosis',
    dec_dist = 0.2,
    str_metric_id = '7b',
    str_metric_name = 'Number of patients with incidental findings - tuberculosis'
  )
  
  return(df)
}

#' Get a dataframe for metric 7c - participants who had an incidental finding of
#' mediastinal mass
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of mediastinal mass
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7c_incidental_mediastinal_mass <- function() {
  
  p('df for metric 7c')
  
  # processing
  df <- get_other_incidental_data_for_item(
    str_finding = 'mediastinal mass',
    dec_dist = 0.2,
    str_metric_id = '7c',
    str_metric_name = 'Number of patients with incidental findings - mediastinal mass'
  )
  
  return(df)
}

#' Get a dataframe for metric 7d - participants who had an incidental finding of
#' coronary calcification
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of coronary calcification
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7d_incidental_coronary_calcification <- function() {
  
  p('df for metric 7d')
  
  # processing
  df <- get_other_incidental_data_for_item(
    str_finding = 'coronary calcification',
    dec_dist = 0.2,
    str_metric_id = '7d',
    str_metric_name = 'Number of patients with incidental findings - coronary calcification'
  )
  
  return(df)
}

#' Get a dataframe for metric 7e - participants who had an incidental finding of
#' aortic valve calcification
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of aortic valve calcification
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7e_incidental_aortic_valve_calcification <- function() {
  
  p('df for metric 7e')
  
  # processing
  df <- get_other_incidental_data_for_item(
    str_finding = 'aortic valve calcification',
    dec_dist = 0.2,
    str_metric_id = '7e',
    str_metric_name = 'Number of patients with incidental findings - aortic valve calcification'
  )
  
  return(df)
}


#' Get a dataframe for metric 7f - participants who had an incidental finding of
#' thoracic aortic aneurysm
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of thoracic aortic aneurysm
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7f_incidental_thoracic_aortic_aneurysm <- function() {
  
  p('df for metric 7f')
  
  # processing
  df <- get_other_incidental_data_for_item(
    str_finding = 'thoracic aortic aneurysm',
    dec_dist = 0.2,
    str_metric_id = '7f',
    str_metric_name = 'Number of patients with incidental findings - thoracic aortic aneurysm'
  )
  
  return(df)
}

#' Get a dataframe for metric 7g - participants who had an incidental finding of
#' pleural effusions/thickening
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of pleural effusions/thickening
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7g_incidental_pleural_effusions_thickening <- function() {
  
  p('df for metric 7g')
  
  # processing
  df <- get_other_incidental_data_for_item(
    str_finding = 'pleural effusions/thickening',
    dec_dist = 0.2,
    str_metric_id = '7g',
    str_metric_name = 'Number of patients with incidental findings - pleural effusions/thickening'
  )
  
  return(df)
}

#' Get a dataframe for metric 7h - participants who had an incidental finding of
#' suspicious breast lesion
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of pleural effusions/thickening
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7h_incidental_suspicious_breast_lesion <- function() {
  
  p('df for metric 7h')
  
  # processing
  df <- get_other_incidental_data_for_item(
    str_finding = 'suspicious breast lesion',
    dec_dist = 0.2,
    str_metric_id = '7h',
    str_metric_name = 'Number of patients with incidental findings - suspicious breast lesion'
  )
  
  return(df)
}

#' Get a dataframe for metric 7i - participants who had an incidental finding of
#' thyroid lesion
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of thyroid lesion
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7i_incidental_thyroid_lesion <- function() {
  
  p('df for metric 7i')
  
  # processing
  df <- get_other_incidental_data_for_item(
    str_finding = 'thyroid lesion',
    dec_dist = 0.2,
    str_metric_id = '7i',
    str_metric_name = 'Number of patients with incidental findings - thyroid lesion'
  )
  
  return(df)
}

#' Get a dataframe for metric 7j - participants who had an incidental finding of
#' liver or splenic lesions
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of liver or splenic lesions
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7j_incidental_liver_or_splenic_lesions <- function() {
  
  p('df for metric 7j')
  
  # processing
  df <- get_other_incidental_data_for_item(
    str_finding = 'liver or splenic lesions',
    dec_dist = 0.2,
    str_metric_id = '7j',
    str_metric_name = 'Number of patients with incidental findings - liver or splenic lesions'
  )
  
  return(df)
}

#' Get a dataframe for metric 7k - participants who had an incidental finding of
#' renal lesions
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of renal lesions
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7k_incidental_renal_lesions <- function() {
  
  p('df for metric 7k')
  
  # processing
  df <- get_other_incidental_data_for_item(
    str_finding = 'renal lesions',
    dec_dist = 0.1,
    str_metric_id = '7k',
    str_metric_name = 'Number of patients with incidental findings - renal lesions'
  )
  
  return(df)
}

#' Get a dataframe for metric 7l - participants who had an incidental finding of
#' adrenal lesions
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of adrenal lesions
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7l_incidental_adrenal_lesions <- function() {
  
  p('df for metric 7l')
  
  # processing
  df <- get_other_incidental_data_for_item(
    str_finding = 'adrenal lesions',
    dec_dist = 0.1,
    str_metric_id = '7l',
    str_metric_name = 'Number of patients with incidental findings - adrenal lesions'
  )
  
  return(df)
}

#' Get a dataframe for metric 7m - participants who had an incidental finding of
#' abdominal aortic aneurysm
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of abdominal aortic aneurysm
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7m_incidental_abdominal_aortic_aneurysm <- function() {
  
  p('df for metric 7m')
  
  # processing
  df <- get_other_incidental_data_for_item(
    str_finding = 'abdominal aortic aneurysm',
    dec_dist = 0.2,
    str_metric_id = '7m',
    str_metric_name = 'Number of patients with incidental findings - abdominal aortic aneurysm'
  )
  
  return(df)
}

#' Get a dataframe for metric 7n - participants who had an incidental finding of
#' bone abnormalities
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of bone abnormalities
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7n_incidental_bone_abnormalities <- function() {
  
  p('df for metric 7n')
  
  # processing
  df <- get_other_incidental_data_for_item(
    str_finding = 'bone abnormalities',
    dec_dist = 0.2,
    str_metric_id = '7n',
    str_metric_name = 'Number of patients with incidental findings - bone abnormalities'
  )
  
  return(df)
}

#' Get a dataframe for metric 7o - participants who had an incidental finding of
#' osteoporosis
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of osteoporosis
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7o_incidental_osteoporosis <- function() {
  
  p('df for metric 7o')
  
  # processing
  df <- get_other_incidental_data_for_item(
    str_finding = 'osteoporosis',
    dec_dist = 0.2,
    str_metric_id = '7o',
    str_metric_name = 'Number of patients with incidental findings - osteoporosis'
  )
  
  return(df)
}


#' Get a dataframe for metric 7p - participants who had an incidental finding of
#' fractures with no trauma history
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of fractures with no trauma history
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7p_incidental_fractures_with_no_trauma_history <- function() {
  
  p('df for metric 7p')
  
  # processing
  df <- get_other_incidental_data_for_item(
    str_finding = 'fractures with no trauma history',
    dec_dist = 0.2,
    str_metric_id = '7p',
    str_metric_name = 'Number of patients with incidental findings - fractures with no trauma history'
  )
  
  return(df)
}

#' Get a dataframe for metric 7t - participants who had an incidental finding of
#' other cancer
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of other cancer
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7t_incidental_other_cancers <- function() {
  
  p('df for metric 7t')
  
  # processing
  df <- get_other_incidental_data_for_item(
    str_finding = 'other cancers',
    dec_dist = 0.3,
    str_metric_id = '7t',
    str_metric_name = 'Number of patients with incidental findings - other cancers'
  )
  
  return(df)
}




#### based on ldct pulmonary incidental ----------------------------------------

#' Get a dataframe for metric 7q - participants who had an incidental finding of
#' bronchiectasis
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of bronchiectasis
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7q_bronchiectasis <- function() {
  
  p('df for metric 7q')
  
  # processing
  df <- get_pulmonary_incidental_data_for_item(
    str_finding = 'bronchiectasis',
    dec_dist = 0.2,
    str_metric_id = '7q',
    str_metric_name = 'Number of patients with incidental findings - bronchiectasis'
  )
  
  return(df)
}

#' Get a dataframe for metric 7r - participants who had an incidental finding of
#' bronchiectasis
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of bronchiectasis
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7r_respiratory_bronchiolitis <- function() {
  
  p('df for metric 7r')
  
  # processing
  df <- get_pulmonary_incidental_data_for_item(
    str_finding = 'respiratory bronchiolitis',
    dec_dist = 0.2,
    str_metric_id = '7r',
    str_metric_name = 'Number of patients with incidental findings - respiratory bronchiolitis'
  )
  
  return(df)
}

#' Get a dataframe for metric 7s - participants who had an incidental finding of
#' bronchiectasis
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' incidental finding of bronchiectasis
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_7s_interstitial_lung_abnormalities <- function() {
  
  p('df for metric 7s')
  
  # processing for full name
  df1 <- get_pulmonary_incidental_data_for_item(
    str_finding = 'interstitial lung abnormalities (ilas)',
    dec_dist = 0.2,
    str_metric_id = '7s',
    str_metric_name = 'Number of patients with incidental findings - interstitial lung abnormalities (ilas)'
  )
  
  # processing for part match
  df2 <- get_pulmonary_incidental_data_for_item(
    str_finding = 'ila',
    dec_dist = 0.2,
    str_metric_id = '7s',
    str_metric_name = 'Number of patients with incidental findings - interstitial lung abnormalities (ilas)'
  )
  
  # # processing for part match
  # df3 <- get_pulmonary_incidental_data_for_item(
  #   str_finding = 'interstitial',
  #   dec_dist = 0.2,
  #   str_metric_id = '7s',
  #   str_metric_name = 'Number of patients with incidental findings - interstitial lung abnormalities (ilas)'
  # )
  
  # combine results
  df <- bind_rows(
    df1,
    df2
  ) |> 
    unique()
  
  return(df)
}

### based on aggregate data ----------------------------------------------------

#' Get a dataframe for metrics 6a-6f - participants with lung cancer diagnoses
#' 
#' Return a pre-aggregated tibble in support of calculating the metric for 
#' lung cancer diagnoses
#' 
#' @return Tibble
get_df_metric_6_lung_cancers_from_aggregate <- function() {
  
  # get a dataframe with our data (pre aggregated)
  df <- load_df_lung_cancers_from_aggregate()
  
}

## based on smoking ------------------------------------------------------------

#' Get a dataframe for metric 8b - participants who were offered the option of a
#' smoking cessation course
#' 
#' @return Tibble
get_df_metric_8b_smoking_offered <- function() {
  
  p('df for metric 8b')
  
  # load the data if not already loaded
  if(!exists('df_smoking')){df_smoking <- load_df_smoking()}
  
  # processing
  df_smoking <- df_smoking |> 
    # add fields identifying the metric
    mutate(
      metric_id = '8b',
      metric_name = 'Number of participants who were offered the option of a smoking cessation course',
      month = calc_date_offered_smoking_cessation_yearmon,
    ) |> 
    # limit the dataframe for valid metric
    filter(
      calc_valid_transactionid == 'Valid', # valid transactions
      calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_date_offered_smoking_cessation_yearmon) # exclude records without a date offered smoking cessation
    ) 
  
  return(df_smoking)
  
}


#' Get a dataframe for metric 8a - participants who took up a smoking 
#' cessation course
#' 
#' @return Tibble
get_df_metric_8a_smoking_started <- function() {
  
  p('df for metric 8a')
  
  # load the data if not already loaded
  if(!exists('df_smoking')){df_smoking <- load_df_smoking()}
  
  # processing
  df_smoking <- df_smoking |> 
    # add fields identifying the metric
    mutate(
      metric_id = '8a',
      metric_name = 'Number of participants who took up a smoking cessation course',
      month = calc_date_started_smoking_cessation_yearmon,
    ) |> 
    # limit the dataframe for valid metric
    filter(
      calc_valid_transactionid == 'Valid', # valid transactions
      calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_date_offered_smoking_cessation_yearmon), # exclude records without a date offered smoking cessation
      !is.na(calc_date_started_smoking_cessation_yearmon) # exclude records without a date started smoking cessation
    ) 
  
  return(df_smoking)
  
}


#' Get a dataframe for metric 9 - participants who completed a smoking 
#' cessation course
#' 
#' @return Tibble
get_df_metric_9_smoking_completed <- function() {
  
  p('df for metric 9')
  
  # load the data if not already loaded
  if(!exists('df_smoking')){df_smoking <- load_df_smoking()}
  
  # processing
  df_smoking <- df_smoking |> 
    # add fields identifying the metric
    mutate(
      metric_id = '9',
      metric_name = 'Number of participants who completed a smoking cessation course',
      month = calc_date_ended_smoking_cessation_yearmon,
    ) |> 
    # limit the dataframe for valid metric
    filter(
      calc_valid_transactionid == 'Valid', # valid transactions
      calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_date_offered_smoking_cessation_yearmon), # exclude records without a date offered smoking cessation
      !is.na(calc_date_started_smoking_cessation_yearmon), # exclude records without a date started smoking cessation
      !is.na(calc_date_ended_smoking_cessation_yearmon), # exclude records without a date the course ended
      calc_smoking_cessation_completed_successfully == 'YES' # explicit confirmation the course ended successfully
    ) 
  
  return(df_smoking)
  
}

## based on median time metrics ------------------------------------------------

#' Get a dataframe for metric 10 - median time from invite to lhc
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' median time (days) between initial invite and the lhc date
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_10_median_inv_to_lhc <- function() {
  
  p('df for metric 10')
  
  # load the data if not already loaded - this is a helper function that has the metric logic included
  if(!exists('df_lhc_first_attended')){df_lhc_first_attended <- get_first_attended_lhc_per_participant()}
  
  return(
    df_lhc_first_attended |> 
      # add fields identifying the metric
      mutate(
        metric_id = '10',
        metric_name = 'Median time from initial invite to lung health check (days)',
        month = calc_lhc_date_yearmon,
      ) |> 
      # calculate the number of days between invite and lhc
      mutate(calc_days_from_invite_to_lhc = (calc_lhc_date - calc_first_letter_date)) |> 
      filter(calc_days_from_invite_to_lhc >= 0) # only keep records with a positive number
  )
}


#' Get a dataframe for metric 11 - median time from invite to ldct
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' median time (days) between initial invite and the ldct scan date
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_11_median_inv_to_ldct <- function() {
  
  p('df for metric 11')
  
  # load the data if not already loaded
  if(!exists('df_ldct')){df_ldct <- load_df_ldct()}
  
  return(
    df_ldct |> 
      # add fields identifying the metric
      mutate(
        metric_id = '11',
        metric_name = 'Median time from initial invite to CT scan (days)',
        month = calc_ldct_date_corrected_yearmon,
      ) |> 
      # limit the dataframe for valid metric
      filter(
        calc_valid_transactionid == 'Valid', # valid transactions
        calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
        calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
        !is.na(calc_ldct_date_corrected_yearmon), # exclude records without a LDCT date
        calc_ldct_outcome_corrected_groups == 'LDCT performed', # we have confirmation the scan took place (i.e. exclude future booked)
        calc_ldct_date_corrected_category == 'Initial scan', # we only want initial scans
        !is.na(calc_first_letter_date) # exclude records without an invite date
      ) |> 
      # calculate the number of days between invite and lhc
      mutate(calc_days_from_invite_to_ldct = (calc_ldct_date_corrected - calc_first_letter_date)) |> 
      filter(calc_days_from_invite_to_ldct >= 0) # only keep records with a positive number
  )
}

#' Get a dataframe for metric 12 - median time from lhc to ldct
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' median time (days) between lung health check and the initial ldct scan date
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_12_median_lhc_to_ldct <- function() {
  
  p('df for metric 12')
  
  # load the data if not already loaded
  if(!exists('df_ldct')){df_ldct <- load_df_ldct()}
  
  return(
    df_ldct |> 
      # add fields identifying the metric
      mutate(
        metric_id = '12',
        metric_name = 'Median time from lung health check to CT scan (days)',
        month = calc_ldct_date_corrected_yearmon,
      ) |> 
      # limit the dataframe for valid metric
      filter(
        calc_valid_transactionid == 'Valid', # valid transactions
        calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
        calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
        !is.na(calc_ldct_date_corrected_yearmon), # exclude records without a LDCT date
        calc_ldct_outcome_corrected_groups == 'LDCT performed', # we have confirmation the scan took place (i.e. exclude future booked)
        calc_ldct_date_corrected_category == 'Initial scan', # we only want initial scans
        !is.na(calc_lhc_date) # exclude records without a lhc date
      ) |> 
      # calculate the number of days between lhc and scan
      mutate(calc_days_from_lhc_to_ldct = (calc_ldct_date_corrected - calc_lhc_date)) |> 
      filter(calc_days_from_lhc_to_ldct >= 0) # only keep records with a positive number
  )
}


#' Get a dataframe for metric 13 - median time from invite to diagnosis
#' 
#' Return a record-level tibble in support of calculating the metric for 
#' median time (days) between invite for a lung health check and the diagnosis
#' date
#' 
#' Data is filtered for valid records where we have a value in the month 
#' dimension
#' 
#' @return Tibble
get_df_metric_13_median_inv_to_diag <- function() {
  
  p('df for metric 13')
  
  # load the data if not already loaded
  if(!exists('df_diag')){df_diag <- load_df_diagnostics()}
  
  return(
    df_diag |> 
      # add fields identifying the metric
      mutate(
        metric_id = '13',
        metric_name = 'Median time from initial invite to cancer diagnosis (days)',
        month = calc_cancer_diagnosis_date_yearmon,
      ) |> 
      # limit the dataframe for valid metric
      filter(
        calc_valid_transactionid == 'Valid', # valid transactions
        calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
        calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
        !is.na(calc_cancer_diagnosis_date_yearmon), # exclude records without a cancer diagnosis date
        !is.na(calc_first_letter_date) # exclude records without an invite date
      ) |> 
      # calculate the number of days between lhc and scan
      mutate(calc_days_from_inv_to_diag = (calc_cancer_diagnosis_date - calc_first_letter_date)) |> 
      filter(calc_days_from_inv_to_diag >= 0) # only keep records with a positive number
  )
}


## calculate aggregate metrics -------------------------------------------------

#' Calculate metric 1a performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 1a
calculate_metric_1a <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_1a_invites_first() |> aggregate_metric_standard()
  
}

#' Calculate metric 1b performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 1b
calculate_metric_1c <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_1c_invites_followup() |> aggregate_metric_standard()
  
}

#' Calculate metric 2 performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 2
calculate_metric_2 <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_2_invites_accepted() |>  aggregate_metric_standard()
  
}

#' Calculate metric 3a performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 3a
calculate_metric_3a <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_3a_attend_f2f() |> aggregate_metric_standard()
  
}

# calculate_metric_3a_v2 <- function() {
#   df <- get_df_metric_3a_attend_f2f_v2() |> aggregate_metric_standard()
# }

#' Calculate metric 3b performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 3b
calculate_metric_3b <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_3b_attend_tel() |> aggregate_metric_standard()
  
}

# calculate_metric_3b_v2 <- function() {
#   df <- get_df_metric_3b_attend_tel_v2() |> aggregate_metric_standard()
# }

#' Calculate metric 3c performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 3c
calculate_metric_3c <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_3c_dna_f2f() |> aggregate_metric_standard()
  
}

#' Calculate metric 3d performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 3d
calculate_metric_3d <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_3d_dna_tel() |> aggregate_metric_standard()
  
}

#' Calculate metric 4a performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 4a
calculate_metric_4a <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_4a_ldct_referral() |> aggregate_metric_standard()
  
}

#' Calculate metric 4b performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 4b
calculate_metric_4b <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_4b_ldct_ineligible() |> aggregate_metric_standard()
  
}

#' Calculate metric 5a performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 5a
calculate_metric_5a <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_5a_ldct_initial() |> aggregate_metric_standard()
  
}

#' Calculate metric 5b performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 5b
calculate_metric_5b <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_5b_ldct_initial_dna() |> aggregate_metric_standard()
  
}

#' Calculate metric 5d performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 5d
calculate_metric_5d <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_5d_ldct_3_month() |> aggregate_metric_standard()
  
}

#' Calculate metric 5e performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 5e
calculate_metric_5e <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_5e_ldct_12_month() |> aggregate_metric_standard()
  
}

#' Calculate metric 5f performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 5f
calculate_metric_5f <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_5f_ldct_24_month() |> aggregate_metric_standard()
  
}

#' Calculate metric 5g performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 5g
calculate_metric_5g <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_5g_ldct_48_month() |> aggregate_metric_standard()
  
}

#' Calculate metric 5h performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 5h
calculate_metric_5h <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_5h_ldct_24_month_surveillance() |> aggregate_metric_standard()
  
}

#' Calculate lung cancer performance (metrics 6a-6f)
#'
#' @return df Tibble of aggregated lung cancer performance by project and month
calculate_metric_6a6f <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_6_lung_cancers_from_aggregate()
  
}

#' Calculate metric 7 performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7
calculate_metric_7 <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7_incidental_any() |> aggregate_metric_standard()
  
}

#' Calculate metric 7a performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7a
calculate_metric_7a <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7a_incidental_consolidation() |> aggregate_metric_standard()
  
}

#' Calculate metric 7b performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7b
calculate_metric_7b <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7b_incidental_tuberculosis() |> aggregate_metric_standard()
  
}

#' Calculate metric 7c performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7c
calculate_metric_7c <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7c_incidental_mediastinal_mass() |> aggregate_metric_standard()
  
}

#' Calculate metric 7d performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7d
calculate_metric_7d <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7d_incidental_coronary_calcification() |> aggregate_metric_standard()
  
}

#' Calculate metric 7e performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7e
calculate_metric_7e <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7e_incidental_aortic_valve_calcification() |> aggregate_metric_standard()
  
}

#' Calculate metric 7f performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7f
calculate_metric_7f <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7f_incidental_thoracic_aortic_aneurysm() |> aggregate_metric_standard()
  
}

#' Calculate metric 7g performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7g
calculate_metric_7g <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7g_incidental_pleural_effusions_thickening() |> aggregate_metric_standard()
  
}

#' Calculate metric 7h performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7h
calculate_metric_7h <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7h_incidental_suspicious_breast_lesion() |> aggregate_metric_standard()
  
}

#' Calculate metric 7i performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7i
calculate_metric_7i <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7i_incidental_thyroid_lesion() |> aggregate_metric_standard()
  
}

#' Calculate metric 7j performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7j
calculate_metric_7j <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7j_incidental_liver_or_splenic_lesions() |> aggregate_metric_standard()
  
}

#' Calculate metric 7k performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7k
calculate_metric_7k <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7k_incidental_renal_lesions() |> aggregate_metric_standard()
  
}

#' Calculate metric 7l performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7l
calculate_metric_7l <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7l_incidental_adrenal_lesions() |> aggregate_metric_standard()
  
}

#' Calculate metric 7m performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7m
calculate_metric_7m <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7m_incidental_abdominal_aortic_aneurysm() |> aggregate_metric_standard()
  
}

#' Calculate metric 7n performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7n
calculate_metric_7n <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7n_incidental_bone_abnormalities() |> aggregate_metric_standard()
  
}

#' Calculate metric 7o performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7o
calculate_metric_7o <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7o_incidental_osteoporosis() |> aggregate_metric_standard()
  
}

#' Calculate metric 7p performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7p
calculate_metric_7p <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7p_incidental_fractures_with_no_trauma_history() |> aggregate_metric_standard()
  
}

#' Calculate metric 7q performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7q
calculate_metric_7q <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7q_bronchiectasis() |> aggregate_metric_standard()
  
}

#' Calculate metric 7r performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7r
calculate_metric_7r <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7r_respiratory_bronchiolitis() |> aggregate_metric_standard()
  
}

#' Calculate metric 7s performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7s
calculate_metric_7s <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7s_interstitial_lung_abnormalities() |> aggregate_metric_standard()
  
}

#' Calculate metric 7t performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7t
calculate_metric_7t <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7t_incidental_other_cancers_alternate() |> aggregate_metric_standard()
  
}

#' Calculate metric 7u performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 7u
calculate_metric_7u <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_7u_incidental_emphysema() |> aggregate_metric_standard()
  
}

#' Calculate metric 8b performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 8b
calculate_metric_8b <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_8b_smoking_offered() |> aggregate_metric_standard()
  
}

#' Calculate metric 8a performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 8a
calculate_metric_8a <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_8a_smoking_started() |> aggregate_metric_standard()
  
}

#' Calculate metric 9 performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 9
calculate_metric_9 <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_9_smoking_completed() |> aggregate_metric_standard()
  
}

#' Calculate metric 10 performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 10
calculate_metric_10 <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_10_median_inv_to_lhc() |> aggregate_metric_standard_median(str_median_field = calc_days_from_invite_to_lhc)
  
}

#' Calculate metric 11 performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 11
calculate_metric_11 <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_11_median_inv_to_ldct() |> aggregate_metric_standard_median(str_median_field = calc_days_from_invite_to_ldct)
  
}

#' Calculate metric 12 performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 12
calculate_metric_12 <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_12_median_lhc_to_ldct() |> aggregate_metric_standard_median(str_median_field = calc_days_from_lhc_to_ldct)
  
}

#' Calculate metric 13 performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 13
calculate_metric_13 <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_13_median_inv_to_diag() |> aggregate_metric_standard_median(str_median_field = calc_days_from_inv_to_diag)
  
}

#' Calculate metric 14a performance
#'
#' @return df Tibble of aggregated performance by project and month for metric 14a
calculate_metric_14a <- function() {
  
  # get a dataframe with our data and aggregate
  df <- get_df_metric_14a_lc_pathway_following_ldct() |> aggregate_metric_standard()
  
}
