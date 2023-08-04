#'------------------------------------------------------------------------------
#' TLHC CALCULATE DEMOGRAPHIC REPORTS
#' 
#' Calculate demographic reports
#' 
#'------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(tidyverse)     # tidy data processing
library(here)          # localise file references
library(lubridate)     # date functions
library(zoo)           # date functions
library(progressr)     # progress bar
library(tictoc)        # timing monitor
library(fuzzyjoin)     # joining on fuzzy word matches
library(janitor)       # convenience function to add totals to columns
library(writexl)       # outputs the results to excel
library(tictoc)        # performance monitoring

# UDFs 
source(here('scripts', 'func_name_projects.R')) # naming projects
source(here('scripts', 'tlhc_metric_functions.R')) # functions for loading metric data
source(here('scripts', 'tlhc_general_functions.R')) # user updates

# Notify user 
update_user(stage = 'start', message = 'tlhc_demographics.R')
tic()

# load metric data -------------------------------------------------------------

# set up the progress indicator
handlers(handler_progress(format='[:bar] :percent :eta :message')) # set up the progress indicator

## base tables -----------------------------------------------------------------
#' load base tables into memory ready for use by the other metric calculations
#' nb, loading these beforehand makes the rest of the metrics quicker to work 
#' out.

with_progress({
  
  update_user(message = 'Loading base tables ...', icon = '⏱️')
  p <- progressor(steps = 9)
  
  df_demo <- load_df_demo()
  df_demo_join <- load_df_demo_join()
  df_invites <- load_df_invites()
  df_lhc <- load_df_lhc()
  df_meas <- load_df_measurements()
  df_ldct <- load_df_ldct()
  df_ldct_incid_other <- extract_other_incidental_data()
  df_ldct_incid_pulm <- extract_pulmonary_incidental_data()
  df_smoking <- load_df_smoking()
  df_diag <- load_df_diagnostics()
  
  update_user(message = 'Base tables loaded')
})

## metric tables -----------------------------------------------------------------
#' load base tables into memory ready for use by the other metric calculations
#' nb, loading these beforehand makes the rest of the metrics quicker to work 
#' out.

with_progress({
  
  update_user(message = 'Loading metric tables ...', icon = '⏱️')
  p <- progressor(steps = 6)
  
  df_metric_1a_invites_first <- get_df_metric_1a_invites_first()
  df_metric_3a_attend_f2f <- get_df_metric_3a_attend_f2f()
  df_metric_3b_attend_tel <- get_df_metric_3b_attend_tel()
  df_metric_3ab_attend <- bind_rows(
    df_metric_3a_attend_f2f,
    df_metric_3b_attend_tel
  )                                                                             # combine f2f and tel into a single df
  df_metric_4a_ldct_referral <- get_df_metric_4a_ldct_referral()
  df_metric_5a_ldct_initial <- get_df_metric_5a_ldct_initial()
  
  update_user(message = 'Metric tables loaded')
})


# calculate pivot tables -------------------------------------------------------

## udf -------------------------------------------------------------------------
func_summarise_by_project_gender <- function(df) {
  return(
    df |> 
      mutate(calc_sex = replace_na(calc_sex, 'Not known')) |> # ensure all genders are labelled
      group_by(project, calc_sex) |> 
      summarise(participants = n_distinct(ParticipantID), .groups = 'drop') |> # count participants by group
      ungroup() |> 
      complete(project, calc_sex) |> # ensure all projects have a full distribution of genders
      ungroup() |> 
      pivot_wider(
        names_from = calc_sex,
        values_from = participants
      ) |> 
      adorn_totals(where = 'col') |> # add a total column 
      adorn_totals(where = 'row') # add a total row
  )
}

func_summarise_by_project_age <- function(df) {
  return(
    df |> 
      mutate(calc_age_group_report = replace_na(calc_age_group_report, 'Not known')) |> # ensure all age groups are labelled
      group_by(project, calc_age_group_report) |> 
      summarise(participants = n_distinct(ParticipantID), .groups = 'drop') |> # count participants by group
      ungroup() |> 
      complete(project, calc_age_group_report) |> # ensure all projects have a full distribution of age groups
      ungroup() |> 
      pivot_wider(
        names_from = calc_age_group_report,
        values_from = participants
      ) |> 
      adorn_totals(where = 'col') |> # add a total column 
      adorn_totals(where = 'row') # add a total row
  )
}

func_summarise_by_project_deprivation <- function(df) {
  return(
    df |> 
      #mutate(calc_lsoa_imd_decile = replace_na(calc_lsoa_imd_decile, 'Not known')) |> # ensure all deciles are labelled
      mutate(
        calc_lsoa_imd_decile = factor(x = calc_lsoa_imd_decile),
        calc_lsoa_imd_decile = fct_na_value_to_level(calc_lsoa_imd_decile, level = 'Not known')
      ) |> 
      group_by(project, calc_lsoa_imd_decile) |> 
      summarise(participants = n_distinct(ParticipantID), .groups = 'drop') |> # count participants by group
      ungroup() |> 
      complete(project, calc_lsoa_imd_decile) |> # ensure all projects have a full distribution of imd
      ungroup() |> 
      pivot_wider(
        names_from = calc_lsoa_imd_decile,
        values_from = participants
      ) |> 
      adorn_totals(where = 'col') |> # add a total column
      adorn_totals(where = 'row') # add a total row
  )
}

func_summarise_by_project_ethnicity <- function(df) {
  return(
    df |> 
      mutate(calc_ethnic_group = replace_na(calc_ethnic_group, 'Not known')) |> # ensure all ethnicities are labelled
      group_by(project, calc_ethnic_group) |> 
      summarise(participants = n_distinct(ParticipantID), .groups = 'drop') |> # count participants by group
      ungroup() |> 
      complete(project, calc_ethnic_group) |> # ensure all projects have a full distribution of ethnicity
      ungroup() |> 
      pivot_wider(
        names_from = calc_ethnic_group,
        values_from = participants
      ) |> 
      adorn_totals(where = 'col') |> # add a total column
      adorn_totals(where = 'row') # add a total row
  )
}

func_summarise_overview <- function(df, str_metric_name) {
  return(
    df |> 
      mutate(
        calc_sex = replace_na(calc_sex, 'Not known'),
        calc_age_group_report = replace_na(calc_age_group_report, 'Not known'),
        calc_ethnic_group = replace_na(calc_ethnic_group, 'Not known'),
        #calc_lsoa_imd_decile = replace_na(calc_lsoa_imd_decile, 'Not known'),
        metric_name = str_metric_name
      ) |> 
      group_by(calc_sex, calc_age_group_report, calc_ethnic_group, calc_lsoa_imd_decile, metric_name) |> 
      summarise(participants = n_distinct(ParticipantID), .groups = 'drop') |> 
      ungroup()
  )
}

#' Flatfile output - summarise project demographic
#'
#' @param df Tibble of metric data to be aggregated
#' @param str_demo String name / description of the demographic variable to aggregate by
#' @param var_demo Object demographic variable to be aggregated against
#'
#' @return Tibble of unique participants per project and demographic value
func_flatfile_summarise_project_demographic <- function(df, str_demo, var_demo) {
  
  var_demo <- enquo(var_demo)
  
  return(
    df |>
      mutate(
        demographic_value = !!var_demo,                                            # alias the demographic field
        demographic = str_demo,                                                    # add in the demographic type
        demographic_value = factor(x = demographic_value),                         # convert the demographic to a factor
        #demographic_value = replace_na(demographic_value, 'Not known')             # ensure all NAs are coded
        demographic_value = fct_na_value_to_level(demographic_value, level = 'Not known') # explicitly label NAs
      ) |> 
      group_by(project, demographic, demographic_value, metric_id, metric_name) |> # group by project, variable and metric info
      summarise(participants = n_distinct(ParticipantID), .groups = 'drop') |>     # count participants
      unique() |>                                                                  # remove duplicates
      complete(project, demographic, demographic_value, metric_id, metric_name) |> # ensure all combinations of project & demo value
      filter(!demographic_value == 'Age below zero') |>                            # filter out unwanted categories
      select(
        project,
        metric_id,
        metric_name,
        demographic,
        demographic_value,
        participants
      )                                                                            # select output values
  )
}

# testing only ---
# df_test <- func_flatfile_summarise_project_demographic(
#   df = df_metric_1a_invites_first,
#   str_demo = 'Deprivation',
#   var_demo = calc_lsoa_imd_decile
# )
# df_test <- func_flatfile_summarise_project_demographic(
#   df = df_metric_1a_invites_first,
#   str_demo = 'Gender',
#   var_demo = calc_sex
# )


## overview --------------------------------------------------------------------

# pre-aggregate datasets into a single df
df_metrics_overview <- bind_rows(
  func_summarise_overview(df = df_metric_1a_invites_first, str_metric_name = 'Invited to a Targeted Lung Health Check'),
  func_summarise_overview(df = df_metric_3ab_attend, str_metric_name = 'Attended a Lung Health Check appointment'),
  func_summarise_overview(df = df_metric_4a_ldct_referral, str_metric_name = 'Referred for a Low Dose CT Scan (LDCT)'),
  func_summarise_overview(df = df_metric_5a_ldct_initial, str_metric_name = 'Initial LDCT scan performed')
) |> 
  mutate(
    metric_name = factor(
      metric_name,
      levels = c(
        'Invited to a Targeted Lung Health Check',
        'Attended a Lung Health Check appointment',
        'Referred for a Low Dose CT Scan (LDCT)',
        'Initial LDCT scan performed'
      )
    )
  )

### gender and age ----
# calculate summaries by metric, gender and age
df_overview_metric_gender_age <- df_metrics_overview |> 
  group_by(metric_name, calc_sex, calc_age_group_report) |> 
  summarise(participants = sum(participants, na.rm = T)) 

# calculate summaries by metric and gender (will be the subtotals)
df_overview_metric_gender_age_subtotals <- df_metrics_overview |> 
  group_by(metric_name, calc_sex) |> 
  summarise(participants = sum(participants, na.rm = T)) |> 
  mutate(calc_age_group_report = 'Total')

# calculate summarise by metric (will be the total)
df_overview_metric_gender_age_total <- df_metrics_overview |> 
  group_by(metric_name) |> 
  summarise(participants = sum(participants, na.rm = T)) |> 
  mutate(calc_sex = 'Total')

# add the tables together and pivot wider
df_overview_metric_gender_age <- bind_rows(
  df_overview_metric_gender_age,
  df_overview_metric_gender_age_subtotals,
  df_overview_metric_gender_age_total
) |> 
  arrange(calc_sex, calc_age_group_report) |>
  pivot_wider(
    names_from = metric_name,
    values_from = participants
  )


### ethnicity ----
df_overview_metric_ethnicity <- df_metrics_overview |> 
  group_by(metric_name, calc_ethnic_group) |> 
  summarise(participants = sum(participants, na.rm = T)) |> 
  pivot_wider(
    names_from = metric_name,
    values_from = participants
  ) |> 
  adorn_totals(where = 'row')


### deprivation ----
df_overview_metric_deprivation <- df_metrics_overview |> 
  group_by(metric_name, calc_lsoa_imd_decile) |> 
  summarise(participants = sum(participants, na.rm = T)) |> 
  pivot_wider(
    names_from = metric_name,
    values_from = participants
  ) |> 
  adorn_totals(where = 'row')


## invites ---------------------------------------------------------------------

df_invites_project_gender <- func_summarise_by_project_gender(df = df_metric_1a_invites_first)
df_invites_project_age <- func_summarise_by_project_age(df = df_metric_1a_invites_first)
df_invites_project_deprivation <- func_summarise_by_project_deprivation(df = df_metric_1a_invites_first)
df_invites_project_ethnicity <- func_summarise_by_project_ethnicity(df = df_metric_1a_invites_first)

## lhc attendance --------------------------------------------------------------

df_lhc_attendance_project_gender <- func_summarise_by_project_gender(df = df_metric_3ab_attend)
df_lhc_attendance_project_age <- func_summarise_by_project_age(df = df_metric_3ab_attend)
df_lhc_attendance_project_deprivation <- func_summarise_by_project_deprivation(df = df_metric_3ab_attend)
df_lhc_attendance_project_ethnicity <- func_summarise_by_project_ethnicity(df = df_metric_3ab_attend)

## ldct referral ---------------------------------------------------------------

df_ldct_referral_project_gender <- func_summarise_by_project_gender(df = df_metric_4a_ldct_referral)
df_ldct_referral_project_age <- func_summarise_by_project_age(df = df_metric_4a_ldct_referral)
df_ldct_referral_project_deprivation <- func_summarise_by_project_deprivation(df = df_metric_4a_ldct_referral)
df_ldct_referral_project_ethnicity <- func_summarise_by_project_ethnicity(df = df_metric_4a_ldct_referral)

## ldct initial ----------------------------------------------------------------

df_ldct_initial_project_gender <- func_summarise_by_project_gender(df = df_metric_5a_ldct_initial)
df_ldct_initial_project_age <- func_summarise_by_project_age(df = df_metric_5a_ldct_initial)
df_ldct_initial_project_deprivation <- func_summarise_by_project_deprivation(df = df_metric_5a_ldct_initial)
df_ldct_initial_project_ethnicity <- func_summarise_by_project_ethnicity(df = df_metric_5a_ldct_initial)

# Outputs ----------------------------------------------------------------------
# create a list of objects to output to Excel
outputs <- list(
  
  overview_gender_age = df_overview_metric_gender_age,
  overview_ethnicity = df_overview_metric_ethnicity,
  overview_deprivation = df_overview_metric_deprivation,
  
  invites_gender = df_invites_project_gender,
  invites_age = df_invites_project_age,
  invites_deprivation = df_invites_project_deprivation,
  invites_ethncity = df_invites_project_ethnicity,
  
  attend_gender = df_lhc_attendance_project_gender,
  attend_age = df_lhc_attendance_project_age,
  attend_deprivation = df_lhc_attendance_project_deprivation,
  attend_ethnicity = df_lhc_attendance_project_ethnicity,
  
  refer_gender = df_ldct_referral_project_gender,
  refer_age = df_ldct_referral_project_age,
  refer_deprivation = df_ldct_referral_project_deprivation,
  refer_ethnicity = df_ldct_referral_project_ethnicity,
  
  ldct_gender = df_ldct_initial_project_gender,
  ldct_age = df_ldct_initial_project_age,
  ldct_deprivation = df_ldct_initial_project_deprivation,
  ldct_ethnicity = df_ldct_initial_project_ethnicity
  
)

# save as an Excel file for copying to the template
write_xlsx(
  x = outputs,
  path = here('outputs', paste0('tlhc_demographics_', today(), '.xlsx')),
  col_names = T
)



# Flat file extract ------------------------------------------------------------
# Louise Reynolds requested a long-format file covering demographic outputs to
# help her process the data within NHSE.
# The specification is:
# lung metric          - the name of the metric
# project              - the name of the project
# demographic metric   - the name of the demographic (age, gender, ethnicity or deprivation)
# demographic category - the value of the demographic (50-59, 60-69, Male, Black or Black British)
# n                    - the count of patients


# give the lhc attendance df a single metric and name
df_metric_3ab_attend <- df_metric_3ab_attend |> 
  mutate(
    metric_id = '3ab',
    metric_name = 'Number of patients who attended a Lung Health Check'
  )

# build the flat-file output
df_demographic_flatfile <- bind_rows(
  
  ## invites ----
  func_flatfile_summarise_project_demographic(
    df = df_metric_1a_invites_first,
    str_demo = 'Age group',
    var_demo = calc_age_group_report
  ),
  
  func_flatfile_summarise_project_demographic(
    df = df_metric_1a_invites_first,
    str_demo = 'Gender',
    var_demo = calc_sex
  ),
  
  func_flatfile_summarise_project_demographic(
    df = df_metric_1a_invites_first,
    str_demo = 'Ethnic group',
    var_demo = calc_ethnic_group
  ),
  
  func_flatfile_summarise_project_demographic(
    df = df_metric_1a_invites_first,
    str_demo = 'Deprivation',
    var_demo = calc_lsoa_imd_decile
  ),
  
  ## lhc attendances ----
  func_flatfile_summarise_project_demographic(
    df = df_metric_3ab_attend,
    str_demo = 'Age group',
    var_demo = calc_age_group_report
  ),
  
  func_flatfile_summarise_project_demographic(
    df = df_metric_3ab_attend,
    str_demo = 'Gender',
    var_demo = calc_sex
  ),
  
  func_flatfile_summarise_project_demographic(
    df = df_metric_3ab_attend,
    str_demo = 'Ethnic group',
    var_demo = calc_ethnic_group
  ),
  
  func_flatfile_summarise_project_demographic(
    df = df_metric_3ab_attend,
    str_demo = 'Deprivation',
    var_demo = calc_lsoa_imd_decile
  ),
  
  ## ldct referrals ----
  func_flatfile_summarise_project_demographic(
    df = df_metric_4a_ldct_referral,
    str_demo = 'Age group',
    var_demo = calc_age_group_report
  ),
  
  func_flatfile_summarise_project_demographic(
    df = df_metric_4a_ldct_referral,
    str_demo = 'Gender',
    var_demo = calc_sex
  ),
  
  func_flatfile_summarise_project_demographic(
    df = df_metric_4a_ldct_referral,
    str_demo = 'Ethnic group',
    var_demo = calc_ethnic_group
  ),
  
  func_flatfile_summarise_project_demographic(
    df = df_metric_4a_ldct_referral,
    str_demo = 'Deprivation',
    var_demo = calc_lsoa_imd_decile
  ),
  
  ## ldct initial ----
  func_flatfile_summarise_project_demographic(
    df = df_metric_5a_ldct_initial,
    str_demo = 'Age group',
    var_demo = calc_age_group_report
  ),
  
  func_flatfile_summarise_project_demographic(
    df = df_metric_5a_ldct_initial,
    str_demo = 'Gender',
    var_demo = calc_sex
  ),
  
  func_flatfile_summarise_project_demographic(
    df = df_metric_5a_ldct_initial,
    str_demo = 'Ethnic group',
    var_demo = calc_ethnic_group
  ),
  
  func_flatfile_summarise_project_demographic(
    df = df_metric_5a_ldct_initial,
    str_demo = 'Deprivation',
    var_demo = calc_lsoa_imd_decile
  )
)


# test <- func_flatfile_summarise_project_demographic(
#   df = df_metric_1a_invites_first,
#   str_demo = 'Deprivation',
#   var_demo = calc_lsoa_imd_decile
# )

# test <- func_flatfile_summarise_project_demographic(
#   df = df_metric_1a_invites_first,
#   str_demo = 'Deprivation',
#   var_demo = calc_lsoa_imd_decile
# )

## output the flatfile for sending ----
# Louise stated she would like it as a .csv:
write_csv(
  x = df_demographic_flatfile,
  file = here('outputs', paste0('tlhc_demographics_flatfile_', today(), '.csv'))
)

# writing as .rds in case we can use it elsewhere
write_rds(
  x = df_demographic_flatfile,
  file = here('data', 'tlhc', paste0('tlhc_demographics_flatfile_', today(), '.Rds'))
)


# done!
update_user(icon = '🔚', stage = 'end')
toc()