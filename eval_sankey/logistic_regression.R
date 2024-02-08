#' -----------------------------------------------------------------------------
#' LOGISTIC REGRESSION
#' 
#' Using regression to enumerate the level of impact each explanatory factor has
#' on the likelihood of a) lhc attendance, b) ct scan, c) (eventually) cancer 
#' diagnosis.
#' -----------------------------------------------------------------------------

# libraries
library(tidyverse)       # tidy data wrangling
library(finalfit)        # lr and or outputs
library(here)            # localised file paths
library(kableExtra)

# setup
base_out <- 'C:/Users/craig.parylo/Midlands and Lancashire CSU/Strategy Unit Team - Strategy Unit/Projects/Current/610 Targeted Lung Health Checks Evaluation/Reports/2024-01/sankey_charts'


# temp - add in admin ----------------------------------------------------------
# df_sankey <- readRDS(file = here('eval_sankey', 'df_sankey_preagg.Rds'))
# 
# # add the in-house / outsource details 
# # NB, this is temporary and will be replaced by a proper process
# df_project_ref <- readxl::read_excel(path = here('data', 'reference', 'project_reference.xlsx')) |> 
#   select(project, admin)
# 
# df_sankey <- df_sankey |> 
#   left_join(
#     y = df_project_ref,
#     by = 'project'
#   )
# 
# # save this file
# saveRDS(
#   object = df_sankey,
#   file = here('eval_sankey', 'df_sankey_preagg.Rds')
# )


# data -------------------------------------------------------------------------
df_sankey <- readRDS(file = here('eval_sankey', 'df_sankey_preagg.Rds')) |> 
  # remove phase 3 projects
  filter(!phase == 'Phase 3') |> 
  # tidy up data to ensure they are presented as sensible options in the pickers
  mutate(
    phase = fct(x = phase, levels = c('Original', 'Onboarded')),
    
    calc_lsoa_imd_decile = fct(x = as.character(calc_lsoa_imd_decile), levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10')),
    calc_lsoa_imd_decile = fct_na_value_to_level(f = calc_lsoa_imd_decile, level = 'Not known'),
    
    calc_lsoa_rurality_group_category = fct(x = calc_lsoa_rurality_group_category, levels = c('Rural', 'Urban')),
    calc_lsoa_rurality_group_category = fct_na_value_to_level(f = calc_lsoa_rurality_group_category, level = 'Not known'),
    
    triage_before_risk_assessment = fct(x = triage_before_risk_assessment, levels = c('Yes', 'No', 'Unknown')),
    
    # NB, already a factor, so just code NAs as a level
    calc_ethnic_group = fct_na_value_to_level(f = calc_ethnic_group, level = 'Not known'),
    
    calc_sex = fct(x = calc_sex, levels = c('Female', 'Male', 'Not known')),
    calc_sex = fct_na_value_to_level(f = calc_sex, level = 'Not known'),
    
    # NB, already a factor, just relevelling and coding NAs as a level
    calc_age_group_ipsos = fct_collapse(.f = calc_age_group_ipsos, Other = c('Other', 'Age below zero')), # group age<0 with other
    calc_age_group_ipsos = fct_relevel(.f = calc_age_group_ipsos, '55-64', '65-74', '75', 'Other'), # move other to end
    calc_age_group_ipsos = fct_na_value_to_level(f = calc_age_group_ipsos, level = 'Not known')
  ) |> 
  # expand to one row per participant
  uncount(weights = participants) |> 
  # define dependent variables
  mutate(
    flag_lhc = case_when(
      calc_lhc_attendance_category_overall == 'LHC Attended' ~ 'LHC Attended',
      .default = 'No attendance'
    ) |> 
      fct(levels = c('No attendance', 'LHC Attended')) |> 
      ff_label('Attendance at LHC'),
    flag_ldct = case_when(
      !is.na(calc_ldct_count_groups) ~ 'LDCT scan',
      .default = 'No scan'
    ) |> 
      fct(levels = c('No scan', 'LDCT scan')) |> 
      ff_label('Receive LDCT scan')
  )



# UDF --------------------------------------------------------------------------

#' Carry out the FinalFit logistic regression
#' 
#' For a given df and set of dependent and explanatory variables conduct 
#' logistic regression analyses to output the summary tables and odds ratio
#' plot
#'
#' @param df Tibble - df_sankey
#' @param dependent a character vector of dependent (outcome) variables
#' @param explanatory a character vector of independent (explanatory) variables
#'
#' @examples do_ff_logistic_regression(df = df_sankey_temp, dependent = 'flag_lhc', explanatory = c('var_invite_mode', 'var_age'))
do_ff_logistic_regression <- function(df, dependent, explanatory) {
  
  # produce the summary table
  df_ff <- df |> 
    finalfit(dependent = dependent, explanatory = explanatory, metrics = T) |> 
    kbl()
  
  # produce the or plot
  svg(filename = file.path(
    base_out, paste0('lr_', paste0(explanatory, collapse = '_'), '_', paste0(dependent, collapse = '_'), '.svg')
    ), width = 10, height = 2)
  df_sankey_temp |> 
    or_plot(dependent = dependent, explanatory = explanatory)
  dev.off() 
  
  # return the table
  return(df_ff)
}

# opt-in/opt-out ---------------------------------------------------------------
df_sankey_temp <- df_sankey |> 
  # filter for only opt-in / out
  filter(invite_mode %in% c('Opt-in', 'Opt-out')) |> 
  # define independent variables
  mutate(
    var_invite_mode = fct(x = as.character(invite_mode), levels = c('Opt-in', 'Opt-out')) |>
      ff_label('Invite mode'),
  )

dependent <- c('flag_lhc')
explanatory <- c('var_invite_mode')

## LHC ---
df_sankey_temp |> 
  finalfit(dependent = dependent, explanatory = explanatory, metrics = T) |> 
  kbl()

svg(filename = here('outputs', 'opt_inout_lhc.svg'), width = 8, height = 2)
df_sankey_temp |> 
  or_plot(dependent = dependent, explanatory = explanatory)
dev.off()

## LDCT ---
dependent <- c('flag_ldct')

df_sankey_temp |> 
  finalfit(dependent = dependent, explanatory = explanatory, metrics = T)) |> 
  kbl()

svg(filename = here('outputs', 'opt_inout_ldct.svg'), width = 8, height = 2)
df_sankey_temp |> 
  or_plot(dependent = dependent, explanatory = explanatory)
dev.off()

# combined/optinout ---------------------------------------------------------------
df_sankey_temp <- df_sankey |> 
  # filter for only opt-in / out
  filter(invite_mode %in% c('Combined', 'Opt-in', 'Opt-out')) |> 
  # define independent variables
  mutate(
    var_invite_mode = case_when(
      invite_mode == 'Combined' ~ 'Combined',
      .default = 'Opt-in & Opt-out'
    ) |> fct(levels = c('Combined', 'Opt-in & Opt-out')) |>
      ff_label('Invite mode'),
  )

dependent <- c('flag_lhc')
explanatory <- c('var_invite_mode')

## LHC ---
df_sankey_temp |> 
  finalfit(dependent = dependent, explanatory = explanatory, metrics = T) |> 
  kbl()

svg(filename = file.path(base_out, 'lr_opt_inoutcombined_lhc.svg'), width = 8, height = 2)
df_sankey_temp |> 
  or_plot(dependent = dependent, explanatory = explanatory)
dev.off()

## LDCT ---
dependent <- c('flag_ldct')

df_sankey_temp |> 
  finalfit(dependent = dependent, explanatory = explanatory, metrics = T) |> 
  kbl()

svg(filename = file.path(base_out, 'lr_opt_inoutcombined_ldct.svg'), width = 8, height = 2)
df_sankey_temp |> 
  or_plot(dependent = dependent, explanatory = explanatory)
dev.off()


# triage ---------------------------------------------------------------
df_sankey_temp <- df_sankey |> 
  # filter for only opt-in / out
  filter(triage_before_risk_assessment %in% c('Yes', 'No')) |> 
  # define independent variables
  mutate(
    var_triage = as.character(triage_before_risk_assessment) |> 
      fct(levels = c('No', 'Yes')) |> 
      ff_label('Triage before risk assessment')
  )

dependent <- c('flag_lhc')
explanatory <- c('var_triage')

## LHC ---
df_sankey_temp |> 
  finalfit(dependent = dependent, explanatory = explanatory, metrics = T) |> 
  kbl()

svg(filename = file.path(base_out, paste0('lr_', explanatory, '_lhc.svg')), width = 9, height = 2)
df_sankey_temp |> 
  or_plot(dependent = dependent, explanatory = explanatory)
dev.off()

## LDCT ---
dependent <- c('flag_ldct')

df_sankey_temp |> 
  finalfit(dependent = dependent, explanatory = explanatory, metrics = T) |> 
  kbl()

svg(filename = file.path(base_out, paste0('lr_', explanatory, '_ldct.svg')), width = 9, height = 2)
df_sankey_temp |> 
  or_plot(dependent = dependent, explanatory = explanatory)
dev.off()


# triage ---------------------------------------------------------------
df_sankey_temp <- df_sankey |> 
  # filter for only opt-in / out
  filter(lhc_delivery %in% c('Face-to-face', 'Virtual')) |> 
  # define independent variables
  mutate(
    var_delivery = as.character(lhc_delivery) |> 
      fct(levels = c('Virtual', 'Face-to-face')) |> 
      ff_label('LHC delivery')
  )

dependent <- c('flag_lhc')
explanatory <- c('var_delivery')

## LHC ---
df_sankey_temp |> 
  finalfit(dependent = dependent, explanatory = explanatory, metrics = T) |> 
  kbl()

svg(filename = file.path(base_out, paste0('lr_', explanatory, '_lhc.svg')), width = 9, height = 2)
df_sankey_temp |> 
  or_plot(dependent = dependent, explanatory = explanatory)
dev.off()

## LDCT ---
dependent <- c('flag_ldct')

df_sankey_temp |> 
  finalfit(dependent = dependent, explanatory = explanatory, metrics = T) |> 
  kbl()

svg(filename = file.path(base_out, paste0('lr_', explanatory, '_ldct.svg')), width = 9, height = 2)
df_sankey_temp |> 
  or_plot(dependent = dependent, explanatory = explanatory)
dev.off()

# deprivation ---------------------------------------------------------------
df_sankey_temp <- df_sankey |> 
  # filter for only opt-in / out
  filter(calc_lsoa_imd_decile %in% c(1, 2, 5, 6, 7, 8, 9, 10)) |> 
  # define independent variables
  mutate(
    var_imd = case_when(
      calc_lsoa_imd_decile %in% c(1, 2) ~ 'Quintile 1',
      .default = 'Quintiles 3-5'
    ) |>  
      fct(levels = c('Quintiles 3-5', 'Quintile 1')) |> 
      ff_label('Deprivation')
  )

dependent <- c('flag_lhc')
explanatory <- c('var_imd')

## LHC ---
df_sankey_temp |> 
  finalfit(dependent = dependent, explanatory = explanatory, metrics = T) |> 
  kbl()

svg(filename = file.path(base_out, paste0('lr_', explanatory, '_lhc.svg')), width = 9, height = 2)
df_sankey_temp |> 
  or_plot(dependent = dependent, explanatory = explanatory)
dev.off()

## LDCT ---
dependent <- c('flag_ldct')

df_sankey_temp |> 
  finalfit(dependent = dependent, explanatory = explanatory, metrics = T) |> 
  kbl()

svg(filename = file.path(base_out, paste0('lr_', explanatory, '_ldct.svg')), width = 9, height = 2)
df_sankey_temp |> 
  or_plot(dependent = dependent, explanatory = explanatory)
dev.off()


# age ---------------------------------------------------------------
df_sankey_temp <- df_sankey |> 
  # filter for only opt-in / out
  filter(calc_age_group_ipsos %in% c('55-64', '65-74')) |> 
  # define independent variables
  mutate(
    var_age = as.character(calc_age_group_ipsos) |>  
      fct(levels = c('55-64', '65-74')) |> 
      ff_label('Age group')
  )

dependent <- c('flag_lhc')
explanatory <- c('var_age')

## LHC ---
df_sankey_temp |> 
  finalfit(dependent = dependent, explanatory = explanatory, metrics = T) |> 
  kbl()

svg(filename = file.path(base_out, paste0('lr_', explanatory, '_lhc.svg')), width = 9, height = 2)
df_sankey_temp |> 
  or_plot(dependent = dependent, explanatory = explanatory)
dev.off()

## LDCT ---
dependent <- c('flag_ldct')

df_sankey_temp |> 
  finalfit(dependent = dependent, explanatory = explanatory, metrics = T) |> 
  kbl()

svg(filename = file.path(base_out, paste0('lr_', explanatory, '_ldct.svg')), width = 9, height = 2)
df_sankey_temp |> 
  or_plot(dependent = dependent, explanatory = explanatory)
dev.off()

# gender ---------------------------------------------------------------
df_sankey_temp <- df_sankey |> 
  # filter for only opt-in / out
  filter(calc_sex %in% c('Female', 'Male')) |> 
  # define independent variables
  mutate(
    var_gender = as.character(calc_sex) |>  
      fct(levels = c('Female', 'Male')) |> 
      ff_label('Gender')
  )

dependent <- c('flag_lhc')
explanatory <- c('var_gender')

## LHC ---
df_sankey_temp |> 
  finalfit(dependent = dependent, explanatory = explanatory, metrics = T) |> 
  kbl()

svg(filename = file.path(base_out, paste0('lr_', explanatory, '_lhc.svg')), width = 9, height = 2)
df_sankey_temp |> 
  or_plot(dependent = dependent, explanatory = explanatory)
dev.off()

## LDCT ---
dependent <- c('flag_ldct')

df_sankey_temp |> 
  finalfit(dependent = dependent, explanatory = explanatory, metrics = T) |> 
  kbl()

svg(filename = file.path(base_out, paste0('lr_', explanatory, '_ldct.svg')), width = 9, height = 2)
df_sankey_temp |> 
  or_plot(dependent = dependent, explanatory = explanatory)
dev.off()

# ethnicity ---------------------------------------------------------------
df_sankey_temp <- df_sankey |> 
  # filter for only opt-in / out
  filter(calc_ethnic_group %in% c('Asian or Asian British', 'Black or Black British', 'Mixed', 'Not stated', 'Other Ethnic Group', 'White')) |> 
  # define independent variables
  mutate(
    var_ethnicity = case_when(
      calc_ethnic_group == 'White' ~ 'White',
      .default = 'Other ethnicities'
    ) |> 
      as.character() |>  
      fct(levels = c('Other ethnicities', 'White')) |> 
      ff_label('Broad ethnic group')
  )

dependent <- c('flag_lhc')
explanatory <- c('var_ethnicity')

# LHC
do_ff_logistic_regression(df = df_sankey_temp, dependent = dependent, explanatory = explanatory)

# LDCT
dependent <- c('flag_ldct')
do_ff_logistic_regression(df = df_sankey_temp, dependent = dependent, explanatory = explanatory)

# (opt-in/out) vs (deprivation) ------------------------------------------------
df_sankey_temp <- df_sankey |> 
  # filter for only opt-in / out
  filter(
    invite_mode %in% c('Opt-in', 'Opt-out'),
    calc_lsoa_imd_decile %in% c(1, 2, 5, 6, 7, 8, 9, 10)
  ) |> 
  # define independent variables
  mutate(
    var_invite_mode = invite_mode |> 
      as.character() |> 
      fct(levels = c('Opt-in', 'Opt-out')) |> 
      ff_label('Invite mode'),
    var_imd = case_when(
      calc_lsoa_imd_decile %in% c(1, 2) ~ 'Quintile 1',
      .default = 'Quintiles 3-5'
    ) |> 
      fct(levels = c('Quintiles 3-5', 'Quintile 1')) |> 
      ff_label('Deprivation')
  )
      
dependent <- c('flag_lhc')
explanatory <- c('var_invite_mode', 'var_imd')

# LHC
do_ff_logistic_regression(df = df_sankey_temp, dependent = dependent, explanatory = explanatory)

# LDCT
dependent <- c('flag_ldct')
do_ff_logistic_regression(df = df_sankey_temp, dependent = dependent, explanatory = explanatory)   
      

# (opt-in/out) vs (age) ------------------------------------------------
df_sankey_temp <- df_sankey |> 
  # filter for only opt-in / out
  filter(
    invite_mode %in% c('Opt-in', 'Opt-out'),
    calc_age_group_ipsos %in% c('55-64', '65-74')
  ) |> 
  # define independent variables
  mutate(
    var_invite_mode = invite_mode |> 
      as.character() |> 
      fct(levels = c('Opt-in', 'Opt-out')) |> 
      ff_label('Invite mode'),
    var_age = as.character(calc_age_group_ipsos) |>  
      fct(levels = c('55-64', '65-74')) |> 
      ff_label('Age group')
  )

dependent <- c('flag_lhc')
explanatory <- c('var_invite_mode', 'var_age')

# LHC
do_ff_logistic_regression(df = df_sankey_temp, dependent = dependent, explanatory = explanatory)

# LDCT
dependent <- c('flag_ldct')
do_ff_logistic_regression(df = df_sankey_temp, dependent = dependent, explanatory = explanatory)   

# admin ------------------------------------------------------------------------
df_sankey_temp <- df_sankey |> 
  filter(
    admin %in% c('In-house', 'Outsourced')
  ) |> 
  # define independent variables
  mutate(
    var_admin = admin |> 
      as.character() |> 
      fct(levels = c('In-house', 'Outsourced')) |> 
      ff_label('Admin')
  )

dependent <- c('flag_lhc')
explanatory <- c('var_admin')

# LHC
do_ff_logistic_regression(df = df_sankey_temp, dependent = dependent, explanatory = explanatory)

# LDCT
dependent <- c('flag_ldct')
do_ff_logistic_regression(df = df_sankey_temp, dependent = dependent, explanatory = explanatory)   
