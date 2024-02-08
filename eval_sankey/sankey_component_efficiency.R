#' -----------------------------------------------------------------------------
#' SANKEY COMPONENT EFFICIENCY
#' 
#' Work out the efficiency of key milestones, e.g. how many lhc_attenance_cateogry
#' are 'LHC attended', to be used as an indicator for how efficient a step is in
#' producing the 'right' output, that is more people in the flow towards a ldct.
#' -----------------------------------------------------------------------------

# libraries
library(tidyverse)    # tidy data wrangling
library(here)         # localised file paths
library(formattable)  # format float numbers as percentages and keep numeric value
library(rlang)        # handling fields as parameters

# read the data
df_sankey <- readRDS(file = here('eval_sankey', 'df_sankey_preagg.Rds')) |> 
  filter(!phase == 'Phase 3')


# udf --------------------------------------------------------------------------

ct_conversion <- function(df) {
  df |> 
    # count the number of participants who 
    summarise(
      lhc_attended = sum(participants[calc_lhc_attendance_category_overall == 'LHC Attended'], na.rm = T),
      ct_scan = sum(participants[!is.na(calc_ldct_count_groups)], na.rm = T)
    ) |> 
    mutate(
      ct_conversion = percent(ct_scan / lhc_attended, digits = 1),
      lhc_attended = prettyunits::pretty_num(lhc_attended),
      ct_scan = prettyunits::pretty_num(ct_scan)
    )
}

lhc_conversion <- function(df) {
  df |> 
    # count the number of participants who 
    summarise(
      invited = sum(participants, na.rm = T),
      lhc_attended = sum(participants[calc_lhc_attendance_category_overall == 'LHC Attended'], na.rm = T),
    ) |> 
    mutate(
      lhc_conversion = percent(lhc_attended / invited / , digits = 1),
      invited = prettyunits::pretty_num(invited),
      lhc_attended = prettyunits::pretty_num(lhc_attended)
    )
}


#' Individual milestone efficiency
#'
#' @param df Tibble of data to use
#' @param field The field to be summarised
#' @param milestone A description of the milestone
#'
individual_efficiency <- function(df, field, milestone) {
  
  return(
    df |> 
      filter(!is.na({{field}})) |> 
      mutate(n_total = sum(participants, na.rm = T)) |> 
      group_by({{field}}) |> 
      summarise(
        n = sum(participants, na.rm = T),
        n_perc = percent(n / max(n_total), digits = 1)
      ) |> 
      ungroup() |> 
      mutate(
        milestone = milestone
      ) |> 
      rename(response := {{field}})
  )
  
}

#' Show the efficiency of the process using sankey milestones
#'
#' @param df Tibble - a pre-sankey dataframe, already filtered for the desired elements
#'
show_sankey_efficiency <- function(df = df_sankey) {
  
  return(
    bind_rows(
      
      # invite outcome
      individual_efficiency(df = df, field = calc_invite_outcome, milestone = 'invite'),
      
      # lhc attendance
      individual_efficiency(df = df, field = calc_lhc_attendance_category_overall, milestone = 'lhc'),
      
      # risk score
      individual_efficiency(df = df, field = calc_risk_assessment, milestone = 'risk'),
      
      # scan (needs to be different as calculating inverse of na)
      df |> 
        mutate(
          n_total = sum(participants, na.rm = T),
          scanned = if_else(is.na(calc_ldct_count_groups), 'No scan', 'Scanned')
        ) |> 
        group_by(scanned) |> 
        summarise(
          n = sum(participants, na.rm = T),
          n_perc = percent(n / max(n_total), digits = 1)
        ) |> 
        ungroup() |> 
        mutate(
          milestone = 'scan'
        ) |> 
        rename(response := scanned)
      
    ) |> 
      #convert milestones to factors for ease of filtering
      mutate(
        milestone = fct(
          x = milestone,
          levels = c('invite', 'lhc', 'risk', 'scan')
        )
      )
  )
}

# produce outputs
ef_overall <- show_sankey_efficiency(df = df_sankey)
ef_opt_in <- show_sankey_efficiency(df = df_sankey |> filter(invite_mode == 'Opt-in'))
ef_opt_out <- show_sankey_efficiency(df = df_sankey |> filter(invite_mode == 'Opt-out'))
ef_opt_combined <- show_sankey_efficiency(df = df_sankey |> filter(invite_mode == 'Combined'))
ef_opt_in_out <- show_sankey_efficiency(df = df_sankey |> filter(invite_mode %in% c('Opt-in', 'Opt-out')))
ef_triage_yes <- show_sankey_efficiency(df = df_sankey |> filter(triage_before_risk_assessment == 'Yes'))
ef_triage_no <- show_sankey_efficiency(df = df_sankey |> filter(triage_before_risk_assessment == 'No'))
ef_delivery_f2f <- show_sankey_efficiency(df = df_sankey |> filter(project %in% c('Tameside and Glossop')))
ef_delivery_virtual <- show_sankey_efficiency(df = df_sankey |> filter(!project %in% c('Tameside and Glossop', 'Southampton')))
ef_admin_inhouse <- show_sankey_efficiency(df = df_sankey |> filter(admin == 'In-house'))
ef_admin_outsourced <- show_sankey_efficiency(df = df_sankey |> filter(admin == 'Outsourced'))
ef_imd_1 <- show_sankey_efficiency(df = df_sankey |> filter(calc_lsoa_imd_decile %in% c(1,2)))
ef_imd_35 <- show_sankey_efficiency(df = df_sankey |> filter(calc_lsoa_imd_decile %in% c(5, 6, 7, 8, 9, 10)))
ef_age_55_64 <- show_sankey_efficiency(df = df_sankey |> filter(calc_age_group_ipsos %in% c('55-64')))
ef_age_65_74 <- show_sankey_efficiency(df = df_sankey |> filter(calc_age_group_ipsos %in% c('65-74')))
ef_sex_female <- show_sankey_efficiency(df = df_sankey |> filter(calc_sex %in% c('Female')))
ef_sex_male <- show_sankey_efficiency(df = df_sankey |> filter(calc_sex %in% c('Male')))
ef_ethnic_white <- show_sankey_efficiency(df = df_sankey |> filter(calc_ethnic_group %in% c('White')))
ef_ethnic_other <- show_sankey_efficiency(df = df_sankey |> filter(calc_ethnic_group %in% c('Asian or Asian British', 'Mixed', 'Black or Black British', 'Not stated', 'Other Ethnic Group')))
ef_intersect_imdq1_optin <- show_sankey_efficiency(df = df_sankey |> filter(calc_lsoa_imd_decile %in% c(1,2), invite_mode == 'Opt-in'))
ef_intersect_imdq1_optout <- show_sankey_efficiency(df = df_sankey |> filter(calc_lsoa_imd_decile %in% c(1,2), invite_mode == 'Opt-out'))
ef_intersect_imdq35_optin <- show_sankey_efficiency(df = df_sankey |> filter(calc_lsoa_imd_decile %in% c(5,6,7,8,9,10), invite_mode == 'Opt-in'))
ef_intersect_imdq35_optout <- show_sankey_efficiency(df = df_sankey |> filter(calc_lsoa_imd_decile %in% c(5,6,7,8,9,10), invite_mode == 'Opt-out'))

ef_intersect_age55_64_optin <- show_sankey_efficiency(df = df_sankey |> filter(calc_age_group_ipsos %in% c('55-64'), invite_mode == 'Opt-in'))
ef_intersect_age55_64_optout <- show_sankey_efficiency(df = df_sankey |> filter(calc_age_group_ipsos %in% c('55-64'), invite_mode == 'Opt-out'))
ef_intersect_age65_74_optin <- show_sankey_efficiency(df = df_sankey |> filter(calc_age_group_ipsos %in% c('65-74'), invite_mode == 'Opt-in'))
ef_intersect_age65_74_optout <- show_sankey_efficiency(df = df_sankey |> filter(calc_age_group_ipsos %in% c('65-74'), invite_mode == 'Opt-out'))


# CT conversion numbers
ct_conversion(df = df_sankey |> filter(invite_mode == 'Opt-in'))
ct_conversion(df = df_sankey |> filter(invite_mode == 'Opt-out'))
ct_conversion(df = df_sankey |> filter(invite_mode == 'Combined'))
ct_conversion(df = df_sankey |> filter(invite_mode %in% c('Opt-in', 'Opt-out')))
ct_conversion(df = df_sankey |> filter(triage_before_risk_assessment == 'Yes'))
ct_conversion(df = df_sankey |> filter(triage_before_risk_assessment == 'No'))
ct_conversion(df = df_sankey |> filter(project %in% c('Tameside and Glossop')))
ct_conversion(df = df_sankey |> filter(!project %in% c('Tameside and Glossop', 'Southampton')))
ct_conversion(df = df_sankey |> filter(admin == 'In-house'))
ct_conversion(df = df_sankey |> filter(admin == 'Outsourced'))
ct_conversion(df = df_sankey |> filter(calc_lsoa_imd_decile %in% c(1,2)))
ct_conversion(df = df_sankey |> filter(calc_lsoa_imd_decile %in% c(5, 6, 7, 8, 9, 10)))
ct_conversion(df = df_sankey |> filter(calc_age_group_ipsos %in% c('55-64')))
ct_conversion(df = df_sankey |> filter(calc_age_group_ipsos %in% c('65-74')))
ct_conversion(df = df_sankey |> filter(calc_sex %in% c('Female')))
ct_conversion(df = df_sankey |> filter(calc_sex %in% c('Male')))
ct_conversion(df = df_sankey |> filter(calc_ethnic_group %in% c('White')))
ct_conversion(df = df_sankey |> filter(calc_ethnic_group %in% c('Asian or Asian British', 'Mixed', 'Black or Black British', 'Not stated', 'Other Ethnic Group')))
ct_conversion(df = df_sankey |> filter(calc_lsoa_imd_decile %in% c(1,2), invite_mode == 'Opt-in'))
ct_conversion(df = df_sankey |> filter(calc_lsoa_imd_decile %in% c(1,2), invite_mode == 'Opt-out'))
ct_conversion(df = df_sankey |> filter(calc_lsoa_imd_decile %in% c(5,6,7,8,9,10), invite_mode == 'Opt-in'))
ct_conversion(df = df_sankey |> filter(calc_lsoa_imd_decile %in% c(5,6,7,8,9,10), invite_mode == 'Opt-out'))
ct_conversion(df = df_sankey |> filter(calc_age_group_ipsos %in% c('55-64'), invite_mode == 'Opt-in'))
ct_conversion(df = df_sankey |> filter(calc_age_group_ipsos %in% c('55-64'), invite_mode == 'Opt-out'))
ct_conversion(df = df_sankey |> filter(calc_age_group_ipsos %in% c('65-74'), invite_mode == 'Opt-in'))
ct_conversion(df = df_sankey |> filter(calc_age_group_ipsos %in% c('65-74'), invite_mode == 'Opt-out'))


# LHC conversion numbers
lhc_conversion(df = df_sankey |> filter(invite_mode == 'Opt-in'))
lhc_conversion(df = df_sankey |> filter(invite_mode == 'Opt-out'))
lhc_conversion(df = df_sankey |> filter(invite_mode == 'Combined'))
lhc_conversion(df = df_sankey |> filter(invite_mode %in% c('Opt-in', 'Opt-out')))
lhc_conversion(df = df_sankey |> filter(triage_before_risk_assessment == 'Yes'))
lhc_conversion(df = df_sankey |> filter(triage_before_risk_assessment == 'No'))
lhc_conversion(df = df_sankey |> filter(project %in% c('Tameside and Glossop')))
lhc_conversion(df = df_sankey |> filter(!project %in% c('Tameside and Glossop', 'Southampton')))
lhc_conversion(df = df_sankey |> filter(calc_lsoa_imd_decile %in% c(1,2)))
lhc_conversion(df = df_sankey |> filter(calc_lsoa_imd_decile %in% c(5, 6, 7, 8, 9, 10)))
lhc_conversion(df = df_sankey |> filter(calc_age_group_ipsos %in% c('55-64')))
lhc_conversion(df = df_sankey |> filter(calc_age_group_ipsos %in% c('65-74')))
lhc_conversion(df = df_sankey |> filter(calc_sex %in% c('Female')))
lhc_conversion(df = df_sankey |> filter(calc_sex %in% c('Male')))
lhc_conversion(df = df_sankey |> filter(calc_ethnic_group %in% c('White')))
lhc_conversion(df = df_sankey |> filter(calc_ethnic_group %in% c('Asian or Asian British', 'Mixed', 'Black or Black British', 'Not stated', 'Other Ethnic Group')))
lhc_conversion(df = df_sankey |> filter(calc_lsoa_imd_decile %in% c(1,2), invite_mode == 'Opt-in'))
lhc_conversion(df = df_sankey |> filter(calc_lsoa_imd_decile %in% c(1,2), invite_mode == 'Opt-out'))
lhc_conversion(df = df_sankey |> filter(calc_lsoa_imd_decile %in% c(5,6,7,8,9,10), invite_mode == 'Opt-in'))
lhc_conversion(df = df_sankey |> filter(calc_lsoa_imd_decile %in% c(5,6,7,8,9,10), invite_mode == 'Opt-out'))
lhc_conversion(df = df_sankey |> filter(calc_age_group_ipsos %in% c('55-64'), invite_mode == 'Opt-in'))
lhc_conversion(df = df_sankey |> filter(calc_age_group_ipsos %in% c('55-64'), invite_mode == 'Opt-out'))
lhc_conversion(df = df_sankey |> filter(calc_age_group_ipsos %in% c('65-74'), invite_mode == 'Opt-in'))
lhc_conversion(df = df_sankey |> filter(calc_age_group_ipsos %in% c('65-74'), invite_mode == 'Opt-out'))
