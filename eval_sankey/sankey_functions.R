#' -----------------------------------------------------------------------------
#' PRODUCE AGGREGATE SUMMARY FOR SANKEY
#' 
#' Produce an aggregate summary of the data for use in the Sankey app
#' 
#' This process requires that metrics are calculated first 
#' -----------------------------------------------------------------------------

# libraries --------------------------------------------------------------------
library(tidyverse)
library(here)
library(glue)
library(clock)

# udf --------------------------------------------------------------------------
source(here('scripts', 'tlhc_cancer_functions.R'))
source(here('scripts', 'tlhc_general_functions.R'))

#' Flag Participant smoking status
#'
#' @return
#' @export
#'
#' @examples
flag_smoking_status <- function(df) {
  
  # Current smoker when:
  # a. Is offered smoking cessation
  # b. has a total quit period greater than zero
  
  # current smokers
  # is noted in measurement as offered smoking cessation
  temp_meas_offered_sc <- df_meas |> 
    mutate(Smoking_Cessation_Offered <- Smoking_Cessation_Offered |> trimws()) |> 
    filter(
      !is.na(Smoking_Cessation_Offered),
      !str_detect(Smoking_Cessation_Offered |> tolower(), 'current'),
      !Smoking_Cessation_Offered %in% c('NA', 'NULL', '     ', 'No', 'N')
    ) |> 
    select(ParticipantID) |> 
    mutate(df_meas_smoking_cessation_offered = T) |> 
    distinct()
  
  # is registered in smoking cessation table
  temp_sc_offered_sc <- df_smoking |> 
    filter(!is.na(Date_Offered_Smoking_Cessation)) |> 
    select(ParticipantID) |> 
    mutate(df_smoking_date_offered = T) |> 
    distinct()
  
  # previous smoker
  temp_lhc_smok_quit <- df_lhc |> 
    mutate(
      # flag valid date stopped smoking values
      flag_valid_date_stopped_smoking = case_when(
        !is.na(Date_Stopped_Smoking) & 
          !Date_Stopped_Smoking %in% c('0', 'N/A', '00:00.0', '12:00:00 AM') ~ T
      ),
      
      df_lhc_previous_smoker = case_when(
        # they have quit
        Total_Quit_Period > 0 ~ F, 
        # quit period is zero but have a date when they stopped
        Total_Quit_Period == 0 & flag_valid_date_stopped_smoking ~ F,
        # quit period is empty but have a date when they stopped
        is.na(Total_Quit_Period) & flag_valid_date_stopped_smoking ~ F
      )
    ) |> 
    select(ParticipantID, df_lhc_previous_smoker) |> 
    arrange(ParticipantID, df_lhc_previous_smoker) |> 
    distinct(ParticipantID, .keep_all = T)
  
  
  df_smoking_status <- df |> 
    select(ParticipantID) |> 
    distinct() |> 
    # add in current smokers identified at LHC
    left_join(
      y = temp_meas_offered_sc,
      by = 'ParticipantID'
    ) |> 
    # add in current smokers identified as SC referral
    left_join(
      y = temp_sc_offered_sc,
      by = 'ParticipantID'
    ) |> 
    # add in previous smokers identified at LHC
    left_join(
      y = temp_lhc_smok_quit,
      by = 'ParticipantID'
    ) |> 
    # work out overall status
    mutate(
      smoking_flag = coalesce(
        df_meas_smoking_cessation_offered,
        df_smoking_date_offered,
        df_lhc_previous_smoker
      ),
      smoking_status = case_match(
        smoking_flag,
        T ~ 'Current smoker',
        F ~ 'Previous smoker',
        .default = 'Unknown'
      )
    ) |> 
    select(ParticipantID, smoking_status)
  
  # add smoking status as a field
  df <- left_join(
    x = df,
    y = df_smoking_status,
    by = 'ParticipantID'
  )
  
  return(df)
  
}


flag_people_scanned_after_threshold <- function(df, days_diff_limit = 185, cancer_end_date = as.Date('2023-08-31')) {
  
  # identify flags for people scanned
  flags <- df_ldct |> 
    # limit to valid scan records
    filter(
      calc_valid_transactionid == 'Valid', # valid transactions
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_ldct_date_corrected_yearmon), # exclude records without a LDCT date
      calc_ldct_outcome_corrected_groups == 'LDCT performed', # we have confirmation the scan took place (i.e. exclude future booked)
    ) |> 
    # select latest scan per person
    select(ParticipantID, calc_ldct_date_corrected) |> 
    arrange(ParticipantID, desc(calc_ldct_date_corrected)) |> 
    distinct(ParticipantID, .keep_all = T) |> 
    # flag scans within 185 days of end of cancer data
    mutate(
      day_diff = clock::date_count_between(
        start = calc_ldct_date_corrected,
        end = clock::add_days({{ cancer_end_date }}, {{ days_diff_limit }} * -1),
        precision = 'day'
      ),
      
      flag_awaiting_result_from_latest_scan = day_diff <= 0
    ) |> 
    select(ParticipantID, flag_awaiting_result_from_latest_scan)
    
  # add this flag to the given dataset and return the whole df
  df <- df |> 
    left_join(
      y = flags,
      by = 'ParticipantID'
    ) |> 
    # update cancer outcome if existing outcome is 'Scanned: No lung cancer' and flag is True
    mutate(
      cancer_outcome = case_when(
        cancer_outcome == 'Scanned: No lung cancer' & flag_awaiting_result_from_latest_scan ~ 'Scanned: Awaiting results',
        .default = cancer_outcome
      )
    ) |> 
    # housekeeping
    select(-flag_awaiting_result_from_latest_scan)
}


#' Produce summary of activity for Sankey analysis
#' 
#' Gathers milestones from the TLHC tables and cancer outcomes and returns a summary tibble of activity per participant.
#' 
#' @param days_diff_limit Integer number of days difference between event and diagnosis to use as the maximum threshold for TLHC-attributable cancers
#' @return Tibble fo data ready for use with Sankey plot
#' @export
#'
#' @examples
get_sankey_summary_df <- function(days_diff_limit = 185) {
  
  update_user('Getting People_Event_Diagnoses ...')
  # get the people_events_diagnosis table (but don't output the print messages)
  invisible(capture.output(
    df_ped <- get_people_event_diagnoses()
  ))
  
  update_user('Getting demographic details ...')
  # get demographic data ----
  df_sankey_demo <- df_demo |> 
    filter(ParticipantID %in% df_ped$ParticipantID) |> 
    select(
      ParticipantID,
      calc_lsoa_imd_decile,
      calc_lsoa_rurality_group_category,
      calc_ethnic_group,
      calc_sex,
      calc_age_group_ipsos
    )
  
  # get smoking status ----
  df_sankey_demo <- flag_smoking_status(df = df_sankey_demo)
  
  # get invite data ----
  update_user('Getting invite details ...')
  df_sankey_invites <- df_invites |> 
    filter(ParticipantID %in% df_ped$ParticipantID) |> 
    select(
      ParticipantID,
      project,
      calc_first_letter_date,
      calc_second_letter_date,
      calc_follow_up_call_date,
      Invite_Outcome,
    ) |> 
    mutate(
      # tidy up outcome
      calc_invite_outcome = case_when(
        Invite_Outcome %in% c(
          'Participant accepted invitation',
          'Participant_accepted_invitation',
          'TLHC Accepted'
        ) ~ 'Accepted',
        Invite_Outcome %in% c(
          'Participant declined invitation',
          'Participant_declined_invitation'
        ) ~ 'Declined',
        Invite_Outcome %in% c(
          'Participant did not respond to invitation',
          'Participant_did_not_respond_to_invitation',
          'Screeninginvite-notreplied'
        ) ~ 'No response',
        Invite_Outcome %in% c(
          'Participant does not meet age criteria',
          'Participant does not meet smoking criteria',
          'Participant removed from GP list',
          'Participant_Removed_from_GP_List'
        ) ~ 'Ineligible',
        TRUE ~ 'No response'
      ),
      
      # remove gaps in dates
      calc_first_letter_date = coalesce(
        calc_first_letter_date,
        calc_second_letter_date,
        calc_follow_up_call_date
      ),
      calc_second_letter_date = coalesce(
        calc_second_letter_date,
        calc_follow_up_call_date
      ),
      calc_follow_up_call_date = coalesce(
        calc_follow_up_call_date
      ),
      
      # invite outcomes
      calc_eligible = 'GP eligible population', # all participants start here
      
      calc_invite_1 = case_when(
        (!is.na(calc_first_letter_date) & !is.na(calc_second_letter_date)) ~ glue('Invite 1 No response'),
        (!is.na(calc_first_letter_date) & is.na(calc_second_letter_date)) ~ glue('Invite 1 {calc_invite_outcome}')
      ),
      
      calc_invite_2 = case_when(
        (!is.na(calc_second_letter_date) & !is.na(calc_follow_up_call_date)) ~ glue('Invite 2 No response'),
        (!is.na(calc_second_letter_date) & is.na(calc_follow_up_call_date)) ~ glue('Invite 2 {calc_invite_outcome}')
      ),
      
      calc_invite_3 = case_when(
        (!is.na(calc_follow_up_call_date)) ~ glue('Invite 3 {calc_invite_outcome}')
      ),
      
      calc_invite_outcome = glue('Invite {calc_invite_outcome}')
    ) |> 
    # simplify
    select(
      ParticipantID,
      project,
      calc_eligible,
      calc_invite_1,
      calc_invite_2,
      calc_invite_3,
      calc_invite_outcome
    )
  
  # add demographics to invites
  df_return <- left_join(
    x = df_sankey_invites,
    y = df_sankey_demo,
    by = "ParticipantID"
  )
  
  # get project reference values ----
  update_user('Getting project reference details ...')
  # load in additional project-level reference data
  df_return <- df_return |> 
    left_join(
      y = read_excel(path = here('data', 'reference', 'project_reference.xlsx')) |> 
        select(
          project, # key
          phase,
          CAName,
          invite_mode,
          lhc_delivery,
          triage_before_risk_assessment,
          admin
        ) |> 
        unique(),
      by = 'project'
    )
  
  # get lhc data ----
  update_user('Getting LHC details ...')
  df_sankey_lhc <- df_lhc |> 
    lazy_dt() |> # using dtplyr to speed up the processing
    filter(
      calc_valid_transactionid == 'Valid', # valid transactions
      calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_lhc_date_yearmon), # exclude records without a LHC date
      !calc_lhc_attendance_category == 'NULL response', # exclude records where we don't know the outcome
      !calc_lhc_delivery_method_category %in% c('NULL response', 'NOT CODED') # exclude records where we don't know the method
    ) |> 
    # create an attendance category that includes 'did not complete'
    mutate(
      calc_lhc_attendance_category_new = case_when(
        tolower(calc_lhc_attendance) %in% c(
          'attended',
          'finished',
          'participant attended lhc',
          'paticipant_attend_lhc',
          'screening - health check (xm1xs)'
        ) ~ 'LHC Attended',
        tolower(calc_lhc_attendance) %in% c(
          'did not attend',
          'didnotattend',
          'participant did not attend lhc',
          'paticipant_did_not_attend_lhc'
        ) ~ 'LHC DNA',
        tolower(calc_lhc_attendance) %in% c(
          'participant attended but did not complete lhc'
        ) ~ 'LHC Incomplete',
        TRUE ~ NA
      ),
      # convert to an ordered factor so can find find 'minimum' one
      calc_lhc_attendance_category_new = factor(
        calc_lhc_attendance_category_new,
        levels = c('LHC Attended', 'LHC Incomplete', 'LHC DNA'),
        ordered = T
      )
    ) |>
    # simplify to date and outcome
    select(
      ParticipantID,
      calc_lhc_date,
      calc_lhc_delivery_method_category,
      calc_lhc_attendance_category_new,
      project
    ) |> 
    # sequence lhc
    group_by(ParticipantID) |>
    arrange(calc_lhc_date) |> 
    mutate(
      calc_lhc_sequence = row_number(), # sequence of contacts from earliest
      calc_lhc_sequence_max = max(calc_lhc_sequence), # total number of contacts
      calc_lhc_attendance_category_overall = min(calc_lhc_attendance_category_new), # overall attendance for the patient
      calc_lhc_delivery_methods_all = paste(sort(unique(as.character(calc_lhc_delivery_method_category))), collapse = ', ') # list the methods used to contact the patient
    ) |> 
    as_tibble() # output as tibble from the dtplyr processing
  
  # contact methods
  df_sankey_lhc_1 <- df_sankey_lhc |> 
    filter(!is.na(calc_lhc_delivery_methods_all)) |> 
    select(ParticipantID, calc_lhc_delivery_methods_all) |> 
    unique()
  
  # contact counts
  df_sankey_lhc_2 <- df_sankey_lhc |> 
    mutate(
      calc_lhc_sequence_max_group = cut(
        x = calc_lhc_sequence_max,
        breaks = c(-Inf, 0, 1, 2, Inf),
        labels = c('0 x contacts', '1 x contacts', '2 x contacts', '3+ contacts')
      )
    ) |> 
    select(ParticipantID, calc_lhc_sequence_max_group) |> 
    unique()
  
  # contact outcome
  df_sankey_lhc_3 <- df_sankey_lhc |> 
    filter(!is.na(calc_lhc_attendance_category_overall)) |> 
    select(ParticipantID, calc_lhc_attendance_category_overall) |> 
    unique()
  
  # flag people whose LHC took place before end of cancer data
  # - LHC before 31st Aug 2023 - 185 days
  # - LHC before 31st Aug 2023
  df_sankey_lhc_4 <- df_sankey_lhc |> 
    # limit to people who attended their LHC
    filter(calc_lhc_attendance_category_overall == 'LHC Attended') |>
    # select the latest LHC
    arrange(ParticipantID, desc(calc_lhc_date)) |> 
    distinct(ParticipantID, .keep_all = T) |> 
    # flag LHCs before cut-off dates
    mutate(
      flag_lhc_before_sep23 = case_when(
        calc_lhc_date <= as.Date('2023-08-31') ~ TRUE,
        .default = FALSE
      ),
      flag_lhc_before_sep23minus185d = case_when(
        calc_lhc_date <= clock::add_days(as.Date('2023-08-31'), -185) ~ TRUE,
        .default = FALSE
      ),
      # NB 45 days is the median time between event and diagnosis
      flag_lhc_before_sep23minus45d = case_when(
        calc_lhc_date <= clock::add_days(as.Date('2023-08-31'), -45) ~ TRUE,
        .default = FALSE
      )
    ) |> 
    select(ParticipantID, flag_lhc_before_sep23, flag_lhc_before_sep23minus185d, flag_lhc_before_sep23minus45d)
  
  # combine the results
  df_sankey_lhc <- df_sankey_lhc |> 
    select(ParticipantID) |> 
    unique() |> 
    left_join(
      y = df_sankey_lhc_1,
      by = 'ParticipantID'
    ) |> 
    left_join(
      y = df_sankey_lhc_2,
      by = 'ParticipantID'
    ) |> 
    left_join(
      y = df_sankey_lhc_3,
      by = 'ParticipantID'
    ) |> 
    left_join(
      y = df_sankey_lhc_4,
      by = 'ParticipantID'
    ) |> 
    mutate(
      # convert back to regular character
      calc_lhc_attendance_category_overall = as.character(calc_lhc_attendance_category_overall),
      # ensure all records have an lhc outcome
      calc_lhc_attendance_category_overall = replace_na(
        calc_lhc_attendance_category_overall,
        'No LHC'
      )
    ) |> 
    relocate(calc_lhc_attendance_category_overall, .after = last_col())
  
  # add lhc to the return
  df_return <- left_join(
    x = df_return,
    y = df_sankey_lhc,
    by = 'ParticipantID'
  )
  
  # get measurements ----
  update_user('Getting measurement details ...')
  df_sankey_meas <- df_meas |> 
    filter(
      calc_valid_transactionid == 'Valid', # valid transactions
      calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
    ) |> 
    mutate(
      # output risk assessment
      calc_risk_assessment = case_when(
        (calc_LLPv2_risk_group == 'High risk' ) | (calc_PLCOm2012_risk_group == 'High risk') ~ 'High risk',
        (!is.na(calc_LLPv2_risk_group)) | (!is.na(calc_PLCOm2012_risk_group)) ~ 'Low risk'
      ),
      
      # output exception status
      calc_ineligible_status = case_when(
        (calc_risk_assessment == 'High risk') & (is.na(calc_exclusion_criteria)) ~ 'LDCT: referred',
        (calc_risk_assessment == 'High risk') & (!is.na(calc_exclusion_criteria)) ~ 'LDCT: ineligible'
      )
    ) |> 
    select(
      ParticipantID,
      calc_risk_assessment,
      calc_ineligible_status
    )
  
  # add meas to the return
  df_return <- left_join(
    x = df_return,
    y = df_sankey_meas,
    by = 'ParticipantID'
  )
  
  # get ldct ----
  update_user('Getting LDCT details ...')
  df_sankey_ldct <- df_ldct |> 
    lazy_dt() |> # using dtplyr to speed up
    filter(
      calc_valid_transactionid == 'Valid', # valid transactions
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_ldct_date_corrected_yearmon), # exclude records without a LDCT date
      calc_ldct_outcome_corrected_groups == 'LDCT performed', # we have confirmation the scan took place (i.e. exclude future booked)
    ) |> 
    group_by(ParticipantID) |> 
    mutate(
      calc_ldct_count = n_distinct(calc_ldct_date_corrected),
      calc_ldct_count_groups = cut(
        x = calc_ldct_count,
        breaks = c(-Inf, 1, 2, Inf),
        labels = c('1 x scan', '2 x scans', '3+ scans')
      )
    ) |> 
    ungroup() |> 
    select(ParticipantID, calc_ldct_count_groups) |> 
    unique() |> 
    as_tibble()
  
  # add ldct to the return
  df_return <- left_join(
    x = df_return,
    y = df_sankey_ldct,
    by = 'ParticipantID'
  ) 
  
  # handle DQ ----
  update_user('Fixing DQ issues ...')
  # add categories for risk score and LDCT referral to ensure we capture all patients
  df_return <- df_return |> 
    mutate(
      
      # RISK ASSESSMENT ---
      # add a 'No risk score' category if either: a) have a previous LHC attendance outcome or b) have a scan
      calc_risk_assessment_v2 = case_when(
        is.na(calc_risk_assessment) &
          (!is.na(calc_lhc_attendance_category_overall) | !is.na(calc_ldct_count_groups)) ~ 'No risk score'
      ),
      # combine together
      calc_risk_assessment = coalesce(calc_risk_assessment, calc_risk_assessment_v2),
      
      
      # LDCT referral ---
      # Capture patients where there is no LDCT: referral status but a scan is recorded
      calc_ineligible_status_v2 = case_when(
        is.na(calc_ineligible_status) & !is.na(calc_ldct_count_groups) ~ 'LDCT: unknown'
      ),
      # combine
      calc_ineligible_status = coalesce(calc_ineligible_status, calc_ineligible_status_v2),
      
      # LHC ATTENDANCE ---
      # add a 'No attendance' category if the patient has a risk assessment result
      calc_lhc_attendance_category_overall_v2 = case_when(
        is.na(calc_lhc_attendance_category_overall) & !is.na(calc_risk_assessment) ~ 'No attendance'
      ),
      # combine
      calc_lhc_attendance_category_overall = coalesce(calc_lhc_attendance_category_overall, calc_lhc_attendance_category_overall_v2)
      
    ) |> 
    select(-calc_risk_assessment_v2, -calc_ineligible_status_v2, -calc_lhc_attendance_category_overall_v2)
  
  # get lung cancers ----
  update_user('Getting lung cancer details ...')
  df_sankey_lung_cancers <- df_ped |>
    filter(tumour_site_group == 'Lung') |> 
    # limit to one lung cancer diagnosis per person - go with the closest diagnosis to event date
    arrange(ParticipantID, days_diff) |> 
    distinct(ParticipantID, .keep_all = T) |> 
    # flag diagnoses associated with TLHC
    mutate(
      # flag TLHC-attributable cancers
      cancer_outcome = case_when(
        (flag_diag_lhc_or_later == T) & (days_diff <= days_diff_limit) ~ 'TLHC: lung cancer',
        flag_diag_lhc_or_later == T ~ 'Counterfactual: lung cancer',
        flag_diag_lhc_or_later == F ~ 'Counterfactual: lung cancer'
      ),
      
      # format stage
      stage_group_formatted = case_match(
        stage_group,
        c('1', '2') ~ 'S 1-2',
        c('3', '4') ~ 'S 3-4',
        .default = 'S ?'
      ),
      
      # flag stage
      cancer_stage = case_match(
        cancer_outcome,
        'TLHC: lung cancer' ~ glue('TLHC: {stage_group_formatted}'),
        'Counterfactual: lung cancer' ~ glue('C: {stage_group_formatted}')
      )
    ) |> 
    # simplify output
    select(ParticipantID, cancer_outcome, cancer_stage)
  
  # add cancer to the return
  df_return <- df_return |> 
    left_join(
      y = df_sankey_lung_cancers,
      by = 'ParticipantID'
    ) |>
    # replace NA cancer outcomes with a description
    mutate(
      cancer_outcome = case_when(
        is.na(cancer_outcome) & is.na(calc_ldct_count_groups) ~ 'No lung cancer',
        is.na(cancer_outcome) & !is.na(calc_ldct_count_groups) ~ 'Scanned: No lung cancer',
        .default = cancer_outcome
      )
    )
  
  # return the result
  return(df_return)
}

#' Get Sankey aggregate df
#'
#' @return Tibble of pre-aggregated data
#' @export
#'
#' @examples
get_sankey_aggregate_df <- function(days_diff_limit = 185) {
  
  update_user(message = 'Get aggregate Sankey df', stage = 'start')
  
  # get the record-level df
  df_sankey_record <- get_sankey_summary_df(days_diff_limit = days_diff_limit) |> 
    flag_people_scanned_after_threshold()
  
  # aggregate this
  df_sankey_aggregate <- df_sankey_record |> 
    group_by(across(c(-ParticipantID))) |>  # group by everything except participant ID
    summarise(participants = n_distinct(ParticipantID, na.rm = T)) |> 
    ungroup()
  
  update_user(stage = 'end')
  return(df_sankey_aggregate)
}

save_sankey_aggregate <- function() {
  
  # get the sankey aggregate dataframe
  df_sankey <- get_sankey_aggregate_df(days_diff_limit = 185)
  
  # save as RDS object
  saveRDS(
    object = df_sankey,
    file = here('eval_sankey', 'secret', paste0('df_sankey_preagg_', strftime(now(), '%Y_%m_%d'), '.Rds'))
  )
  
  # open the folder
  #browseURL(url = here('eval_sankey', 'secret'))
  
}



# # reduce the cardinality of data
# df_sankey_preagg_simple <- df_sankey_preagg |>
#   filter(!phase == 'Phase 3') |>
#   # mutate(
#   #   calc_ethnic_group = case_match(
#   #     calc_ethnic_group,
#   #     'White' ~ 'White',
#   #     'Not known' ~ 'Unknown',
#   #     'Not stated' ~ 'Unknown',
#   #     .default = 'Non-White'
#   #   ),
#   #   calc_age_group_ipsos = case_match(
#   #     calc_age_group_ipsos,
#   #     'Other' ~ 'Other/Notknown/75',
#   #     '75' ~ 'Other/Notknown/75',
#   #     'Not known' ~ 'Other/Notknown/75',
#   #     .default = calc_age_group_ipsos
#   #   )
#   # ) |>
#   # select(
#   #   -calc_lhc_sequence_max_group, -calc_lhc_delivery_methods_all,
#   #   -calc_lsoa_imd_decile,
#   #   -calc_invite_1, -calc_invite_2, -calc_invite_3,
#   #   -project, -phase, -admin, -CAName,
#   #   -calc_ldct_count_groups
#   # ) |>
#   select(
#     participants,
#     calc_risk_assessment,
#     cancer_outcome
#   ) |>
#   group_by(across(-c(participants))) |>  # group by everything except participant count
#   summarise(participants = sum(participants, na.rm = T)) |>
#   ungroup() |>
#   arrange(participants)
# 
# df_sankey_preagg_simple |>
#   describe_df(ignore_calc = F)
# 
# df_sankey_preagg_simple |>
#   count(participants)

# people_event_diagnoses <- get_people_event_diagnoses()
# people_event_diagnoses_lung <- people_event_diagnoses |>
#   filter(tumour_site_group == 'Lung') |> 
#   # limit to one lung cancer diagnosis per person - go with the closest diagnosis to event date
#   arrange(ParticipantID, days_diff) |> 
#   distinct(ParticipantID, .keep_all = T) |> 
#   # flag diagnoses associated with TLHC
#   mutate(
#     # flag TLHC-attributable cancers
#     cancer_outcome = case_when(
#       (flag_diag_lhc_or_later == T) & (days_diff <= 147) ~ 'TLHC: lung cancer',
#       flag_diag_lhc_or_later == T ~ 'Counterfactual: lung cancer',
#       flag_diag_lhc_or_later == F ~ 'Counterfactual: lung cancer'
#     ),
#     
#     # format stage
#     stage_group_formatted = case_match(
#       stage_group,
#       c('1', '2') ~ 'S 1-2',
#       c('3', '4') ~ 'S 3-4',
#       .default = 'S ?'
#     ),
#     
#     # flag stage
#     cancer_stage = case_match(
#       cancer_outcome,
#       'TLHC: lung cancer' ~ glue('TLHC: {stage_group_formatted}'),
#       'Counterfactual: lung cancer' ~ glue('C: {stage_group_formatted}')
#     )
#   )
