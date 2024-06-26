#'------------------------------------------------------------------------------
#' TLHC PROCESS TABLES
#' 
#' Process downloaded tables to convert fields to required types, calculate
#' flags used in metric processing.
#' 
#'------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(tidyverse)     # tidy data processing
library(here)          # localise file references
library(lubridate)     # date functions
library(zoo)           # date functions (yearmonths)
library(furrr)         # parallel processing
library(future)        # parallel processing
library(progressr)     # progress bars
library(tictoc)        # timing processes
library(openxlsx)      # opening xlsx files
library(dtplyr)        # faster data wrangling

source(here('scripts', 'tlhc_general_functions.R'))

# Notify user 
update_user(stage = 'start', message = 'tlhc_process_sql_data.R')
tic()

# UDF --------------------------------------------------------------------------

#' Convert string-formatted dates to date types
#' 
#' This is a controller function which determines what type of string-formatted
#' date has been passed and calls on an appopriate function to parse and return
#' the date value.
#'
#' @param str_date Character String formatted date to be parsed
#'
#' @return Date
#' @examples 
#' convert_string_to_date(str_date = '24/11/2023')
convert_string_to_date <- function(str_date) {
  
  # decide which function to use when parsing this data
  if_else(
    nchar(str_date) == 5,
    convert_string_to_date_excel(str_date = str_date),   # e.g. '45254'
    convert_string_to_date_formatted(str_date = str_date)  # e.g. '24/11/2023'
  )
}

#' Convert excel-formatted date strings to date types
#'
#' Takes a string consisting of five characters and tries to parse it as a date
#' using Excel's origin date of 30/12/1899.
#' 
#' @param str_date Character String formatted date to be parsed
#'
#' @return Date
#' @examples
#' convert_string_to_date_excel(str_date = '45254')
convert_string_to_date_excel <- function(str_date) {
  date_output = as.Date(strtoi(str_date), origin = '1899-12-30')
  
  return(date_output)
}

#' Convert strings to dates
#'
#' Takes a string formatted date and attempts to parse to a date. Times 
#' (if supplied) are ignored.
#' 
#' @param str_date Character String formatted date to be parsed
#'
#' @return Date
convert_string_to_date_formatted <- function(str_date) {
  
  # define a list of date times to try
  orders = c(
    '%d/%m/%Y',                 #e.g. 01/01/2022
    '%d/%m/%Y %H:%M',           #e.g. 01/02/2022 10:30
    '%b %d %Y %H:%M',           #e.g. Mar 1 2022 11:30
    '%d-%m-%y',                 #e.g. 01-04-22
    '%Y-%m-%d %H:%M:%S',        #e.g. 2022-05-01 00:00:00
    '%Y%m%d',                   #e.g. 20220101
    '%d-%b-%y'                  #e.g. 25-Aug-21
  )

  # convert the string-formatted date
  date_output = as_date( # convert to a date (ignore times)
    parse_date_time2( # parse the field
      x = str_date,
      orders = orders # use the orders defined above
    )
  )
  
  return(date_output)
}


#' Process all tables ----------------------------------------------------------
#'
#' Extracts the code for the submitting organisation and adds the name
#' 
#' @param df tibble containing a TLHC table
#'
#' @return df
process_alltables <- function(df) {
  
  df <- df |> 
    mutate(
      # submitting org code
      calc_submitting_organisation_code = str_sub(SubmittedZipFile, 1, 5),
      
      # identify invalid Participant IDs
      calc_valid_participantid = case_when(
        nchar(ParticipantID) < 60 | is.numeric(ParticipantID) ~ 'Invalid',
        TRUE ~ 'Valid'
      ),
      
      # calculate month the file was received
      calc_received_date_yearmonth = as.yearmon(ReceivedDate),
      
      # calculate month the file was loaded
      calc_loaded_date_yearmon = as.yearmon(LoadDate)
    ) |> 
    # flag valid transaction ids (only applicable for C&M at the moment)
    group_by(calc_submitting_organisation_code) |> 
    mutate(
      calc_valid_transactionid = case_when(
        # C&M submission which is not the latest one
        (calc_submitting_organisation_code == 'RBQ00') & (TransactionId < max(TransactionId)) ~ 'Invalid',
        
        # North Kirklees / Bradford errant LDCT submission with all records on the same date (2023-02)
        TransactionId == 187617 ~ 'Invalid',
        
        # Tameside and Glossop - errant LDCT submission with records not recognised by project (2023-02-17)
        TransactionId == 148984 ~ 'Invalid',
        
        # Else mark as valid
        TRUE ~ 'Valid'
      )
    ) |> 
    ungroup()
  
  # add in submitting organisation name
  df <- left_join(
    x = df,
    y = df_projectlu |> rename(
      calc_submitting_organisation_code = ProjectCode,
      calc_submitting_organisation_name = ProjectName),
    by = 'calc_submitting_organisation_code'
  )
  
  return(df)
}

#' Process the demographic table -----------------------------------------------
#'
#' @param df tibble containing the raw demographics table
#'
#' @return df
process_demographics <- function(df) {
  
  # add in lsoa and deprivation data
  df <- left_join(
    x = df,
    y = df_lsoalu |> rename(
      LSOA = LSOA_CODE,
      calc_lsoa_name = LSOA_NAME,
      calc_lsoa_la_code = LA_CODE,
      calc_lsoa_la_name = LA_NAME,
      calc_lsoa_imd_rank = RANK,
      calc_lsoa_imd_decile = Decile
    ),
    by = 'LSOA'
  )
  
  # add in rurality
  df <- left_join(
    x = df,
    y = df_ruralitylu |> select(
      LSOA = LSOA11CD,
      calc_lsoa_rurality_code = RUC11CD,
      calc_lsoa_rurality_group = RUC11
    ) |> 
      mutate(
        calc_lsoa_rurality_group_category = case_when(
          calc_lsoa_rurality_code %in% c('A1', 'B1', 'C1', 'C2') ~ 'Urban',
          calc_lsoa_rurality_code %in% c('D1', 'D2', 'E1', 'E2', 'F1', 'F2') ~ 'Rural',
          TRUE ~ 'NOT CODED'
        )
      ),
    by = 'LSOA'
  )
  
  # add in martial status
  df <- left_join(
    x = df,
    y = df_marital_status,
    by = 'Marital_Status'
  )
  
  # add in ethnicity
  df <- left_join(
    x = df,
    y = df_ethnicitylu |> rename(calc_ethnic_group = EthnicGroup),
    by = 'Ethnicity'
  )
  
  # add in language
  df <- left_join(
    x = df,
    y = df_languagelu,
    by = 'Language'
  )
  
  # further processing
  df <- df |> 
    mutate(
      
      # gender
      calc_sex = case_when(
        toupper(trimws(Sex)) %in% c('F', 'FEMAL', 'FEMALE') ~ 'Female',
        toupper(trimws(Sex)) %in% c('M', 'MALE') ~ 'Male',
        TRUE ~ 'Not known'
      ),
      
      # age
      calc_age = str_remove(Age, 'yrs'), # tidy up ages supplied as XXyrs
      calc_age = na_if(calc_age, 'NULL'), # explicitly cast string literal 'NULL' as NA
      calc_age = na_if(calc_age, 'IA2'), # set as NA a spurious value that cannot be coerced to an age
      calc_age = as.numeric(calc_age), # cast to numeric
      calc_age_tlhc_valid = case_when( # categorises ages which are valid for tlhc (55-74)
        (calc_age >= 55) & (calc_age <= 74) ~ 'Valid', 
        TRUE ~ 'Invalid'
      ),
      
      # bins ages into groups required for MI report
      calc_age_group_report = fct_na_value_to_level(
        f = cut(
          x = calc_age,
          breaks = c(-Inf, -1, 49, 59, 69, 79, Inf),
          labels = addNA(c('Age below zero', '0-49', '50-59', '60-69', '70-79', '80+'))
        ),
        level = 'Not known'
      ),
      
      # bins ages into groups required for MI report (version 2)
      calc_age_group_report_v2 = fct_na_value_to_level(
        f = cut(
          x = calc_age,
          breaks = c(-Inf, -1, 49, 54, 59, 64, 69, 74, 79, Inf),
          labels = addNA(c('Age below zero', '0-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80+'))
        ),
        level = 'Not known'
      ),
      
      # bins ages into groups required for the Ipsos report
      calc_age_group_ipsos = fct_na_value_to_level(
        f = cut(
          x = calc_age,
          breaks = c(-Inf, -1, 54, 64, 74, 75, Inf),
          labels = c('Age below zero', 'Other', '55-64', '65-74', '75', 'Other')
        ),
        level = 'Not known'
      ),
      
      # marital status - convert to factor and ensure we have values for na
      calc_marital_status = fct_na_value_to_level(
        f = factor(
          calc_marital_status,
          levels = c('Single', 'Married or civil partner', 'Divorced', 'Separated', 'Widow(er)', 'Prefer not to say', 'Not known')
        ),
        level = 'Not known'
      ),
      
      # ethnic group - convert to factor and ensure we have values for na
      calc_ethnic_group = fct_na_value_to_level(
        f = factor(
          calc_ethnic_group,
          levels = c('Asian or Asian British', 'Black or Black British', 'Mixed', 'Not stated', 'Other Ethnic Group', 'White')
        ),
        level = 'Not known'
      ),
      
      # language - convert to a factor and ensure we have values for na
      calc_language = fct_na_value_to_level(
        f = fct_infreq(calc_language),
        level = 'Not known'
      )
    )
  
  return(df)
  
  # TODO Employment status (based on LHC table) --- will need to put this after the initial processing
  # TODO Smoking status (based on LHC table) --- will need to put this after the initial processing
}


#' Process the lung health check table -----------------------------------------
#'
#' @param df tibble containing the raw lung health check table
#'
#' @return df
process_lhc <- function(df) {
  
  # parse dates ---
  # NB, using base-R vectors to do this as much MUCH faster than dplyr
  df$calc_date_stopped_smoking <- convert_string_to_date(df$Date_Stopped_Smoking)
  df$calc_temp_lhc_date_v1 <- convert_string_to_date(df$LHC_Date)
  df$calc_temp_lhc_date_v2 <- convert_string_to_date(df$LHC_Attendance)
  df$calc_lhc_date <- coalesce(
    df$calc_temp_lhc_date_v1,
    df$calc_temp_lhc_date_v2
  )
  
  # calculate month of key dates ---
  # NB, using base-R vectors to do this as much MUCH faster than dplyr
  df$calc_lhc_date_yearmon <- as.yearmon(df$calc_lhc_date)
  df$calc_date_stopped_smoking_yearmon <- as.yearmon(df$calc_date_stopped_smoking)
  
  # tidy up attendance where dates and attendances were transposed ---
  # where transposed (i.e. there was a date in LHC_Attendance) then take the attendance value from the date field
  df$calc_temp_lhc_attendance_v1 <- case_when(
    !is.na(df$calc_temp_lhc_date_v2) ~ df$LHC_Date
  )
  
  # coalesce fields to get a single column of data
  df$calc_lhc_attendance <- coalesce(
    df$calc_temp_lhc_attendance_v1,
    df$LHC_Attendance
  )
  
  df <- df |> 
    mutate(
      
      # flag lhc attendances  
      calc_lhc_attendance_category = case_when(
        toupper(calc_lhc_attendance) %in% c(
          'ATTENDED',
          #'PARTICIPANT ATTENDED BUT DID NOT COMPLETE LHC',
          'PARTICIPANT ATTENDED LHC',
          'PATICIPANT_ATTEND_LHC',
          'SCREENING - HEALTH CHECK (XM1XS)',
          'FINISHED'
        ) ~ 'Attended',
        toupper(calc_lhc_attendance) %in% c(
          'PARTICIPANT ATTENDED BUT DID NOT COMPLETE LHC'
        ) ~ 'Partially attended',
        toupper(calc_lhc_attendance) %in% c(
          'DID NOT ATTEND',
          'DIDNOTATTEND',
          'PARTICIPANT DID NOT ATTEND LHC',
          'PATICIPANT_DID_NOT_ATTEND_LHC'
        ) ~ 'DNA',
        is.na(calc_lhc_attendance) ~ 'NULL response',
        .default = 'NOT CODED'
      ),
      
      # flag lhc delivery method
      calc_lhc_delivery_method_category = case_when(
        toupper(LHC_Delivery_Mode) %in% c(
          'F2F',
          'FACE TO FACE',
          'IN-PERSON'
        ) ~ 'F2F',
        toupper(LHC_Delivery_Mode) %in% c(
          'PHONE',
          'TELEPHONE',
          'VIRTUAL'
        ) ~ 'Virtual',
        is.na(LHC_Delivery_Mode) ~ 'NULL response',
        TRUE ~ 'NOT CODED'
      ),
    ) |> 
    # tidy up temp columns
    select(-calc_temp_lhc_date_v1, -calc_temp_lhc_date_v2, -calc_temp_lhc_attendance_v1)
  
  return(df)
}

#' Process the Other History table ---------------------------------------------
#'
#' @param df tibble containing the raw other history table
#'
#' @return df
process_otherhistory <- function(df) {
  
  # parse dates ---
  # NB, using base-R vectors to do this as much MUCH faster than dplyr
  df$calc_cancer_date <- convert_string_to_date(df$Cancer_Date)
  
  return(df)
}

#' Process the Diagnostics table -----------------------------------------------
#'
#' @param df tibble containing the diagnostics history table
#'
#' @return df
process_diagnostics <- function(df) {
  
  # parse dates ---
  # NB, using base-R vectors to do this as much MUCH faster than dplyr
  df$calc_pet_ct_date <- convert_string_to_date(df$`PET-CT_Date`)
  df$calc_bronchoscopy_date <- convert_string_to_date(df$Bronchoscopy_Date)
  df$calc_ebus_date <- convert_string_to_date(df$EBUS_Date)
  df$calc_eus_date <- convert_string_to_date(df$EUS_Date)
  df$calc_mediastinoscopy_date <- convert_string_to_date(df$Mediastinoscopy_Date)
  df$calc_lung_biospy_date <- convert_string_to_date(df$Lung_Biopsy_Date)
  df$calc_diagnostic_complication_date <- convert_string_to_date(df$Diagnostic_Complication_Date)
  df$calc_pathology_report_date <- convert_string_to_date(df$Pathology_Report_Date)
  df$calc_cancer_diagnosis_date <- convert_string_to_date(df$Cancer_Diagnosis_Date)
  df$calc_treatment_start_date <- convert_string_to_date(df$Treatment_Start_Date)
  
  # convert key dates to yearmon ---
  df$calc_cancer_diagnosis_date_yearmon <- as.yearmon(df$calc_cancer_diagnosis_date)
  
  return(df)
}

#' Process the Invites table ---------------------------------------------------
#'
#' @param df tibble containing the invites table
#'
#' @return df
process_invites <- function(df) {
  
  # parse dates ---
  # NB, using base-R vectors to do this as much MUCH faster than dplyr
  df$calc_first_letter_date <- convert_string_to_date(df$First_Letter_Date)
  df$calc_second_letter_date <- convert_string_to_date(df$Second_Letter_Date)
  df$calc_follow_up_call_date <- convert_string_to_date(df$Follow_Up_Call_Date)
  df$calc_contact_date <- convert_string_to_date(df$Contact_Date)
  
  # assume the outcome date is the maximum of these fields
  df$calc_invite_outcome_date <- pmax(
    df$calc_contact_date,
    df$calc_follow_up_call_date,
    df$calc_second_letter_date,
    df$calc_first_letter_date,
    na.rm = T
  )
  
  # convert key dates to yearmon ---
  df$calc_first_letter_date_yearmonth <- as.yearmon(df$calc_first_letter_date)
  df$calc_second_letter_date_yearmonth <- as.yearmon(df$calc_second_letter_date)
  df$calc_follow_up_call_date_yearmonth <- as.yearmon(df$calc_follow_up_call_date)
  df$calc_invite_outcome_date_yearmon <- as.yearmon(df$calc_invite_outcome_date)
  
  # convert string dates to date-types
  df <- df |> 
    mutate(
      
      # calculate eligibility for lhc
      calc_eligible = case_when(
        toupper(Invite_Outcome) %in% c(
          'PARTICIPANT DOES NOT MEET AGE CRITERIAA',
          'PARTICIPANT DOES NOT MEET AGE CRITERIA',
          'PARTICIPANT DOES NOT MEET SMOKING CRITERIA',
          'PARTICIPANT REMOVED FROM GP LIST',
          'PARTICIPANT_REMOVED_FROM_GP_LIST'
        ) ~ 'Ineligible',
        TRUE ~ 'Eligible'
      ),
      
      # calculate lhc acceptance
      calc_invite_accepted = case_when(
        toupper(Invite_Outcome) %in% c(
          'ACCEPTED',
          'PARTICIPANT ACCEPTED INVITATION',
          'PARTICIPANT_ACCEPTED_INVITATION',
          'TLHC ACCEPTED'
        ) ~ 'Accepted'
      )
    )
  
  return(df)
}

#' Process the LDCT table ------------------------------------------------------
#'
#' @param df tibble containing the LDCT table
#'
#' @return df
process_ldct <- function(df) {
  
  # parse dates ---
  # NB, using base-R vectors to do this as much MUCH faster than dplyr

  # correct ldct date for when dates and outcomes were recorded in the wrong columns
  df$calc_ldct_date_corrected <- coalesce(
    convert_string_to_date(df$LDCT_Date),
    convert_string_to_date(df$LDCT_Outcome)
  )
  
  df$calc_ldct_report_date <- convert_string_to_date(df$LDCT_ReportDate1)
  df$calc_date_referral_lung_cancer <- convert_string_to_date(df$Date_Referral_Lung_Cancer)
  df$calc_referral_date_to_tuberculosis <- convert_string_to_date(df$Referral_Date_To_Tuberculosis)
  df$calc_referral_date_to_secondary_care_respiratory <- convert_string_to_date(df$Referral_Date_To_Secondary_Care_Respiratory)
  df$calc_date_referralothercancer <- convert_string_to_date(df$Date_ReferralOtherCancer)
  df$calc_referral_date_to_secondary_care_other_noncancer_team <- convert_string_to_date(df$`Referral_Date_To_Secondary_Care-Other_Non-Cancer_Team`)
  df$calc_date_referralprimarycare <- convert_string_to_date(df$Date_ReferralPrimaryCare)
  df$calc_date_lhc_letter_sent_to_patient <- convert_string_to_date(df$Date_LHC_Letter_Sent_To_Patient)
  df$calc_date_lhc_results_letter_sent_to_gp <- convert_string_to_date(df$Date_LHC_Results_Letter_Sent_To_GP)
  
  # calculate month of key dates ---
  df$calc_ldct_date_corrected_yearmon <- as.yearmon(df$calc_ldct_date_corrected)
  df$calc_date_referral_lung_cancer_yearmon <- as.yearmon(df$calc_date_referral_lung_cancer)

  # convert string dates to date-types
  df <- df |>
    mutate(
      
      # calculate the scan outcome status
      calc_ldct_outcome_v2 = case_when(LDCT_Date == 'LDCT performed' ~ LDCT_Date),        # get outcomes recorded in the dates column
      calc_ldct_outcome_corrected = coalesce(calc_ldct_outcome_v2, LDCT_Outcome),         # get the outcomes
      calc_ldct_outcome_corrected_groups = suppressWarnings(
        recode(
          toupper(calc_ldct_outcome_corrected),
          'LD CT SCAN PERFORMED' = 'LDCT performed',
          'LDCT _PERFORMED' = 'LDCT performed',
          'LDCT PERFORMED' = 'LDCT performed',
          'LDCT PERFROMED' = 'LDCT performed',
          'LDCT_PERFORMED' = 'LDCT performed',
          'PERFORMED' = 'LDCT performed',
          'LDCT DID NOT ATTEND' = 'LDCT did not attend',
          'LDCT NOT COMPLETED (TECHNICAL/OPERATIONA' = 'LDCT not completed (technical / operational)',
          'LDCT NOT COMPLETED (TECHNICAL/OPERATIONAL)' = 'LDCT not completed (technical / operational)',
          'LDCT NOT COMPLETED(TECHNICAL/OPERATIONAL)' = 'LDCT not completed (technical / operational)',
          'LDCT NOT COMPLETED (PARTICIPANT UNWELL)' = 'LDCT not completed (participant unwell)',
          'LDCT NOT COMPLETED (PARTICIPANT ABANDONE' = 'LDCT not completed (participant abandoned)',
          'LDCT NOT COMPLETED (PARTICIPANT ABANDONED)' = 'LDCT not completed (participant abandoned)',
          'LDCT NOT COMPLETED(PARTICIPANT ABANDONED)' = 'LDCT not completed (participant abandoned)',
          'LDCT NOT COMPLETED' = 'LDCT not completed (no reason recorded)',
          .default = 'Other'
        )
      )
    )
}

#' Post-process LDCT table
#' 
#' Takes the tibble of LDCT data which has been processed to convert strings to dates
#' and calculate outcome groups, and adds additional processing to:
#' 
#' 1. correct the LDCT dates
#'    Where the provided LDCT date occurs before the person was first invited.
#'    This was picked up in the review of TLHC data where scan dates often preceeded
#'    the date of invite - which should not happen. Visual inspection of these found
#'    LDCT report date is often a better fit for date sequence.
#'    
#' 2. sequence LDCT scans
#'    order the scans by date and work out which is the baseline and work out the
#'    follow-up period of any subsequent scans.
#' 
#' @param df Tibble of LDCT data which has been processed
#'
#' @return Tibble of post-processed LDCT data
post_process_ldct <- function() {
  
  # load the processed data 
  df <- readRDS(file = here('data', 'tlhc', 'calc_tbTLHCTLHC_Pathway_LDCT.Rds'))
  df_invites <- readRDS(file = here('data', 'tlhc', 'calc_tbTLHCTLHC_Pathway_Invite.Rds'))
  
  # correct the LDCT dates
  # step 1 - add in the first invite date
  # create a lookup between participant id and eligibility and initial invite date (for metric 10)
  temp_first_letter_date <- df_invites |> 
    select(ParticipantID, calc_first_letter_date) |> 
    unique()
  
  # add eligiblity to the supplied df
  df <- left_join(
    x = df,
    y = temp_first_letter_date,
    by = "ParticipantID"
  )
  
  # housekeeping
  rm(temp_first_letter_date)
  
  # step 2 - correct ldct dates for cases where the report date is a better fit
  df <- df |> 
    mutate(
      # flag records where this phenomena is true so we can track them later on
      flag_reportdate_better_than_ldctdate = case_when(
        # where the report date is a better fit for the sequence of events
        !is.na(calc_ldct_date_corrected) & # we need an ldct date
          !is.na(calc_ldct_report_date) & # we need a report date
          !is.na(calc_first_letter_date) & # we need a first letter date
          calc_ldct_date_corrected < calc_first_letter_date & # current ldct date is earlier than the invite (the issue we are trying to fix)
          calc_ldct_report_date >= calc_first_letter_date # but the report date occurs after the invite 
          ~ TRUE,
        .default = FALSE
      ),
      
      # change the ldct date to report date for cases where this makes sense, otherwise keep the existing date
      calc_ldct_date_corrected = case_when(
        flag_reportdate_better_than_ldctdate == T ~ calc_ldct_report_date,
        .default = calc_ldct_date_corrected
      )
    ) |> 
    # remove the calc_first_letter_date field - this is added in the TLHC metrics functions and we don't want to cause issues
    select(-calc_first_letter_date)
  
  # work out sequence of scans - requires a temp table calculating which to be joined to the main table afterwards
  # !! NB REQUIRES FILTER ON LDCT DATE, the following relies on at least one ldct date being non-NA
  ## NNB, this is also computationally expensive so using the dtplyr lazy_dt to handle processing and transforming back to tibble
  ## afterward.
  
  # step 1 - work out date of first attended scan per participant
  df_temp_firstattended <- lazy_dt(df) |> 
    ungroup() |> 
    filter(
      !is.na(calc_ldct_date_corrected), # the following process requires dates
      calc_ldct_outcome_corrected_groups == 'LDCT performed' # limit to attended scans
    ) |> 
    select(ParticipantID, calc_ldct_date_corrected) |> # small subset
    unique() |> # handle cases where same dates have been uploaded in different formats (e.g.15/09/2021 and 2021-09-15 00:00:00)
    group_by(ParticipantID) |> 
    summarise(calc_temp_ldct_date_first = min(calc_ldct_date_corrected, na.rm = T)) |>  # get the date of the first attended scan
    ungroup() |> 
    as_tibble()
  
  # step 2 - add first scan date and proceed
  df_temp <- left_join(
    x = df,
    y = df_temp_firstattended,
    by = 'ParticipantID'
  )
  
  # step 3 - process the table
  df_temp <- lazy_dt(df_temp) |> 
    ungroup() |> 
    filter(!is.na(calc_ldct_date_corrected)) |> # the following process requires dates
    select(ParticipantID, calc_ldct_date_corrected, calc_temp_ldct_date_first) |> # small subset
    unique() |> # handle cases where same dates have been uploaded in different formats (e.g.15/09/2021 and 2021-09-15 00:00:00)
    group_by(ParticipantID) |> 
    arrange(calc_ldct_date_corrected, .by_group = T) |> # sort by scan date for each participant
    mutate(
      calc_ldct_date_corrected_days_from_first = (calc_ldct_date_corrected - calc_temp_ldct_date_first), # days since first scan
      calc_ldct_date_corrected_category = cut(
        x = as.numeric(calc_ldct_date_corrected_days_from_first),
        breaks = c(-Inf, -1, 1, 61, 152, 335, 456, 669, 791, 1400, 1521, Inf),
        labels = c(
          'Not classified',               # -Inf to -1
          'Initial scan',                 # -1 to 1 days --
          'Not classified',               # 2 to 61 days
          '3 month follow-up scan',       # 61 to 152 days --
          'Not classified',               # 152 to 335 days
          '12 month follow-up scan',      # 335 to 456 days --
          'Not classified',               # 456 to 669 days
          '24 month follow-up scan',      # 669 to 791 days --
          '2-year+ nodule surveillance',  # 791 to 1400 days
          '48 month follow-up scan',      # 1400 to 1521 days --
          '2-year+ nodule surveillance'   # above 1521 days
        )
      ),
      calc_ldct_date_corrected_sequence = row_number(), # whether the scan is the 1st, 2nd, 3rd, etc in the sequence
      temp_ldct_date_corrected_previous = lag(x = calc_ldct_date_corrected, n = 1), # get the date of the previous scan (where appropriate)
    ) |> 
    ungroup() |> 
    as_tibble()
  
  # calculate month intervals ---
  # NB, using base-R vectors to do this as much MUCH faster than dplyr
  
  # calculate the number of months since first scan
  df_temp$calc_ldct_date_corrected_months_from_first = date_count_between(
    start = df_temp$calc_temp_ldct_date_first,
    end = df_temp$calc_ldct_date_corrected,
    precision = 'month'
  )
  
  # calculate the number of months since the previous scan
  df_temp$calc_ldct_date_corrected_months_from_previous = date_count_between(
    start = df_temp$temp_ldct_date_corrected_previous,
    end = df_temp$calc_ldct_date_corrected,
    precision = 'month'
  )
  
  # add these details to the main table
  df <- left_join(
    x = df,
    y = df_temp |> select(-temp_ldct_date_corrected_previous),
    by = c('ParticipantID', 'calc_ldct_date_corrected')
  )
  rm(df_temp) # housekeeping
  
  # save the ldct table
  saveRDS(
    object = df,
    file = here('data', 'tlhc', 'calc_tbTLHCTLHC_Pathway_LDCT.Rds'),
    compress = F # don't compress to make metric calculation quicker
  )
}

#' Process the Measurements table ----------------------------------------------
#'
#' @param df tibble containing the Measurement table
#'
#' @return df
process_measurement <- function(df) {

  df <- df |> 
    # Convert risk score fields to numeric values (NB, used in ldct referral eligibility metric)
    mutate(
      # PLCOm2012 score (NB, a predictive risk model score for six-year lung cancer risk - expressed as a percentage)
      calc_PLCOm2012 = na_if(PLCOm2012, 'NULL'), # deal with textual NULLs
      calc_PLCOm2012 = str_replace(string = calc_PLCOm2012, pattern = '\\.{2}|\\:', replacement = '.'), # deal with malformatted values (e.g. 0..45 or 01:50)
      calc_PLCOm2012 = as.numeric(calc_PLCOm2012), # convert to numeric
      
      # PLCOm2012 risk group with reference to a 1.51 threshold
      calc_PLCOm2012_risk_group = ifelse(calc_PLCOm2012 >= 1.51, 'High risk', 'Low risk'),
      
      # LLPv2 score (NB, an alternate risk model score)
      calc_LLPv2_risk = na_if(LLPv2_Risk, 'NULL'), # deal with textual NULLs
      calc_LLPv2_risk = str_replace(string = calc_LLPv2_risk, pattern = '\\>|\\.{2}', replacement = ''), # deal with malformatted values (e.g. >3.56 or 2..07)
      calc_LLPv2_risk = str_remove(string = calc_LLPv2_risk, pattern = '\\?|\\;'), # deal with malformatted values (e.g. '?0.89 ' or 1.56;)
      calc_LLPv2_risk = trimws(calc_LLPv2_risk), # deal with malformed values including leading or trailing spaces
      calc_LLPv2_risk = as.numeric(calc_LLPv2_risk), # convert to numeric
      
      # LLPv2 risk group with reference to a 2.5 threshold
      calc_LLPv2_risk_group = ifelse(calc_LLPv2_risk >= 2.5, 'High risk', 'Low risk'),
      
      # convert ldct exclusion criteria into valid options
      calc_exclusion_criteria = na_if(Exclusion_Criteria, '     '),     # tidy up some null values
      calc_exclusion_criteria = na_if(toupper(calc_exclusion_criteria), 'NONE'), # tidy up some null values
      calc_exclusion_criteria = na_if(toupper(calc_exclusion_criteria), 'NULL'), # tidy up some null values
      calc_exclusion_criteria = suppressWarnings(
        recode(
          toupper(calc_exclusion_criteria),
          'UNABLE TO LIE DOWN' = 'Unable to lie flat',
          'UNABLE TO LIE FLAT' = 'Unable to lie flat',
          'WEIGHT >200KG' = 'Weight >200Kg',
          'PREVIOUS CT <12 MONTHS AGO' = 'Previous CT <12 months ago',
          'DOES NOT HAVE CAPACITY TO CONSENT TO LDCT' = 'Does not have capacity to consent to LDCT',
          'NOT PHYSICALLY FIT' = 'Not physically fit',
          'PARTICIPANT DECLINED' = 'Participant declined',
          'SCREENING DECLINED (XAZ1N)' = 'Participant declined',
          'PROCEDURE CONTRAINDICATED' = 'Procedure contraindicated',
          'PROCEDURECONTRAINDICATED' = 'Procedure contraindicated',
          'DUE FOR CT SCAN THORAX/ABDOMEN WITH CONTRAST AT HOSPITAL ON 09/09/2022' = 'Other',
          'CT BOOKED WITHIN NEXT 3 MONTHS' = 'Other',
          .default = 'Other'
        )
      )
    )
  
  return(df)
}

#' Process the Smoking table ---------------------------------------------------
#'
#' @param df tibble containing the Smoking table
#'
#' @return df
process_smoking <- function(df) {
  
  # parse dates ---
  # NB, using base-R vectors to do this as much MUCH faster than dplyr
  # df$calc_field <- convert_string_to_date(df$Field)
  df$calc_date_offered_smoking_cessation <- convert_string_to_date(df$Date_Offered_Smoking_Cessation)
  df$calc_date_started_smoking_cessation <- convert_string_to_date(df$`Date_Started-Smoking_Cessation`)
  df$calc_date_ended_smoking_cessation <- convert_string_to_date(df$Date_Ended_Smoking_Cessation)
  
  # convert key dates to yearmon ---
  df$calc_date_offered_smoking_cessation_yearmon <- as.yearmon(df$calc_date_offered_smoking_cessation)
  df$calc_date_started_smoking_cessation_yearmon <- as.yearmon(df$calc_date_started_smoking_cessation)
  df$calc_date_ended_smoking_cessation_yearmon <- as.yearmon(df$calc_date_ended_smoking_cessation)
  
  df <- df |> 
    mutate(
      
      # convert smoking flag to valid options
      calc_smoking_cessation_completed_successfully = 
        suppressWarnings(
          recode(
            toupper(
              trimws(Smoking_Cessation_Completed_Successfully)
            ),
            'N' = 'NO',
            'NO' = 'NO',
            'Y' = 'YES',
            'YES' = 'YES'
          )
        )
    )
  
  return(df)
}

#' Process TLHC table ----------------------------------------------------------
#' 
#' Process a supplied tibble to make it easier to work with
#' Convert date fields to recognised dates
#' 
#' @param df 
#' @param str_table String name of the table being processed
#'
#' @return df
process_tlhc_table <- function(df, str_table = '') {
  
  # define a list of date times to try
  orders = c(
    '%d/%m/%Y',
    '%d/%m/%Y %H:%M',
    '%b %d %Y %H:%M',
    '%d-%b-%y'
  )
  
  ## processing for all tables -------------------------------------------------
  df <- process_alltables(df = df)
  
  ## table-specific processing -------------------------------------------------
  if(str_table == 'tbTLHCTLHC_Demographics') {df <- process_demographics(df = df |> ungroup())} 
  else if(str_table == 'tbTLHCTLHC_LungHealthCheck') {df <- process_lhc(df = df |> ungroup())}
  else if(str_table == 'tbTLHCTLHC_OtherHistory') {df <- process_otherhistory(df = df |> ungroup())}
  else if(str_table == 'tbTLHCTLHC_Pathway_Invite') {df <- process_invites(df = df |> ungroup())} 
  else if(str_table == 'tbTLHCTLHC_Pathway_LDCT') {df <- process_ldct(df = df |> ungroup())}
  else if(str_table == 'tbTLHCTLHC_Measurements') {df <- process_measurement(df = df |> ungroup())}
  else if(str_table == 'tbTLHCTLHC_SmokingCessation') {df <- process_smoking(df = df |> ungroup())}
  else if(str_table == 'tbTLHCTLHC_Pathway_Diagnostics') {df <- process_diagnostics(df = df |> ungroup())}
  
  return(df)
}

#' Manage table processing -----------------------------------------------------
#' 
#' Manages the processing of the table so that it:
#' 1. loads the data from file
#' 2. processes where required - managed by process_tlhc_table
#' 3. saves the file
#'
#' @param str_table String name for the table to be loaded
#'
#' @return
manage_table_processing <- function(str_table) {
  
  # read the file
  df <- readRDS(file = here('data', 'tlhc', paste0(str_table, '.Rds'))) |> ungroup()
  
  # process the file
  df_calc <- process_tlhc_table(df, str_table = str_table)
  
  # DEVELOPMENT - process the file
  #df <- readRDS(file = here('mi_data', 'tlhc', 'tbTLHCTLHC_Demographics.Rds')) |> ungroup()
  #df_sample <- sample_n(tbl = df, size = 1000)
  #df_calc <- process_tlhc_table(df = sample_n(tbl = df, size = 100000), str_table = str_table)
  
  # save the new file
  # saveRDS(
  #   object = df_calc,
  #   file = here('data', 'tlhc', paste0('calc_', str_table, '.Rds'))
  # )
  
  # save the file without compression
  future:::save_rds(
    object = df_calc,
    pathname = here('data', 'tlhc', paste0('calc_', str_table, '.Rds')),
    compress = F # don't compress to make metric calculation quicker
  )
  
  # update the user
  p(str_table)
  #cat(paste('☑️', Sys.time(), '...', str_table, 'processed\n', sep = ' '))
}

# notify the user
update_user(message = 'Setup complete and UDFs loaded')

# Load data --------------------------------------------------------------------
# load tables and run them through the processing function

# initiate tlhc file read process with progress indicator
update_user(message = 'Processing downloaded tables, please wait ...', icon = '⏱️')

## project lookups (used as reference) ----
df_projectlu <- readRDS(file = here('data', 'tlhc', 'dboProjectLookup.Rds')) |> ungroup()
df_lsoalu <- readRDS(file = here('data', 'tlhc', 'lkp_LSOADeprivation2019.Rds')) |> ungroup()
df_ruralitylu <- readRDS(file = here('data', 'tlhc', 'Rural_Urban_Classification.Rds')) |> ungroup()
df_marital_status <- read.xlsx(xlsxFile = here('data', 'reference', 'marital_status.xlsx')) |> ungroup()
#df_ethnicitylu <- readRDS(file = here('mi_data', 'tlhc', 'EthnicityLookup.Rds')) |> ungroup() # NB, this one is faulty.
df_ethnicitylu <- read.xlsx(xlsxFile = here('data', 'reference', 'ethnicity.xlsx')) |> ungroup() |> unique()
df_languagelu <- read.xlsx(xlsxFile = here('data', 'reference', 'language.xlsx')) |> ungroup() |> unique()

## list tables to process
df_table_details <- tibble(
  table = c(
    'tbTLHCTLHC_Demographics',
    'tbTLHCTLHC_LungHealthCheck',
    'tbTLHCTLHC_Measurements',
    'tbTLHCTLHC_OtherHistory',
    'tbTLHCTLHC_Pathway_Diagnostics',
    'tbTLHCTLHC_Pathway_Invite',
    'tbTLHCTLHC_Pathway_LDCT',
    'tbTLHCTLHC_SmokingCessation'
  )
)

# Set up the progress bar and parallel processing
handlers(handler_progress(format='[:bar] :percent :eta :message')) # set up the progress indicator
plan('multisession') # set up the future.apply package

# begin parallel processes
with_progress({
  
  # set up progress bar  
  p <- progressor(steps = length(df_table_details$table))
  
  # call function to process table data
  df_table_details <- df_table_details |>
    mutate(
      data = future_pmap(
        .l = list(table),
        .f = manage_table_processing,
        .options = furrr_options(seed=NULL)
      )
    )
  
})

# post-process the ldct table
update_user(message = 'Post-processing the LDCT table', icon = '⏱️')
post_process_ldct()


# manual processing - development only ----
#manage_table_processing(str_table = 'tbTLHCTLHC_Demographics')
#df_demo <- readRDS(here('mi_data', 'tlhc', 'calc_tbTLHCTLHC_Demographics.Rds'))
# df_marital_status <- df_demo |>
#   ungroup() |>
#   count(calc_marital_status) |> 
#   view()
# 
# write_csv(x = df_marital_status, file = here('mi_data', 'tlhc', 'reference', 'marital_status.csv'))

# done!
update_user(stage = 'end')
toc()