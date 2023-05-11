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

#' Convert strings to dates
#'
#' Takes a string formatted date and attempts to parse to a date. Times 
#' (if supplied) are ignored.
#' 
#' @param str_date 
#'
#' @return datetype
convert_string_to_date <- function(str_date) {
  
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
  
  # convert the date
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
      # calc_valid_participantid = case_when(
      #   nchar(ParticipantID) < 15 ~ 'Invalid', # length is too short
      #   is.numeric(ParticipantID) ~ 'Invalid', # not an alphanumeric pseudo_NHS_number
      #   TRUE ~ 'Valid'
      # ),
      
      #calc_valid_participantid_length = (nchar(as.character(ParticipantID)) < 60),
      #calc_valid_participantid_numeric = is.numeric(ParticipantID),
      # calc_valid_participantid = case_when(
      #   calc_valid_participantid_length | calc_valid_participantid_numeric ~ 'Invalid',
      #   .default = 'Valid'
      # ),
      
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
      # calc_valid_transactionid = ifelse(
      #   # C&M submission which is not the latest one
      #   (calc_submitting_organisation_code == 'RBQ00') & (TransactionId < max(TransactionId)),
      #   'Invalid',
      #   'Valid'
      # )
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
        toupper(trimws(Sex)) %in% c('F', 'FEMAL') ~ 'Female',
        toupper(trimws(Sex)) %in% c('M', 'MALE') ~ 'Male',
        TRUE ~ 'Not known'
      ),
      
      # age
      calc_age = str_remove(Age, 'yrs'), # tidy up ages supplied as XXyrs
      calc_age = na_if(calc_age, 'NULL'), # explicitly cast string literal 'NULL' as NA
      #calc_age = as.numeric(str_remove(Age, 'yrs')), # tidy up ages supplied as XXyrs
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
  
  df <- df |> 
    mutate(
      # convert string dates to dates
      #calc_lhc_date = convert_string_to_date(LHC_Date),
      calc_date_stopped_smoking = convert_string_to_date(Date_Stopped_Smoking),
      
      # tidy up some data where dates and attendances were transposed
      calc_temp_lhc_date_v1 = convert_string_to_date(LHC_Date),
      calc_temp_lhc_date_v2 = convert_string_to_date(LHC_Attendance),                   # get dates recorded in the attendance column
      calc_temp_lhc_date_v3 = as.Date(strtoi(LHC_Date), origin = '1899-12-30'),         # get dates recorded in Excel's date format
      calc_lhc_date = coalesce(
        calc_temp_lhc_date_v1, 
        calc_temp_lhc_date_v2, 
        calc_temp_lhc_date_v3
      ),                                                                                # get the date
      
      # calculate month of key dates
      calc_lhc_date_yearmon = as.yearmon(calc_lhc_date),
      calc_date_stopped_smoking_yearmon = as.yearmon(calc_date_stopped_smoking),
      
      # tidy up attendance where dates and attendances were transposed
      calc_temp_lhc_attendance_v1 = case_when(
        !is.na(calc_temp_lhc_date_v2) ~ LHC_Date                                        # where transposed then take the value in the date field
      ),
      calc_lhc_attendance = coalesce(
        calc_temp_lhc_attendance_v1,
        LHC_Attendance
      ),                                                                                # get the attendance
      
      # flag lhc attendances  
      calc_lhc_attendance_category = case_when(
        toupper(calc_lhc_attendance) %in% c(
          'ATTENDED',
          'PARTICIPANT ATTENDED BUT DID NOT COMPLETE LHC',
          'PARTICIPANT ATTENDED LHC',
          'PATICIPANT_ATTEND_LHC',
          'SCREENING - HEALTH CHECK (XM1XS)',
          'FINISHED'
        ) ~ 'Attended',
        toupper(calc_lhc_attendance) %in% c(
          'DID NOT ATTEND',
          'DIDNOTATTEND',
          'PARTICIPANT DID NOT ATTEND LHC',
          'PATICIPANT_DID_NOT_ATTEND_LHC'
        ) ~ 'DNA',
        is.na(calc_lhc_attendance) ~ 'NULL response',
        TRUE ~ 'NOT CODED'
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
      
      
    )
  
  return(df)
}

#' Process the Other History table ---------------------------------------------
#'
#' @param df tibble containing the raw other history table
#'
#' @return df
process_otherhistory <- function(df) {
  df <- df |> 
    mutate(
      # convert string dates to dates
      calc_cancer_date = convert_string_to_date(Cancer_Date)
    )
  return(df)
}

#' Process the Diagnostics table -----------------------------------------------
#'
#' @param df tibble containing the diagnostics history table
#'
#' @return df
process_diagnostics <- function(df) {
  
  df <- df |> 
    mutate(
      # convert string dates to dates
      calc_pet_ct_date = convert_string_to_date(`PET-CT_Date`),
      calc_bronchoscopy_date = convert_string_to_date(Bronchoscopy_Date),
      calc_ebus_date = convert_string_to_date(EBUS_Date),
      calc_eus_date = convert_string_to_date(EUS_Date),
      calc_mediastinoscopy_date = convert_string_to_date(Mediastinoscopy_Date),
      calc_lung_biospy_date = convert_string_to_date(Lung_Biopsy_Date),
      calc_diagnostic_complication_date = convert_string_to_date(Diagnostic_Complication_Date),
      calc_pathology_report_date = convert_string_to_date(Pathology_Report_Date),
      calc_cancer_diagnosis_date = convert_string_to_date(Cancer_Diagnosis_Date),
      calc_treatment_start_date = convert_string_to_date(Treatment_Start_Date),
      
      # convert key dates to yearmon dimensions
      calc_cancer_diagnosis_date_yearmon = as.yearmon(calc_cancer_diagnosis_date),
    )
  
  return(df)
}

#' Process the Invites table ---------------------------------------------------
#'
#' @param df tibble containing the invites table
#'
#' @return df
process_invites <- function(df) {
  # convert string dates to date-types
  df <- df |> 
    mutate(
      # convert string dates to dates
      calc_first_letter_date = convert_string_to_date(First_Letter_Date),
      calc_second_letter_date = convert_string_to_date(Second_Letter_Date),
      calc_follow_up_call_date = convert_string_to_date(Follow_Up_Call_Date),
      calc_contact_date = convert_string_to_date(Contact_Date),
      
      # calculate month of key dates
      calc_first_letter_date_yearmonth = as.yearmon(calc_first_letter_date),
      calc_second_letter_date_yearmonth = as.yearmon(calc_second_letter_date),
      calc_follow_up_call_date_yearmonth = as.yearmon(calc_follow_up_call_date),
      
      # calculate eligibility for lhc
      calc_eligible = case_when(
        toupper(Invite_Outcome) %in% c(
          'PARTICIPANT DOES NOT MEET AGE CRITERIAA',
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
      ),
      
      # work out when the invite was outcomed
      calc_invite_outcome_date_old = coalesce(
        calc_contact_date, 
        calc_follow_up_call_date, 
        calc_second_letter_date, 
        calc_first_letter_date
      ),
      calc_invite_outcome_date_yearmon_old = as.yearmon(calc_invite_outcome_date_old),
      
      # work out when the invite was outcomed
      # version 2 using the maximum date of these fields instead of coalescing
      calc_invite_outcome_date = pmax(
        calc_contact_date, 
        calc_follow_up_call_date, 
        calc_second_letter_date, 
        calc_first_letter_date,
        na.rm = T
      ),
      calc_invite_outcome_date_yearmon = as.yearmon(calc_invite_outcome_date)
      
    )
  return(df)
}

#' Process the LDCT table ------------------------------------------------------
#'
#' @param df tibble containing the LDCT table
#'
#' @return df
process_ldct <- function(df) {
  # convert string dates to date-types
  df <- df |>
    mutate(
      # convert string dates to dates
      calc_temp_ldct_date = convert_string_to_date(LDCT_Date), # this needs further processing to tidy up 
      calc_ldct_report_date = convert_string_to_date(LDCT_ReportDate1),
      calc_date_referral_lung_cancer = convert_string_to_date(Date_Referral_Lung_Cancer),
      calc_referral_date_to_tuberculosis = convert_string_to_date(Referral_Date_To_Tuberculosis),
      calc_referral_date_to_secondary_care_respiratory = convert_string_to_date(Referral_Date_To_Secondary_Care_Respiratory),
      calc_date_referralothercancer = convert_string_to_date(Date_ReferralOtherCancer),
      calc_referral_date_to_secondary_care_other_noncancer_team = convert_string_to_date(`Referral_Date_To_Secondary_Care-Other_Non-Cancer_Team`),
      calc_date_referralprimarycare = convert_string_to_date(Date_ReferralPrimaryCare),
      calc_date_lhc_letter_sent_to_patient = convert_string_to_date(Date_LHC_Letter_Sent_To_Patient),
      calc_date_lhc_results_letter_sent_to_gp = convert_string_to_date(Date_LHC_Results_Letter_Sent_To_GP),
      
      # tidy up some Corby data where dates and outcome were transposed
      calc_temp_ldct_date_v2 = convert_string_to_date(LDCT_Outcome),                      # get dates recorded in the outcome column
      calc_temp_ldct_date_v3 = as.Date(strtoi(LDCT_Date), origin = '1899-12-30'),         # get dates recorded in Excel's date format
      calc_ldct_date_corrected = coalesce(
        calc_temp_ldct_date, 
        calc_temp_ldct_date_v2, 
        calc_temp_ldct_date_v3
      ),                                                                                  # get the date
      
      # calculate month of key dates
      calc_ldct_date_corrected_yearmon = as.yearmon(calc_ldct_date_corrected),
      calc_date_referral_lung_cancer_yearmon = as.yearmon(calc_date_referral_lung_cancer),
      
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
      ),                                                                                   # classify outcomes into groups
      
      # flag incidental findings
      calc_incidental_consolidation = str_detect(toupper(Other_Incidental_Findings), 'CONSOLIDATION'),
      calc_incidental_tuberculosis = str_detect(toupper(Other_Incidental_Findings), 'TUBERCULOSIS'),
      calc_incidental_mediastinalmass = str_detect(toupper(Other_Incidental_Findings), 'MEDIASTINAL MASS'),
      calc_incidental_coronarycalcification = str_detect(toupper(Other_Incidental_Findings), 'CORONARY CALCIFICATION'),
      calc_incidental_aorticvalvecalcification = str_detect(toupper(Other_Incidental_Findings), 'AORTIC VALVE CALCIFICATION'),
      calc_incidental_thoracicaorticaneurysm = str_detect(toupper(Other_Incidental_Findings), 'THORACIC AORTIC ANEURYSM'),
      calc_incidental_pleuraleffusions = str_detect(toupper(Other_Incidental_Findings), 'PLEURAL EFFUSIONS/THICKENING'),
    )
  
  # work out sequence of scans - requires a temp table calculating which to be joined to the main table afterwards
  # !! NB REQUIRES FILTER ON LDCT DATE, the following relies on at least one ldct date being non-NA
  ## NNB, this is also computationally expensive so using the dtplyr lazy_dt to handle processing and transforming back to tibble
  ## afterward.
  df_temp <- lazy_dt(df) |> 
    select(ParticipantID, calc_ldct_date_corrected) |> # small subset
    filter(!is.na(calc_ldct_date_corrected)) |> # the following process requires dates
    unique() |> # handle cases where same dates have been uploaded in different formats (e.g.15/09/2021 and 2021-09-15 00:00:00)
    group_by(ParticipantID) |> 
    mutate(
      calc_temp_ldct_date_first = min(calc_ldct_date_corrected, na.rm = T), # get the date of the first scan
      calc_ldct_date_corrected_days_from_first = (calc_ldct_date_corrected - calc_temp_ldct_date_first), # days since first scan
      calc_ldct_date_corrected_category = cut(
        x = as.numeric(calc_ldct_date_corrected_days_from_first),
        breaks = c(-Inf, -1, 1, 61, 152, 335, 456, 669, 850, 1005, 1275, Inf),
        labels = c(
          'Not classified',          # -Inf to -1
          'Initial scan',            # -1 to 1 days --
          'Not classified',          # 2 to 61 days
          '3 month follow-up scan',  # 61 to 152 days --
          'Not classified',          # 153 to 335 days
          '12 month follow-up scan', # 335 to 465 days --
          'Not classified',          # 456 to 669 days
          '24 month follow-up scan', # 669 to 850 days --
          'Not classified',          # 850 to 1005 days
          '48 month follow-up scan', # 1005 to 1275 days --
          'Not classified'          # above 1275 days
        )
      ),
      calc_ldct_date_corrected_sequence = row_number() # whether the scan is the 1st, 2nd, 3rd, etc in the sequence
    ) |> 
    ungroup() |> 
    as_tibble()
  
  # add these details to the main table
  df <- left_join(
    x = df,
    y = df_temp,
    by = c('ParticipantID', 'calc_ldct_date_corrected')
  )
  rm(df_temp) # housekeeping
  
  return(df)
}

#' Process the Measurements table ----------------------------------------------
#'
#' @param df tibble containing the Measurement table
#'
#' @return df
process_measurement <- function(df) {
  # convert string dates to date-types
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
  
  df <- df |> 
    mutate(
      
      # convert string dates to dates
      calc_date_offered_smoking_cessation = convert_string_to_date(Date_Offered_Smoking_Cessation),
      calc_date_started_smoking_cessation = convert_string_to_date(`Date_Started-Smoking_Cessation`),
      calc_date_ended_smoking_cessation = convert_string_to_date(Date_Ended_Smoking_Cessation),
      
      # convert to yearmon dimensions
      calc_date_offered_smoking_cessation_yearmon = as.yearmon(calc_date_offered_smoking_cessation),
      calc_date_started_smoking_cessation_yearmon = as.yearmon(calc_date_started_smoking_cessation),
      calc_date_ended_smoking_cessation_yearmon = as.yearmon(calc_date_ended_smoking_cessation),
      
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
  if(str_table == 'tbTLHCTLHC_Demographics') {df <- process_demographics(df = df)} 
  else if(str_table == 'tbTLHCTLHC_LungHealthCheck') {df <- process_lhc(df = df)}
  else if(str_table == 'tbTLHCTLHC_OtherHistory') {df <- process_otherhistory(df = df)}
  else if(str_table == 'tbTLHCTLHC_Pathway_Invite') {df <- process_invites(df = df)} 
  else if(str_table == 'tbTLHCTLHC_Pathway_LDCT') {df <- process_ldct(df = df)}
  else if(str_table == 'tbTLHCTLHC_Measurements') {df <- process_measurement(df = df)}
  else if(str_table == 'tbTLHCTLHC_SmokingCessation') {df <- process_smoking(df = df)}
  else if(str_table == 'tbTLHCTLHC_Pathway_Diagnostics') {df <- process_diagnostics(df = df)}
  
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
update_user(message = 'Processing SQL tables, please wait ...', icon = '⏱️')

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
  
  # call function to download table data
  df_table_details <- df_table_details |>
    mutate(
      data = future_pmap(
        .l = list(table),
        .f = manage_table_processing,
        .options = furrr_options(seed=NULL)
      )
    )
  
})



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