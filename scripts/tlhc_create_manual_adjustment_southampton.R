#'------------------------------------------------------------------------------
#' SOUTHAMPTON INVITE MANUAL ADJUSTMENTS
#' 
#' BACKGROUND
#' Southampton's invites are fine up to July 2022 at which point they had
#' covered all their eligible population.
#' 
#' From August 2022 onward they started re-contacting patients who hadn't 
#' responded. These new records overwrite the First_Letter_Date for these
#' patients so it looks like are being contacted for the first time much later
#' than they really were.
#' 
#' The solution is to freeze initial invites up to July 2022 (handled in the
#' download_sql.R script) and to manually adjust for the newer follow-up
#' invitations.
#' 
#' This script handles the downloading and creation of a manual adjustment file
#' for Southampton records
#' 
#'------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(tidyverse)     # tidy data processing
library(here)          # localise file references
library(lubridate)     # date functions
library(zoo)           # date functions
library(DBI)           # sql connection
library(dbplyr)        # tidy sql processing
library(tictoc)        # process timing

source(here('scripts', 'tlhc_general_functions.R'))

# Notify user 
update_user(stage = 'start', message = 'tlhc_create_manual_adjustment_southampton.R')
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
      calc_valid_participantid = case_when(
        nchar(ParticipantID) < 15 ~ 'Invalid', # length is too short
        is.numeric(ParticipantID) ~ 'Invalid', # not an alphanumeric pseudo_NHS_number
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
      calc_invite_outcome_date = coalesce(
        calc_contact_date, 
        calc_follow_up_call_date, 
        calc_second_letter_date, 
        calc_first_letter_date
      ),
      calc_invite_outcome_date_yearmon = as.yearmon(calc_invite_outcome_date)
      
    )
  return(df)
}
# Setup ------------------------------------------------------------------------
# set up connection to the tlhc sql database
con <- dbConnect(
  odbc::odbc(),
  .connection_string = "Driver={SQL Server};SERVER=MLCSU-BI-SQL;DATABASE=TLHC_Reporting"
)

## project lookups (used as reference) ----
df_projectlu <- readRDS(file = here('data', 'tlhc', 'dboProjectLookup.Rds')) |> ungroup()

# Download the data ------------------------------------------------------------
# define transactions to ignore
invalid_transid <- c(
  
  # Define a list of Southampton's transactions that we want to ignore - i.e. these are the 'good data':
  178804, 174481, 171602, 167090, 167088, 167087, 166867, 164023, 164015, 
  164014, 159485, 159394, 159292, 156700, 154479, 154473, 150947, 150447, 
  148966, 147877, 144644, 144477, 142109, 142106, 136150, 134522, 131883, 
  130492, 127555, 127554, 127553, 127552, 127539, 127538, 127535, 127534, 
  127532, 127401, 119536, 118219, 105757, 105752, 103920, 102635
  
)

# download the data
df <- tbl(con, in_schema('dbo', 'tbTLHCTLHC_Pathway_Invite')) |> # lazy load
  filter(substr(SubmittedZipFile, 1, 5) == 'RHM00') |> # only use Southampton's data
  filter(!TransactionId %in% invalid_transid) |> # ignore invalid transactions
  group_by(ParticipantID) |> # get one record for each participant
  slice_max(ReceivedDate) |> # get record(s) with the latest datetime received
  slice_max(CSURowNumber) |> # get record(s) with the highest CSU row number
  filter(row_number(ParticipantID)==1) |> # get the first row where multiples still exist
  collect() # download the data

rm(invalid_transid)
dbDisconnect(con)

# Process the data -------------------------------------------------------------
df <- df |> ungroup()
df <- process_alltables(df = df) # identify the project, when received, etc
df <- process_invites(df = df) # invite outcome, date parsing, etc.

# Produce a manual adjustment file ---------------------------------------------

## Follow-up invitations ----------
# combine both first and second invites to the same field and output a simple
# dataframe of Participant and follow-up invite month

# follow-up invitations
df_southampton_followups <- bind_rows(
  # follow-ups which overwrite the first letter dates 
  df |> 
    # ensure records for Aug 2022 onwards are counted
    filter(calc_first_letter_date >= make_date(2022,08,01)) |> 
    # simplify
    select(ParticipantID, month = calc_first_letter_date_yearmonth),
  
  # follow-ups recorded in the second letter date
  df |> 
    # ensure records for Aug 2022 onwards are counted
    filter(calc_second_letter_date >= make_date(2022,08,01)) |> 
    # simplify
    select(ParticipantID, month = calc_second_letter_date_yearmonth)
)

# summarise the number of invitations per month
df_fu <- df_southampton_followups |> 
  group_by(month) |> 
  summarise(metric_1c_followup_invitations = n_distinct(ParticipantID)) |> 
  ungroup()

## accepted invitations -----

# filter records the same way for metric 2 then
df_accep <- df |> 
  filter(
    calc_valid_transactionid == 'Valid', # valid transactions
    calc_eligible == 'Eligible', # eligible for the lhc (age group and smoking status)
    calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
    calc_invite_accepted == 'Accepted', # confirmation the invite was accepted
    !is.na(calc_invite_outcome_date_yearmon)
  ) |> 
  # get the month to report this metric by
  mutate(month = pmax(calc_first_letter_date_yearmonth, calc_second_letter_date_yearmonth, na.rm = T)) |> 
  select(ParticipantID, month, calc_first_letter_date_yearmonth, calc_second_letter_date_yearmonth) |> 
  group_by(month) |> 
  summarise(metric_2_accepted_invitations = n_distinct(ParticipantID)) |> 
  ungroup()

# combine both to a single object
df_southampton_ma <- full_join(
  x = df_fu,
  y = df_accep,
  by = 'month'
) |> 
  arrange(month) |> 
  # limit to Aug 2022 onward
  filter(month >= 'Aug 2022')
  
view(df_southampton_ma)

# open the manual adjustment file for editing
browseURL(file.path(Sys.getenv('base_365'),'Monthly MI reporting', 'Data processing procedure', 'tlhc_manual_adjustments.xlsx'))

# done!
update_user(stage = 'end')
toc()