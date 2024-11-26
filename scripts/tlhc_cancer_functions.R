#' -----------------------------------------------------------------------------
#' CANCER FUNCTIONS
#' 
#' Functions useful in the preparation of cancer data and using it with TLHC
#' activity
#' -----------------------------------------------------------------------------

# libraries ----
library(tidyverse)
library(readxl)

# udf ----
source(here('scripts', 'tlhc_general_functions.R'))

# collate a summary
create_people_count <- function() {
  return(
    tibble(
      step = character(),
      people = integer(),
      perc = character(),
      .rows = 0
    )
  )
}

update_people_count <- function(df_summary, df_people, description) {
  df_summary <- df_summary |> 
    add_row(
      step = description,
      people = df_people$ParticipantID |> unique() |> length()
    ) |> 
    mutate(
      perc = scales::percent(people / first(people), accuracy = 0.01)
    )
}

# people-events
get_people_events <- function() {
  
  # get reference information for each project
  df_reference <- read_excel(path = here('data', 'reference', 'project_reference.xlsx'))
  
  # load other history table
  if(!exists('df_oh')){
    df_oh <- readRDS(file = here('data', 'tlhc', 'calc_tbTLHCTLHC_OtherHistory.Rds')) |> 
      calculate_project_name()
  }
  update_user(message = 'Setup complete')
  
  # prepare an empty summary table of figures
  temp_summary <- create_people_count()
  
  ## people 1 - participants from all tables
  people <- bind_rows(
    df_invites |> select(ParticipantID, project) |> mutate(table = 'invite'),
    df_oh |> select(ParticipantID, project) |> mutate(table = 'oh'),
    df_lhc |> select(ParticipantID, project) |> mutate(table = 'lhc'),
    df_meas |> select(ParticipantID, project) |> mutate(table = 'meas'),
    df_ldct |> select(ParticipantID, project) |> mutate(table = 'ldct'),
    df_diag |> select(ParticipantID, project) |> mutate(table = 'diag'),
    df_smoking |> select(ParticipantID, project) |> mutate(table = 'smoking')
  ) |> 
    #distinct() |>
    filter(
      # only include participants from phase 1 & projects
      project %in% df_reference$project[df_reference$phase %in% c('Original', 'Onboarded')],
      # only work with valid participant ids
      !nchar(ParticipantID) < 60 & !is.numeric(ParticipantID),
      # only keep records found in the demographics table
      ParticipantID %in% df_demo$ParticipantID
    ) |> 
    arrange(ParticipantID)
  
  # how many unique people
  temp_summary <- update_people_count(temp_summary, people, 'people 1 - participants from all tables')
  update_user(message = 'Got unique list of people from all tables in phase 1&2')
  
  ## invites 1 - dates from invites ----
  people_invites1 <- df_invites |>
    # limit to our pool of people
    filter(ParticipantID %in% people$ParticipantID) |> 
    # select participant and date fields NB, excludes datetime like received date, etc.
    select(ParticipantID, where(is.Date)) |>
    # pivot longer so all dates are in one column
    pivot_longer(
      cols = -any_of(c('ParticipantID', 'event_outcome')),
      names_to = 'event',
      values_to = 'date'
    ) |> 
    filter(
      !is.na(date), # exclude blanks
      date >= as.Date('2019-04-01'), # exclude dates with DQ issues
      date < as.Date('2024-04-01') # exclude dates with DQ issues
    ) |> 
    # name all events 'Invite date'
    mutate(event = 'Invite date')
  
  # how many people?
  temp_summary <- update_people_count(temp_summary, people_invites1, 'people invites 1 - dates from invites')
  
  
  ## invites 2 - dates inferred from lhc ----
  people_invites2 <- df_lhc |> 
    filter(
      # limit to our pool of people
      ParticipantID %in% people$ParticipantID,
      # only add dates where there is no first letter date
      is.na(calc_first_letter_date)
    ) |> 
    # select participant and date fields
    select(ParticipantID, calc_lhc_date) |> 
    # pivot longer to match first table
    pivot_longer(
      cols = -any_of(c('ParticipantID', 'event_outcome')),
      names_to = 'event',
      values_to = 'date'
    ) |> 
    filter(
      !is.na(date), # exclude blanks
      date >= as.Date('2019-04-01'), # exclude dates with DQ issues
      date < as.Date('2024-04-01') # exclude dates with DQ issues
    ) |> 
    # name all events 'Invite date'
    mutate(event = 'Invite date')
  
  # how many people?
  temp_summary <- update_people_count(temp_summary, people_invites2, 'people invites 2 - dates inferred from lhc')
  
  ## invites 3 - combine ----
  people_invites <- bind_rows(
    people_invites1,
    people_invites2
  ) |> 
    # take the earliest date as the invite date, NB, arrange > distinct is MUCH faster than slice_min
    arrange(date) |> 
    distinct(ParticipantID, .keep_all = T)
  
  # how many people?
  temp_summary <- update_people_count(temp_summary, people_invites, 'people invites - combined count')
  update_user(message = 'Got dates of first invites for these people')
  
  ## lhc 1 - LHC dates ----
  people_lhc1 <- df_lhc |> 
    filter(
      # limit to our pool of people
      ParticipantID %in% people$ParticipantID,
      # countable lhc filters
      calc_valid_transactionid == 'Valid', # valid transactions
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_lhc_date_yearmon), # exclude records without a LHC date
      calc_lhc_attendance_category == 'Attended', # participant is recorded as attending
      # exclude dates with dq issues
      calc_lhc_date >= as.Date('2019-04-01'), # exclude dates with DQ issues
      calc_lhc_date < as.Date('2024-04-01') # exclude dates with DQ issues
    ) |> 
    # select participant and date fields
    select(ParticipantID, date = calc_lhc_date) |> 
    distinct() |>
    # prepare fields
    mutate(event = 'LHC Date') |> 
    # select the first attended where multiples exist
    arrange(date) |> 
    distinct(ParticipantID, .keep_all = T)
  
  # how many people?
  temp_summary <- update_people_count(temp_summary, people_lhc1, 'people lhc 1 - lhc dates')
  
  
  ## lhc 2 - LHC dates inferred from LDCT dates ----
  people_lhc2 <- df_ldct |> 
    filter(
      # limit to our pool of people
      ParticipantID %in% people$ParticipantID,
      # countable ldct filters
      calc_valid_transactionid == 'Valid', # valid transactions
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_ldct_date_corrected_yearmon), # exclude records without a LDCT date
      # exclude dates with dq issues
      calc_lhc_date >= as.Date('2019-04-01'), # exclude dates with DQ issues
      calc_lhc_date < as.Date('2024-04-01'), # exclude dates with DQ issues
      # keep only those not appearing in lhc1
      !ParticipantID %in% people_lhc1$ParticipantID
    ) |> 
    # select participant and date fields
    select(ParticipantID, date = calc_ldct_date_corrected) |> 
    distinct() |> 
    # prepare fields
    mutate(event = 'LHC Date') |> 
    # select the first date where multiples exist
    arrange(date) |> 
    distinct(ParticipantID, .keep_all = T)
  
  # how many people?
  temp_summary <- update_people_count(temp_summary, people_lhc2, 'people lhc 2 - lhc dates inferred from LDCT dates')
  
  ## lhc 3 - combine ----
  people_lhc <- bind_rows(
    people_lhc1,
    people_lhc2
  ) |> 
    # take the earliest date as the invite date, NB, arrange > distinct is MUCH faster than slice_min
    arrange(date) |> 
    distinct(ParticipantID, .keep_all = T) |> 
    # add in risk score result
    left_join(
      y = df_meas |> 
        mutate(
          risk_result = case_when(
            calc_LLPv2_risk_group == 'High risk' ~ 'High risk',
            calc_PLCOm2012_risk_group == 'High risk' ~ 'High risk',
          )
        ) |> 
        select(ParticipantID, risk_result),
      by = 'ParticipantID'
    ) |> 
    mutate(
      # capture people with no record in meas
      risk_result = case_match(
        risk_result,
        'High risk' ~ 'high risk',
        .default = 'low risk'
      ),
      event = paste(event, risk_result)
    ) |> 
    select(-risk_result)
  
  # how many people?
  temp_summary <- update_people_count(temp_summary, people_lhc, 'people lhc - combined')
  update_user(message = 'Got dates of LHCs for these people')
  
  ## ldct 1 - ldct dates ----
  people_ldct <- df_ldct |> 
    filter(
      # limit to our pool of people
      ParticipantID %in% people$ParticipantID,
      # countable ldct filters
      calc_valid_transactionid == 'Valid', # valid transactions
      calc_valid_participantid == 'Valid', # participant ID is a valid pseudonymised format
      !is.na(calc_ldct_date_corrected_yearmon), # exclude records without a LDCT date
      calc_ldct_outcome_corrected_groups == 'LDCT performed', # we have confirmation the scan took place (i.e. exclude future booked)
      # exclude dates with dq issues
      calc_ldct_date_corrected >= as.Date('2019-04-01'), # exclude dates with DQ issues
      calc_ldct_date_corrected < as.Date('2024-04-01'), # exclude dates with DQ issues
    ) |> 
    # select participant and date fields
    select(ParticipantID, date = calc_ldct_date_corrected, event = calc_ldct_date_corrected_category) |> 
    mutate(event = case_match(
      event,
      'Not classified' ~ 'Other scan',
      .default = event
    )) |> 
    distinct() |> 
    arrange(ParticipantID, date)
  
  # how many people?
  temp_summary <- update_people_count(temp_summary, people_ldct, 'people ldct')
  update_user(message = 'Got dates of LDCTs for these people')
  
  ## combine all ----
  # get a single table of events
  events_temp <- bind_rows(
    people_invites,
    people_lhc,
    people_ldct
  ) |> 
    arrange(ParticipantID, date)
  
  # how many people?
  temp_summary <- update_people_count(temp_summary, events_temp, 'people events')
  
  # create our final table of people events by joining the people table with the events table
  people_events <- people |> 
    # count the occurence of projects by Participant - needed to get the most frequently occuring project
    add_count(project, ParticipantID) |> 
    # get high-level info per person
    summarise(
      projects = paste(project |> unique(), collapse = ', '), # comma-separated projects
      project_mode = project[n == max(n)][1], # most frequently occuring project (limited to 1 where there are ties)
      tables = paste(table |> unique(), collapse = ', '), # comma-separated tables
      .by = ParticipantID
    ) |> 
    # add in event dates as a join
    left_join(
      y = events_temp,
      by = 'ParticipantID'
    ) |> 
    arrange(ParticipantID, date, event) |> 
    mutate(
      date_ym = as.yearmon(date),
      pk_pe = consecutive_id(ParticipantID, event, date)
    )
  update_user(message = 'Got a big table of events for people')
  
  # result
  print(temp_summary)
  return(people_events)
}


get_people_event_diagnoses <- function() {
  
  # get the people-events table
  people_events <- get_people_events() |> 
    # convert the event to an ordered factor
    mutate(event = fct(event, levels = c(
      'Invite date',
      'LHC date',
      'LHC Date low risk',
      'Initial scan', # putting the scan before LHC so this is the associated event for any diagnosis
      'LHC Date high risk',
      '3 month follow-up scan',
      '12 month follow-up scan',
      '24 month follow-up scan',
      '48 month follow-up scan',
      '2-year+ nodule surveillance',
      'Other scan'
    )))
  
  # link people-events with diagnoses ---
  update_user(message = 'Linking people-events with cancer diagnoses')
  
  # get a list of unique Participant-event dates
  temp_pe <- people_events |> 
    select(pk_pe, ParticipantID, date, event) |> 
    arrange(ParticipantID, date, event) |> 
    distinct(ParticipantID, date, .keep_all = T)
  
  # get a list of diagnosis dates
  temp_canc <- df_cancer |> 
    select(pk, ParticipantID, diagnosis_date) |> 
    distinct() |> 
    mutate(diagnosis_date = diagnosis_date |> as.Date())
  
  # get a lookup between event to diagnosis
  temp_event_diag_lu <- temp_pe |> 
    left_join(
      y = temp_canc,
      by = 'ParticipantID',
      relationship = 'many-to-many' # expecting one cancer to link with all events, and there may be multiple cancers per person
    ) |> 
    # exclude people-event records without a cancer match
    filter(!is.na(pk)) |> 
    # work out difference between the two dates
    mutate(days_diff = clock::date_count_between(start = date, end = diagnosis_date, precision = 'day')) |> 
    # remove negative differences
    filter(days_diff >= 0) |>
    # arrange by diagnosis primary key and days_diff in order to take the nearest event leading up to the diagnosis
    arrange(pk, days_diff) |> 
    distinct(pk, .keep_all = T) |> 
    # get the lookup
    select(pk_pe, pk, days_diff) |> 
    unique()
  
  # create a people-event-diagnosis table
  people_events_diagnoses <- people_events |> 
    # add in the lookup to diagnosis
    left_join(
      y = temp_event_diag_lu,
      by = 'pk_pe'
    ) |> 
    # add in diagnosis details
    left_join(
      y = df_cancer |> select(-ParticipantID),
      by = 'pk'
    ) |> 
    # flag diagnoses made at LHC or later
    mutate(
      flag_diag_lhc_or_later = case_when(
        !is.na(pk) & !event %in% c('Invite date', 'LHC Date low risk') ~ TRUE,
        !is.na(pk) & event %in% c('Invite date', 'LHC Date low risk') ~ FALSE
      )
    )
  
  return(people_events_diagnoses)
}

#people_events_diagnoses <- get_people_event_diagnoses()
