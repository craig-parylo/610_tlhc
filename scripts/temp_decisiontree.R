#' -----------------------------------------------------------------------------
#' Decision tree dataset
#' 
#' -----------------------------------------------------------------------------

# libraries --------------------------------------------------------------------
library(tidyverse)

# data -------------------------------------------------------------------------
# get some data
df <- df_metric_1a_invites_first |> 
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
    
    # count the number of contact attempts
    calc_contact_attempts = rowSums(!is.na(df_metric_1a_invites_first |> select(First_Letter_Date, Second_Letter_Date, Follow_Up_Call_Date)))
  ) |> 
  
  # simplify
  select(
    ParticipantID,
    calc_invite_outcome,
    calc_contact_attempts,
    project
  )

# add in some demographic details
df_decision_tree <- left_join(
  x = df,
  y = df_demo |> 
    select(
      ParticipantID,
      calc_age,
      calc_sex,
      calc_ethnic_group,
      calc_language,
      calc_lsoa_imd_decile,
      calc_lsoa_rurality_group_category,
      calc_marital_status,
    ),
  by = 'ParticipantID'
)

# ensure categories are all factors
df_decision_tree <- df_decision_tree |> 
  mutate(
    calc_invite_outcome = fct_explicit_na(factor(x = calc_invite_outcome, ordered = F), na_level = 'Not known'),
    project = fct_explicit_na(factor(x = project, ordered = F), na_level = 'Not known'),
    calc_sex = fct_explicit_na(factor(x = calc_sex, ordered = F), na_level = 'Not known'),
    calc_ethnic_group = fct_explicit_na(factor(x = calc_ethnic_group, ordered = F), na_level = 'Not known'),
    calc_lsoa_imd_decile = fct_explicit_na(factor(x = calc_lsoa_imd_decile, ordered = T), na_level = 'Not known'),
    calc_lsoa_rurality_group_category = fct_explicit_na(factor(x = calc_lsoa_rurality_group_category, ordered = F), na_level = 'Not known'),
    calc_marital_status = fct_explicit_na(factor(x = calc_marital_status, ordered = F), na_level = 'Not known')
  )
