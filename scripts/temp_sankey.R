
# libraries --------------------------------------------------------------------
library(tidyverse)
library(plotly)
library(ggalluvial)
library(glue)
library(tidygraph)
library(scales)
library(dtplyr)

source(here('scripts', 'tlhc_metric_functions.R'))
source(here('scripts', 'tlhc_general_functions.R'))

#' Get data for Sankey
#'
#' @return Tibble of data ready for use with Sankey plot
#' @export
#'
#' @examples
get_data_for_sankey <- function() {
  
  # get demographic data ----
  update_user(message = 'Getting demographic data ...', icon = '⏱️')
  df_sankey_demo <- load_df_demo() |> 
    select(
      ParticipantID,
      calc_lsoa_imd_decile,
      calc_lsoa_rurality_group_category,
      calc_ethnic_group,
      calc_sex,
      calc_age,
    )
  
  # get invite data ----
  update_user(message = 'Getting invite data ...', icon = '⏱️')
  df_sankey_invites <- get_df_metric_1a_invites_first() |> 
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
  
  # get lhc data ----
  update_user(message = 'Getting LHC data ...', icon = '⏱️')
  df_sankey_lhc <- load_df_lhc() |> 
    lazy_dt() |> 
    
    #lazy_dt(df_lhc) |> # using dtplyr to speed up the processing
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
  update_user(message = 'Getting measurement data ...', icon = '⏱️')
  df_sankey_meas <- load_df_measurements() |> 
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
  update_user(message = 'Getting ldct data ...', icon = '⏱️')
  df_sankey_ldct <- load_df_ldct() |> 
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
    as_tibble()
  
  # add ldct to the return
  df_return <- left_join(
    x = df_return,
    y = df_sankey_ldct,
    by = 'ParticipantID'
  )
  
  
  # return the result
  return(df_return)
  
}


get_flows_for_data <- function(df, from, to, outcome) {
  
  # where 'from' and 'to' are not NA
  df_from_to <- df |>
    select(ParticipantID, origin = !!from, destination = !!to) |> # select relevant fields
    mutate(ParticipantID, origin = as.character(origin), destination = as.character(destination)) |> # convert all to character
    filter(!is.na(origin), !is.na(destination)) |> # limit to where we have values in both
    group_by(origin, destination) |> # group
    summarise(flow = n_distinct(ParticipantID, na.rm = T), .groups = 'drop_last') |> # count participants
    ungroup() # ungroup
  
  # where 'to' is NA so flow is to outcome
  df_from_outcome <- df |>
    select(ParticipantID, origin = !!from, destination = !!to, outcome = !!outcome) |> # select relevant fields
    filter(!is.na(origin), is.na(destination)) |> # limit to where we have an origin but not destination values
    select(ParticipantID, origin, destination = outcome) |> # set the destination as the overall outcome
    mutate(ParticipantID, origin = as.character(origin), destination = as.character(destination)) |> # convert all to character
    group_by(origin, destination) |> # group
    summarise(flow = n_distinct(ParticipantID, na.rm = T), .groups = 'drop_last') |> # count participants
    ungroup()
  
  # bind rows together
  return(
    bind_rows(
      df_from_to,
      df_from_outcome
    ) |> 
      unique()
  )
  
}

#' Get a Sankey plot for the given data
#'
#' @param df Tibble of data ready for use
#'
#' @return Sankey plot
#' @export
#'
#' @examples
get_sankey_for_data <- function(df) {
  
  # summary table ---------
  # produce summary tables
  df_summary <- bind_rows(
    # Invitation 1 
    get_flows_for_data(
      df = df,
      from = 'calc_eligible',
      to = 'calc_invite_1',
      outcome = 'calc_invite_outcome'
    ),

    # Invitation 2
    get_flows_for_data(
      df = df,
      from = 'calc_invite_1',
      to = 'calc_invite_2',
      outcome = 'calc_invite_outcome'
    ),

    # Invitation 3
    get_flows_for_data(
      df = df,
      from = 'calc_invite_2',
      to = 'calc_invite_3',
      outcome = 'calc_invite_outcome'
    ),

    # Invite outcome
    get_flows_for_data(
      df = df,
      from = 'calc_invite_3',
      to = 'calc_invite_outcome',
      outcome = 'calc_invite_outcome'
    ),

    # LHC 1 - invite outcome to lhc delivery methods
    get_flows_for_data(
      df = df,
      from = 'calc_invite_outcome',
      to = 'calc_lhc_delivery_methods_all',
      outcome = 'calc_lhc_attendance_category_overall'
    ),

    # LHC 2 - lhc delivery methods to contact count
    get_flows_for_data(
      df = df,
      from = 'calc_lhc_delivery_methods_all',
      to = 'calc_lhc_sequence_max_group',
      outcome = 'calc_lhc_attendance_category_overall'
    ),

    # LHC 3 - contact count to lhc attendance status
    get_flows_for_data(
      df = df,
      from = 'calc_lhc_sequence_max_group',
      to = 'calc_lhc_attendance_category_overall',
      outcome = 'calc_lhc_attendance_category_overall'
    ),

    # meas - lhc attendance status to risk score
    get_flows_for_data(
      df = df,
      from = 'calc_lhc_attendance_category_overall',
      to = 'calc_risk_assessment',
      outcome = 'calc_ineligible_status'
    ),

    # meas - risk score to eligible for scan
    get_flows_for_data(
      df = df,
      from = 'calc_risk_assessment',
      to = 'calc_ineligible_status',
      outcome = 'calc_ineligible_status'
    ),

    # ldct - scans
    get_flows_for_data(
      df = df,
      from = 'calc_ineligible_status',
      to = 'calc_ldct_count_groups',
      outcome = 'calc_ldct_count_groups'
    ),

    
  ) |> 
    # convert NAs to a string
    mutate(
      destination = replace_na(data = destination, replace = 'Loss to follow up')
    )
  
  ## tidygraph ----
  # get a tidy graph object
  tg <- df_summary |> as_tbl_graph()
  
  # extract nodes and links
  nodes <- tg |> 
    activate(nodes) |> 
    data.frame() |> 
    mutate(
      # create an id with zero-base
      id = row_number()-1,
      
      # add colour definitions
      colour = case_when(
        
        # grey
        ( str_detect(name, 'No response') |
          str_detect(name, 'GP eligible population') |
          str_detect(name, 'Loss to follow up') |
          str_detect(name, 'not applicable')
        ) ~ '#7f8fa6',
        
        # green
        ( str_detect(name, 'Accepted') |
          str_detect(name, 'Attended') |
          str_detect(name, 'referred') |
          str_detect(name, 'High risk') |
          str_detect(name, 'scan')
        ) ~ '#44bd32',
         
        # red
        ( str_detect(name, 'Declined') |
          str_detect(name, 'DNA')
        ) ~ '#c23616',
        
        # amber
        ( str_detect(name, 'Ineligible') |
          str_detect(name, 'Incomplete') |
          str_detect(name, 'ineligible') |
          str_detect(name, 'Low risk')
        ) ~ '#e1b12c',

        # blue
        ( str_detect(name, 'Virtual') |
          str_detect(name, 'F2F') |
          str_detect(name, 'contacts')
        ) ~ '#0097e6'
      ),
      
      colour_fade = col2hcl(colour = colour, alpha = 0.3),
      
      ### positioning ----
      # define horizontal positions
      x = case_when(
        name %in% c('GP eligible population') ~ 1,
        name %in% c('Invite 1 Accepted', 'Invite 1 No response', 'Invite 1 Declined', 'Invite 1 Ineligible') ~ 2,
        name %in% c('Invite 2 Accepted', 'Invite 2 No response', 'Invite 2 Declined', 'Invite 2 Ineligible') ~ 3,
        name %in% c('Invite 3 Accepted', 'Invite 3 No response', 'Invite 3 Declined', 'Invite 3 Ineligible') ~ 4,
        name %in% c('Invite Accepted', 'Invite No response', 'Invite Declined', 'Invite Ineligible') ~ 5,
        name %in% c('Virtual', 'Virtual, F2F', 'F2F', 'F2F, Virtual') ~ 6,
        name %in% c('1 x contacts', '2 x contacts', '3+ contacts') ~ 7,
        name %in% c('LHC Attended', 'LHC DNA', 'LHC Incomplete') ~ 8,
        name %in% c('High risk', 'Low risk') ~ 9,
        name %in% c('LDCT: referred', 'LDCT: ineligible') ~ 10,
        name %in% c('1 x scan', '2 x scans', '3+ scans', 'Loss to follow up') ~ 11
      ),
      x = rescale(x, to = c(1e-09, 0.99)),
      
      # define vertical positions (1 = bottom, 0 = top)
      y = case_when(
        name %in% c(
          'Invite 1 Accepted', 'Invite Accepted', 'Virtual', '1 x contacts', 
          'LHC Attended', 'High risk', 'LDCT: referred', 'Initial scan', 
          '3 month follow-up scan', '1 x scan'
        ) ~ 0.0001,
        
        name %in% c(
          '2 x scans', '3+ scans'
        ) ~ 0.1,
        
        name %in% c(
          'Invite 2 Accepted', '2 x contacts', 'LHC Incomplete', 
          'LDCT: ineligible'
        ) ~ 0.2,
        
        name %in% c(
          'Invite 3 Accepted', 'F2F', 'Virtual, F2F', 'F2F, Virtual', 
          '3+ contacts', 'LHC DNA'
        ) ~ 0.33,
        
        name %in% c('Low risk') ~ 0.4,
        
        name %in% c('GP eligible population', 'Invite 1 No response') ~ 0.5,
        
        name %in% c('Loss to follow up') ~ 0.65,
        
        name %in% c('Invite 1 Declined') ~ 0.7,
        
        name %in% c('Invite 3 No response') ~ 0.71,
        
        name %in% c('Invite 3 Declined') ~ 0.8,
        
        name %in% c('Invite 1 Ineligible', 'Invite Ineligible') ~ 0.8,
        
        name %in% c('Invite 2 Declined') ~ 0.88,
        
        name %in% c('Invite 2 Ineligible', 'Invite 3 Ineligible') ~ 0.99
      )
    )
  
  
  links <- tg |> 
    activate(edges) |> 
    data.frame() |> 
    mutate(
      # make ids zero-based
      from = from - 1, 
      to = to - 1,
      
      # add a label
      label = number(flow, big.mark = ',')
    )
  
  # add destination colours to links
  links <- left_join(
    x = links,
    y = nodes |> select(to = id, colour_fade),
    by = 'to'
  ) 
  
  ## plotly ----
  # plot
  plot_ly(
    type = 'sankey',
    orientation = 'h',
    arrangement = 'snap',
    
    node = list(
      label = nodes$name,
      color = nodes$colour,
      x = nodes$x,
      y = nodes$y
    ),
    
    link = list(
      source = links$from,
      target = links$to,
      value = links$flow,
      label = links$label,
      color = links$colour_fade
    )
  ) |> 
    layout(
      title = 'Targeted Lung Health Checks',
      font = list(
        family = 'Arial, Helvetica, sans-serif',
        size = 11
      ),
      paper_bgcolor = 'rgba(0,0,0,0)'
    ) |> 
    config(
      toImageButtonOptions = list(
        format = 'svg', # one of png, svg, jpeg, webp
        filename = 'tlhc_sankey'
      )
    )
}


# test functions ---------------------------------------------------------------
df <- get_data_for_sankey()
get_sankey_for_data(df = df)
df |> 
  filter(project == 'Tameside and Glossop') |> 
  get_sankey_for_data()



