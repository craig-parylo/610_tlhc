
# libraries --------------------------------------------------------------------
library(tidyverse)
library(plotly)
library(ggalluvial)
library(glue)
library(tidygraph)
library(scales)

# data -------------------------------------------------------------------------
# get some data
df_test <- df_metric_1a_invites_first

# adjust columns ----
df <- df_test |> 
  # select some fields
  select(
    ParticipantID,
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
    )
  ) |> 
  # simplify
  select(
    ParticipantID,
    calc_eligible,
    calc_invite_1,
    calc_invite_2,
    calc_invite_3,
    calc_invite_outcome
  )

# summary table ---------
# produce summary tables
df_summary <- bind_rows(
  # Invitation 1 
  df |> 
    select(ParticipantID, origin = calc_eligible, destination = calc_invite_1) |> 
    filter(!is.na(origin), !is.na(destination)) |> 
    group_by(origin, destination) |> 
    summarise(flow = n_distinct(ParticipantID, na.rm = T), .groups = 'drop_last') |> 
    ungroup(),
  
  df |> 
    filter(!is.na(calc_eligible), is.na(calc_invite_1)) |> 
    select(ParticipantID, origin = calc_eligible, destination = calc_invite_outcome) |> 
    group_by(origin, destination) |> 
    summarise(flow = n_distinct(ParticipantID, na.rm = T), .groups = 'drop_last') |> 
    ungroup(),
  
  # Invitation 2
  df |>
    select(ParticipantID, origin = calc_invite_1, destination = calc_invite_2) |>
    filter(!is.na(origin), !is.na(destination)) |>
    group_by(origin, destination) |>
    summarise(flow = n_distinct(ParticipantID, na.rm = T), .groups = 'drop_last') |>
    ungroup(),
  
  df |> 
    filter(!is.na(calc_invite_1), is.na(calc_invite_2)) |> 
    select(ParticipantID, origin = calc_invite_1, destination = calc_invite_outcome) |> 
    group_by(origin, destination) |> 
    summarise(flow = n_distinct(ParticipantID, na.rm = T), .groups = 'drop_last') |> 
    ungroup(),

  # Invitation 3
  df |>
    select(ParticipantID, origin = calc_invite_2, destination = calc_invite_3) |>
    filter(!is.na(origin), !is.na(destination)) |>
    group_by(origin, destination) |>
    summarise(flow = n_distinct(ParticipantID, na.rm = T), .groups = 'drop_last') |>
    ungroup(),
  
  df |> 
    filter(!is.na(calc_invite_2), is.na(calc_invite_3)) |> 
    select(ParticipantID, origin = calc_invite_2, destination = calc_invite_outcome) |> 
    group_by(origin, destination) |> 
    summarise(flow = n_distinct(ParticipantID, na.rm = T), .groups = 'drop_last') |> 
    ungroup(),
  
  df |> 
    filter(!is.na(calc_invite_3)) |> 
    select(ParticipantID, origin = calc_invite_3, destination = calc_invite_outcome) |> 
    group_by(origin, destination) |> 
    summarise(flow = n_distinct(ParticipantID, na.rm = T), .groups = 'drop_last') |> 
    ungroup(),
  
)

## tidygraph ----
# get a tidy graph object
tg <- df_summary |> as_tbl_graph()

test_tg <- df |> as_tbl_graph()

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
      str_detect(name, 'No response') ~ '#7f8fa6',
      str_detect(name, 'GP eligible population') ~ '#7f8fa6',
      
      # green
      str_detect(name, 'Accepted') ~ '#44bd32',
      
      # red
      str_detect(name, 'Declined') ~ '#c23616',
      
      # amber
      str_detect(name, 'Ineligible') ~ '#e1b12c'
    ),
    
    colour_fade = col2hcl(colour = colour, alpha = 0.3)
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
  
  node = list(
    label = nodes$name,
    color = nodes$colour
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
    title = 'Sankey diagram of all TLHC invites',
    font = list(
      family = 'arial, sans-serif',
      size = 14
    ),
    paper_bgcolor = 'rgba(0,0,0,0)'
  )
