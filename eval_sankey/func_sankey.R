
# libraries --------------------------------------------------------------------
library(tidyverse)
library(plotly)
#library(glue)
library(tidygraph)
library(scales)


# get aggregate flows of data when working with pre-aggregated
get_flows_for_data_aggregate <- function(df, from, to, outcome) {
  
  # where 'from' and 'to' are not NA
  df_from_to <- df |> 
    select(participants, origin = !!from, destination = !!to) |> # select relevant fields
    mutate(participants, origin = as.character(origin), destination = as.character(destination)) |> # convert all to character
    filter(!is.na(origin), !is.na(destination)) |> # limit to where we have values in both
    group_by(origin, destination) |> # group
    summarise(flow = sum(participants, na.rm = T), .groups = 'drop_last') |> # sum participants
    ungroup() # ungroup
  
  # where 'to' is NA so flow is to outcome
  df_from_outcome <- df |>
    select(participants, origin = !!from, destination = !!to, outcome = !!outcome) |> # select relevant fields
    filter(!is.na(origin), is.na(destination)) |> # limit to where we have an origin but not destination values
    select(participants, origin, destination = outcome) |> # set the destination as the overall outcome
    mutate(participants, origin = as.character(origin), destination = as.character(destination)) |> # convert all to character
    group_by(origin, destination) |> # group
    summarise(flow = sum(participants, na.rm = T), .groups = 'drop_last') |> # sum participants
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
    get_flows_for_data_aggregate(
      df = df,
      from = 'calc_eligible',
      to = 'calc_invite_1',
      outcome = 'calc_invite_outcome'
    ),
    
    # Invitation 2
    get_flows_for_data_aggregate(
      df = df,
      from = 'calc_invite_1',
      to = 'calc_invite_2',
      outcome = 'calc_invite_outcome'
    ),
    
    # Invitation 3
    get_flows_for_data_aggregate(
      df = df,
      from = 'calc_invite_2',
      to = 'calc_invite_3',
      outcome = 'calc_invite_outcome'
    ),
    
    # Invite outcome
    get_flows_for_data_aggregate(
      df = df,
      from = 'calc_invite_3',
      to = 'calc_invite_outcome',
      outcome = 'calc_invite_outcome'
    ),
    
    # LHC 1 - invite outcome to lhc delivery methods
    get_flows_for_data_aggregate(
      df = df,
      from = 'calc_invite_outcome',
      to = 'calc_lhc_delivery_methods_all',
      outcome = 'calc_lhc_attendance_category_overall'
    ),
    
    # LHC 2 - lhc delivery methods to contact count
    get_flows_for_data_aggregate(
      df = df,
      from = 'calc_lhc_delivery_methods_all',
      to = 'calc_lhc_sequence_max_group',
      outcome = 'calc_lhc_attendance_category_overall'
    ),
    
    # LHC 3 - contact count to lhc attendance status
    get_flows_for_data_aggregate(
      df = df,
      from = 'calc_lhc_sequence_max_group',
      to = 'calc_lhc_attendance_category_overall',
      outcome = 'calc_lhc_attendance_category_overall'
    ),
    
    # meas - lhc attendance status to risk score
    get_flows_for_data_aggregate(
      df = df,
      from = 'calc_lhc_attendance_category_overall',
      to = 'calc_risk_assessment',
      outcome = 'calc_ineligible_status'
    ),
    
    # meas - risk score to eligible for scan
    get_flows_for_data_aggregate(
      df = df,
      from = 'calc_risk_assessment',
      to = 'calc_ineligible_status',
      outcome = 'calc_ineligible_status'
    ),
    
    # ldct - scans
    get_flows_for_data_aggregate(
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
  
  # get the total eligible population
  temp_eligible_pop <- df_summary |> 
    filter(origin == 'GP eligible population') |> 
    summarise(total = sum(flow, na.rm = T)) |> 
    pull(total)
  
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
            str_detect(name, 'not applicable') |
            str_detect(name, 'Low risk')
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
            str_detect(name, 'DNA') |
            str_detect(name, 'LDCT: unknown') |
            str_detect(name, 'No attendance')
        ) ~ '#c23616',
        
        # amber
        ( str_detect(name, 'Ineligible') |
            str_detect(name, 'Incomplete') |
            str_detect(name, 'ineligible') |
            str_detect(name, 'No risk score')
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
        name %in% c('LHC Attended', 'LHC DNA', 'LHC Incomplete', 'No attendance') ~ 8,
        name %in% c('High risk', 'Low risk', 'No risk score') ~ 9,
        name %in% c('LDCT: referred', 'LDCT: ineligible', 'LDCT: unknown') ~ 10,
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
        
        name %in% c('LDCT: unknown', 'Invite 2 Accepted') ~ 0.15,
        
        name %in% c(
          '2 x contacts', 'LHC Incomplete', 
          'LDCT: ineligible', 'No risk score'
        ) ~ 0.2,
        
        name %in% c(
          'Invite 3 Accepted'
        ) ~ 0.3,
        
        name %in% c(
          'F2F', 'Virtual, F2F', 'F2F, Virtual', '3+ contacts', 'LHC DNA'
        ) ~ 0.33,
        
        name %in% c('LDCT: unknown', 'No attendance', 'Invite No response') ~ 0.4,
        
        name %in% c('Low risk') ~ 0.45,
        
        name %in% c('GP eligible population', 'Invite 1 No response') ~ 0.5,
        
        name %in% c('Loss to follow up') ~ 0.65,
        
        name %in% c('Invite 1 Declined') ~ 0.7,
        
        name %in% c('Invite 3 No response') ~ 0.61,
        
        name %in% c('Invite 3 Declined') ~ 0.7,
        
        name %in% c('Invite 1 Ineligible', 'Invite Declined') ~ 0.75,
        
        name %in% c('Invite 2 Declined', 'Invite 2 Declined') ~ 0.8,
        
        name %in% c('Invite 2 Ineligible', 'Invite 3 Ineligible', 'Invite Ineligible') ~ 0.99
      )
    ) |>
    left_join(
      y = df_summary |>
        group_by(destination) |> 
        summarise(
          flow = sum(flow, na.rm = T),
          flow_perc = scales::percent(flow / temp_eligible_pop, accuracy = 0.1)
        ) |> 
        select(name = destination, flow_perc),
       by = 'name'
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
    ) |> 
    mutate(
      flow_perc = scales::percent(flow / temp_eligible_pop, accuracy = 0.1)
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
      y = nodes$y,
      customdata = nodes$flow_perc,
      hovertemplate = '%{label}<br /><b>%{value}</b> participants<br /><b>%{customdata}</b> of GP eligible population'
    ),
    
    link = list(
      source = links$from,
      target = links$to,
      value = links$flow,
      label = links$label,
      color = links$colour_fade,
      customdata = links$flow_perc,
      hovertemplate = '%{source.label} → %{target.label}<br /><b>%{value}</b> participants<br /><b>%{customdata}</b> of GP eligible population'
    )
  ) |> 
    layout(
      #title = 'Targeted Lung Health Checks',
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
      ),
      responsive = TRUE
    )
}
