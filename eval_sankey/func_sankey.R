
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
    summarise(flow = sum(participants, na.rm = T), .by = c(origin, destination)) # sum participants 
  
  # where 'to' is NA so flow is to outcome
  df_from_outcome <- df |>
    select(participants, origin = !!from, destination = !!to, outcome = !!outcome) |> # select relevant fields
    filter(!is.na(origin), is.na(destination)) |> # limit to where we have an origin but not destination values
    select(participants, origin, destination = outcome) |> # set the destination as the overall outcome
    mutate(participants, origin = as.character(origin), destination = as.character(destination)) |> # convert all to character
    summarise(flow = sum(participants, na.rm = T), .by = c(origin, destination)) # sum participants

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
      #outcome = 'calc_ldct_count_groups'
    ),
    
    # LHC 2 - lhc delivery methods to contact count
    get_flows_for_data_aggregate(
      df = df,
      from = 'calc_lhc_delivery_methods_all',
      to = 'calc_lhc_sequence_max_group',
      outcome = 'calc_lhc_attendance_category_overall'
      #outcome = 'calc_ldct_count_groups'
    ),
    
    # LHC 3 - contact count to lhc attendance status
    get_flows_for_data_aggregate(
      df = df,
      from = 'calc_lhc_sequence_max_group',
      to = 'calc_lhc_attendance_category_overall',
      outcome = 'calc_lhc_attendance_category_overall'
      #outcome = 'calc_ldct_count_groups'
    ),
    
    # meas - lhc attendance status to risk score
    get_flows_for_data_aggregate(
      df = df,
      from = 'calc_lhc_attendance_category_overall',
      to = 'calc_risk_assessment',
      outcome = 'calc_ineligible_status'
      #outcome = 'calc_ldct_count_groups'
    ),
    
    # meas - risk score to eligible for scan
    get_flows_for_data_aggregate(
      df = df,
      from = 'calc_risk_assessment',
      to = 'calc_ineligible_status',
      outcome = 'calc_ineligible_status'
      #outcome = 'calc_ldct_count_groups'
    ),
    
    # ldct - scans
    get_flows_for_data_aggregate(
      df = df,
      from = 'calc_ineligible_status',
      to = 'calc_ldct_count_groups',
      outcome = 'calc_ldct_count_groups'
    )
  ) |> 
    # convert NAs to a string
    mutate(
      #destination = replace_na(data = destination, replace = 'Loss to follow up')
      destination = replace_na(data = destination, replace = 'Not scanned')
    ) 
  
  # add in lc outcomes
  df_summary <- bind_rows(
    df_summary,
    
    # cancer diagnoses
    get_flows_for_data_aggregate(
      df = df |> mutate(calc_ldct_count_groups = calc_ldct_count_groups |> fct_na_value_to_level('Not scanned')),
      from = 'calc_ldct_count_groups',
      to = 'cancer_outcome',
      outcome = 'cancer_outcome'
    ),
    
    # cancer staging
    get_flows_for_data_aggregate(
      #df = df |> mutate(cancer_stage = cancer_stage |> fct_na_value_to_level('Stage NA')),
      df = df |> filter(!is.na(cancer_stage)),
      from = 'cancer_outcome',
      to = 'cancer_stage',
      outcome = 'cancer_stage'
    ),
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
            str_detect(name, 'Low risk') |
            str_detect(name, 'Not scanned') |
            str_detect(name, 'No lung cancer') |
            str_detect(name, 'Stage NA')
        ) ~ '#7f8fa6',
        
        # green
        ( str_detect(name, 'Accepted') |
            str_detect(name, 'Attended') |
            str_detect(name, 'referred') |
            str_detect(name, 'High risk') |
            str_detect(name, 'scan') |
            str_detect(name, 'Lung: TLHC') |
            str_detect(name, 'TLHC:')
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
            str_detect(name, 'contacts') |
            str_detect(name, 'Lung: Counterfactual') |
            str_detect(name, 'Counterfactual:') |
            str_detect(name, 'C: S')
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
        name %in% c('High risk', 'Low risk', 'No risk score') ~ 9.1,
        name %in% c('LDCT: referred', 'LDCT: ineligible', 'LDCT: unknown') ~ 10,
        name %in% c('1 x scan', '2 x scans', '3+ scans', 'Loss to follow up', 'Not scanned') ~ 11,
        name %in% c('Lung: Counterfactual', 'Lung: TLHC', 'No lung cancer', 'Scanned: No lung cancer', 'TLHC: lung cancer', 'Counterfactual: lung cancer') ~ 12,
        name %in% c('Counterfactual: Stage 1', 'Counterfactual: Stage 2', 'Counterfactual: Stage 3', 'Counterfactual: Stage 4', 'Counterfactual: Unknown stage', 'TLHC: Stage 1', 'TLHC: Stage 2', 'TLHC: Stage 3', 'TLHC: Stage 4', 'TLHC: Unknown stage', 'TLHC: S 1-2', 'TLHC: S 3-4', 'TLHC: S ?', 'C: S 1-2', 'C: S 3-4', 'C: S ?') ~ 13.5
      ),
      x = rescale(x, to = c(1e-09, 0.99)),
      
      # define vertical positions (1 = bottom, 0 = top)
      y = case_match(
        name,
        'GP eligible population' ~ 0.5,
        
        # Invite 1
        'Invite 1 Accepted' ~ 0.0001,
        'Invite 1 No response' ~ 0.5,
        'Invite 1 Declined' ~ 0.86,
        'Invite 1 Ineligible' ~ 0.95,
        
        # Invite 2
        'Invite 2 Accepted' ~ 0.135,
        'Invite 2 No response' ~ 0.5,
        'Invite 2 Declined' ~ 0.822,
        'Invite 2 Ineligible' ~ 0.935,
        
        # Invite 3
        'Invite 3 Accepted' ~ 0.221,
        'Invite 3 No response' ~ 0.58,
        'Invite 3 Declined' ~ 0.76,
        'Invite 3 Ineligible' ~ 0.928,
        
        # Invite outcome
        'Invite Accepted' ~ 0.0001,
        'Invite No response' ~ 0.5,
        'Invite Declined' ~ 0.8,
        'Invite Ineligible' ~ 0.92,
        
        # Attendance method
        'Virtual' ~ 0.0001,
        'F2F' ~ 0.1,
        'F2F, Virtual' ~ 0.4,
        
        # Contact attempts
        '1 x contacts' ~ 0.0001,
        '2 x contacts' ~ 0.2,
        '3+ contacts' ~ 0.4,
        
        # Attendance status
        'LHC Attended' ~ 0.0001,
        'LHC DNA' ~ 0.1,
        'LHC Incomplete' ~ 0.4,
        'No attendance' ~ 0.5,
        
        # Risk status
        'High risk' ~ 0.0001,
        'Low risk' ~ 0.2,
        'No risk score' ~ 0.3,
        
        # Scan referral
        'LDCT: referred' ~ 0.0001,
        'LDCT: unknown' ~ 0.25,
        'LDCT: ineligible' ~ 0.4,
        
        # Scan count
        '1 x scan' ~ 0.0001,
        '2 x scans' ~ 0.1,
        '3+ scans' ~ 0.2,
        'Not scanned' ~ 0.7,
        
        # Cancer outcomes
        'Scanned: No lung cancer' ~ 0.0001,
        'TLHC: lung cancer' ~ 0.21,
        'Counterfactual: lung cancer' ~ 0.35,
        'No lung cancer' ~ 0.7,
        
        # Staging
        'TLHC: S 1-2' ~ 0.1,
        'TLHC: S 3-4' ~ 0.2,
        'TLHC: S ?' ~ 0.3,
        'C: S 1-2' ~ 0.5,
        'C: S 3-4' ~ 0.6,
        'C: S ?' ~ 0.7,
        
        .default = 0.5
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
      label = str_wrap(nodes$name, width = 11),
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
        size = 14
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
