#' -----------------------------------------------------------------------------
#' SANKEY DIAGRAMS
#' 
#' Produce a Shiny app to produce Sankey diagrams showing the flow of TLHC
#' participants through key milestones in the programme
#' 
#' CHANGELOG
#' V2 (2023-11-08) Using bslib to house the inputs in a sidebar control
#' -----------------------------------------------------------------------------

# libraries --------------------------------------------------------------------
library(shiny)                            # shiny app framework
library(tidyverse)                        # tidy data wrangling
#library(shinyWidgets)                     # ?
library(gt)                               # nice datatable formatting
#library(gtExtras)                         # ?
library(plotly)                           # producing sankey charts
library(svglite)                          # outputting charts as svg
library(bslib)                            # use of accordion panels and themes for overall UI
library(bsicons)                          # UI icons
library(datamods)                         # use of select_group_x to filter data based on user-selections
library(readxl)                           # reading the data dictionary in Excel format
library(reactable)                        # presenting the data dictionary

# data -------------------------------------------------------------------------
df_sankey <- readRDS(file ='df_sankey_preagg.Rds') |> 
  # remove phase 3 projects
  filter(!phase == 'Phase 3') |> 
  # tidy up data to ensure they are presented as sensible options in the pickers
  mutate(
    phase = fct(x = phase, levels = c('Original', 'Onboarded')),
    
    calc_lsoa_imd_decile = fct(x = as.character(calc_lsoa_imd_decile), levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10')),
    calc_lsoa_imd_decile = fct_na_value_to_level(f = calc_lsoa_imd_decile, level = 'Not known'),
    
    calc_lsoa_rurality_group_category = fct(x = calc_lsoa_rurality_group_category, levels = c('Rural', 'Urban')),
    calc_lsoa_rurality_group_category = fct_na_value_to_level(f = calc_lsoa_rurality_group_category, level = 'Not known'),
    
    triage_before_risk_assessment = fct(x = triage_before_risk_assessment, levels = c('Yes', 'No', 'Unknown')),
    
    # NB, already a factor, so just code NAs as a level
    calc_ethnic_group = fct_na_value_to_level(f = calc_ethnic_group, level = 'Not known'),
    
    calc_sex = fct(x = calc_sex, levels = c('Female', 'Male', 'Not known')),
    calc_sex = fct_na_value_to_level(f = calc_sex, level = 'Not known'),
    
    # NB, already a factor, just relevelling and coding NAs as a level
    calc_age_group_ipsos = fct_collapse(.f = calc_age_group_ipsos, Other = c('Other', 'Age below zero')), # group age<0 with other
    calc_age_group_ipsos = fct_relevel(.f = calc_age_group_ipsos, '55-64', '65-74', '75', 'Other'), # move other to end
    calc_age_group_ipsos = fct_na_value_to_level(f = calc_age_group_ipsos, level = 'Not known')
  )

dt_sankey <- 'September 2023'

df_data_dictionary <- read_excel(path = 'sankey_data_dictionary.xlsx', sheet = 'data_dictionary')

# ui ---------------------------------------------------------------------------

# Define UI for application
ui <- page_sidebar(
  title = 'Targeted Lung Health Checks (including provisional cancer outcomes)',
  #theme = bs_theme(bootswatch = 'bootstrap', version = 5),
  fillable = T,
  
  ## sidebar -------------------------------------------------------------------
  sidebar = sidebar(
    width = '400px',
    bg = '#fff',
    
    accordion(
      open = F, # ensure all panels are closed by default
      
      accordion_panel(
        title = 'Filters',
        icon = bs_icon('filter') |> tooltip('Use these controls to filter the data and see the resulting Sankey chart'),
        
        select_group_ui(
          id = 'filters',
          inline = F,
          params = list(
            # organisational filters
            alliance = list(inputId = 'CAName', label = 'Cancer Alliance'),
            project = list(inputId = 'project', label = 'Project'),
            phase = list(inputId = 'phase', label = 'Phase'),
            invite_mode = list(inputId = 'invite_mode', label = 'Invite mode'),
            triage = list(inputId = 'triage_before_risk_assessment', label = 'Triage before risk assessment'),
            lhc_delivery = list(inputId = 'lhc_delivery', label = 'LHC delivery model'),
            admin = list(inputId = 'admin', label = 'Administrative delivery'),
            
            # demographic filters
            deprivation = list(inputId = 'calc_lsoa_imd_decile', label = 'Deprivation decile'),
            rurality = list(inputId = 'calc_lsoa_rurality_group_category', label = 'Rural / Urban'),
            ethnicity = list(inputId = 'calc_ethnic_group', label = 'Broad ethnic group'),
            gender = list(inputId = 'calc_sex', label = 'Gender'),
            age_group = list(inputId = 'calc_age_group_ipsos', label = 'Age group'),
            smoking = list(inputId = 'smoking_status', label = 'Smoking status')
            
          )
        )
      ),
      
      accordion_panel(
        title = 'Interpreting Sankey charts',
        icon = bs_icon('info-circle') |> tooltip('A short guide to what Sankey charts show and how they are applied to the TLHC data'),
        markdown(
          'Sankey diagrams can be used to visualise flows through a system.
        
          They show the flow / conversion rate after each step of the pathway (e.g. conversion from first invite to lung health check).
        
          The width of the arrows is proportional to the flow  / conversion rate.
        
          They have been used for the patient level analysis to aggregate the flow of individual participants from their first TLHC invitation to their follow up CT scans.
        
          They will be extended to cover cancer diagnosis and staging once the Rapid Cancer Registrations Dataset is available.
        
          Record level data is used to track the pathways of each eligible participant. Rules are applied to remove any double counting and manage data quality issues.
        
          The individual pathways can be aggregated for the whole programme (Original phase) or by organisation (e.g. project, cancer alliance), typology (e.g. delivery model), and demographic factors (e.g. deprivation decile, age group, etc.).
          '
        )
      ),
      
      accordion_panel(
        title = 'Changelog',
        icon = bs_icon('list-ul') |> tooltip('Changes in this version of the app'),
        
        card(
          full_screen = F,
          card_header('April 2024 (Current version)'),
          card_body(
            tags$p('This version includes:'),
            tags$ul(
              tags$li('Activity up to February 2024,'),
              tags$li(markdown('Cancer outcomes provided by [NCRAS](https://digital.nhs.uk/ndrs/about/ncras), including cancer type and staging.')),
              tags$li(markdown('Additional filters for *LHC delivery*, *Administrative delivery* based on updated project delivery model survey.')),
              tags$li(markdown('Additional filter for *Smoking status* calculated from Smoking Cessation, Measurement and LHC submissions.'))
            ),
            tags$p('Cancer outcome caveats:'),
            tags$ul(
              tags$li(markdown('Cancer diagnoses are <b>provisional</b> only.')),
              tags$li('Diagnoses go up to April 2023 (latest available from NCRAS),'),
              tags$li(markdown('Lung cancers are considered TLHC-associated if they are made within 147 days of a LHC or LDCT scan (provisional process).')),
            )
          )
        ),
        card(
          full_screen = F,
          card_header('December 2023'),
          card_body(
            tags$p('The pilot version of this app.'),
            tags$ul(
              tags$li('Activity up to September 2023.'),
              tags$li('Added Sankey chart showing unfiltered data as a comparator.'),
              tags$li('Pop-up labels modified to include percent of GP eligible population in each node and flow.')
            )
          )
        )
      ),
      
      accordion_panel(
        title = 'About',
        icon = bs_icon('link') |> tooltip('Details about this application'),
        markdown(
          'This resource has been produced by [The Strategy Unit](https://www.strategyunitwm.nhs.uk/) and [Ipsos](https://www.ipsos.com/en-uk) as part of the evaluation of the [Targeted Lung Health Check](https://www.england.nhs.uk/contact-us/privacy-notice/how-we-use-your-information/our-services/evaluation-of-the-targeted-lung-health-check-programme/) programme by NHS England.'
        ),
        #tags$p(paste0('This app includes activity up to ', as.character(dt_sankey), '.')),
        tags$br(),
        img(src = 'TLHC Graphic.png', alt = 'Logo for the Targeted Lung Health Check Programme'),
      )
    )
  ),
  
  ## main content --------------------------------------------------------------
  bg = '#f7f7f7',
  
  accordion(
    ### main sankey ------------------------------------------------------------
    accordion_panel(
      title = 'Sankey: filtered data',
      icon = bs_icon('info') |> tooltip('This Sankey chart is based on your selected filters'),
      plotlyOutput('sankey_plot', height = '100%'),
      height = '150%'
    ),
    
    ### national sankey --------------------------------------------------------
    accordion_panel(
      title = 'Sankey: all data',
      icon = bs_icon('info') |> tooltip('This Sankey chart is based on unfiltered data, to be used in contrast with the above chart'),
      plotlyOutput('sankey_all', height = '100%'),
      height = '70vh'
    ),
    
    ### bar charts -------------------------------------------------------------
    accordion_panel(
      title = 'Bar charts',
      icon = bs_icon('info') |> tooltip('These charts show breakdowns for your selected data'),
      
      # tall plots
      layout_column_wrap(
        width = 3/12,
        
        card(
          full_screen = T,
          card_header('Cancer Alliance'),
          card_body(plotlyOutput('bar_alliance'))
        ),
        card(
          full_screen = T,
          card_header('Project'),
          card_body(plotlyOutput('bar_project'))
        ),
        card(
          full_screen = T,
          card_header('Deprivation decile'),
          card_body(plotlyOutput('bar_imd'))
        ),
        card(
          full_screen = T,
          card_header('Broad ethnic group'),
          card_body(plotlyOutput('bar_ethnic'))
        )
      ),
      
      # shorter plots
      layout_column_wrap(
        width = 2/12,
        height = '600px',
        
        card(
          full_screen = T,
          card_header('Phase'),
          card_body(plotlyOutput('bar_phase')) 
        ),
        card(
          full_screen = T,
          card_header('Triage before LHC'),
          card_body(plotlyOutput('bar_triage'))
        ),
        card(
          full_screen = T,
          card_header('Invite mode'),
          card_body(plotlyOutput('bar_invite_mode'))
        ),
        card(
          full_screen = T,
          card_header('Rural / Urban'),
          card_body(plotlyOutput('bar_rural'))
        ),
        card(
          full_screen = T,
          card_header('Gender'),
          card_body(plotlyOutput('bar_gender'))
        ),
        card(
          full_screen = T,
          card_header('Age group'),
          card_body(plotlyOutput('bar_age'))
        ),
        card(
          full_screen = T,
          card_header('Smoking status'),
          card_body(plotlyOutput('bar_smoking'))
        ),
        card(
          full_screen = T,
          card_header('LHC delivery model'),
          card_body(plotlyOutput('bar_lhc_delivery'))
        ),
        card(
          full_screen = T,
          card_header('Admin. delivery model'),
          card_body(plotlyOutput('bar_admin'))
        )
      )
    ),
    
    ### proportions ------------------------------------------------------------
    accordion_panel(
      title = 'Proportions',
      icon = bs_icon('info') |> tooltip('Shows conversion rates for key milestones'),
      layout_column_wrap(
        width = 6/12,
        
        card(
          card_header('Invites'),
          card_body(gt_output('table_invite'))
        ),
        
        card(
          card_header('Lung Health Checks'),
          card_body(gt_output('table_lhc'))
        ),
        
        card(
          card_header('Risk assessment'),
          card_body(gt_output('table_risk'))
        ),
        
        card(
          card_header('Scans'),
          card_body(gt_output('table_scan'))
        )
      )
    ),
    
    ### data dictionary --------------------------------------------------------
    accordion_panel(
      title = 'Data dictionary',
      icon = bs_icon('info') |> tooltip('Shows the data dictionary'),
      reactableOutput('data_dictionary')
    )
  )
)



# server -----------------------------------------------------------------------
server <- function(input, output, session) {

  # setup ----------------------------------------------------------------------
  # load in functions for use in sankey generation
  source('func_sankey.R')
  
  # uncomment this to enable the theme customiser
  #bs_themer()
  
  ## data filtering ------------------------------------------------------------
  # use the select group server to handle data filtering
  df_sankey_prep <- select_group_server(
    id = 'filters',
    data = df_sankey,
    vars = reactive(c(
      # organisational filters
      'CAName', 'project', 'phase', 'invite_mode', 
      'triage_before_risk_assessment', 'lhc_delivery', 'admin',
      
      # demographic filters
      'calc_lsoa_imd_decile', 'calc_lsoa_rurality_group_category', 
      'calc_ethnic_group', 'calc_sex', 'calc_age_group_ipsos', 'smoking_status'
      )
    )
  )
  
  ## main sankey ---------------------------------------------------------------
  # render the main sankey plot
  output$sankey_plot <- renderPlotly({
    get_sankey_for_data(df = df_sankey_prep())
  })
  
  ## national sankey -----------------------------------------------------------
  # render the sankey for the national picture (i.e. unfiltered)
  output$sankey_all <- renderPlotly({
    get_sankey_for_data(df = df_sankey)
  })
  
  ## summary tables ------------------------------------------------------------
  ### all data tables ----------------------------------------------------------
  all_denom <- df_sankey |> count(calc_eligible, wt = participants) |> pull() # get the denominator
  
  df_all_invite <- df_sankey |> 
    count(calc_invite_outcome, wt = participants) |> # count the number of participants by invite outcome
    rename(all_n = n) |> # rename the field
    mutate(
      all_n = round(all_n/5)*5, # round to multiples of 5
      all_p = all_n / all_denom # calculate percent
    )
  
  df_all_lhc <- df_sankey |> 
    count(calc_lhc_attendance_category_overall, wt = participants) |> # count the number of participants by lhc attendance
    rename(all_n = n) |> # rename the field
    mutate(
      all_n = round(all_n/5)*5, # round to multiples of 5
      all_p = all_n / all_denom # calculate percent
    )
  
  df_all_risk <- df_sankey |> 
    count(calc_risk_assessment, wt = participants) |> # count the number of participants by lhc attendance
    rename(all_n = n) |> # rename the field
    mutate(
      all_n = round(all_n/5)*5, # round to multiples of 5
      all_p = all_n / all_denom # calculate percent
    )
  
  df_all_scan <- df_sankey |> 
    # recode scans to a binary scanned / not scanned
    # mutate(
    #   calc_ldct_count_groups_binary = case_when(
    #     !is.na(calc_ldct_count_groups) ~ 'Scanned'
    #   )
    # ) |>
    count(calc_ldct_count_groups, wt = participants) |> # count the number of participants by scan count
    rename(all_n = n) |> # rename the field
    mutate(
      all_n = round(all_n/5)*5, # round to multiples of 5
      all_p = all_n / all_denom # calculate percent
    )
  
  ### invites ------------------------------------------------------------------
  output$table_invite <- render_gt({

    # filtered data
    filtered_denom <- df_sankey_prep() |> count(calc_eligible, wt = participants) |> pull() # get the denominator
    
    df_filtered <- df_sankey_prep() |> 
      count(calc_invite_outcome, wt = participants) |> # count the number of participants by invite outcome
      rename(filtered_n = n) |> # rename the field
      mutate(
        filtered_n = round(filtered_n/5)*5, # round to multiple of 5
        filtered_p = filtered_n / filtered_denom, # calculate percent
      )
    
    # combine with all data and format
    df_show <- left_join(
      x = df_all_invite,
      y = df_filtered,
      by = 'calc_invite_outcome'
    ) |> 
      mutate(
        difference_p = filtered_p - all_p
      ) |> 
      gt() |> 
      # add the column labels
      cols_label(
        calc_invite_outcome = 'Invite outcome',
        all_n = 'n',
        all_p = '%',
        filtered_n = 'n',
        filtered_p = '%',
        difference_p = 'Difference'
      ) |> 
      # format the numbers
      fmt_number(
        columns = c(all_n, filtered_n),
        sep_mark = ',',
        decimals = 0
      ) |> 
      # format the percentages
      fmt_percent(
        columns = c(all_p, filtered_p, difference_p)
      ) |> 
      # bold the difference column
      tab_style(
        style = cell_text(weight = 'bold'),
        locations = cells_body(columns = difference_p)
      ) |> 
      # add the spanners
      tab_spanner(
        label = 'All data',
        columns = c(all_n, all_p)
      ) |> 
      tab_spanner(
        label = 'Filtered data',
        columns = c(filtered_n, filtered_p)
      ) |> 
      tab_footnote(
        'Percentages are calculated with reference to the eligible population'
      )
  })

  ### lhc attendance -----------------------------------------------------------
  output$table_lhc <- render_gt({
    
    # filtered data
    filtered_denom <- df_sankey_prep() |> count(calc_eligible, wt = participants) |> pull() # get the denominator
    
    df_filtered <- df_sankey_prep() |> 
      count(calc_lhc_attendance_category_overall, wt = participants) |> # count the number of participants by lhc attendance
      rename(filtered_n = n) |> # rename the field
      mutate(
        filtered_n = round(filtered_n/5)*5, # round to multiple of 5
        filtered_p = filtered_n / filtered_denom, # calculate percent
      )
    
    # combine and format
    df_show <- left_join(
      x = df_all_lhc,
      y = df_filtered,
      by = 'calc_lhc_attendance_category_overall'
    ) |> 
      mutate(
        difference_p = filtered_p - all_p
      ) |> 
      gt() |> 
      # add the column labels
      cols_label(
        calc_lhc_attendance_category_overall = 'LHC status',
        all_n = 'n',
        all_p = '%',
        filtered_n = 'n',
        filtered_p = '%',
        difference_p = 'Difference'
      ) |> 
      # format the numbers
      fmt_number(
        columns = c(all_n, filtered_n),
        sep_mark = ',',
        decimals = 0
      ) |> 
      # format the percentages
      fmt_percent(
        columns = c(all_p, filtered_p, difference_p)
      ) |> 
      # bold the difference column
      tab_style(
        style = cell_text(weight = 'bold'),
        locations = cells_body(columns = difference_p)
      ) |> 
      # add the spanners
      tab_spanner(
        label = 'All data',
        columns = c(all_n, all_p)
      ) |> 
      tab_spanner(
        label = 'Filtered data',
        columns = c(filtered_n, filtered_p)
      ) |> 
      tab_footnote(
        'Percentages are calculated with reference to the eligible population'
      )
  })
  
  ### risk assessment ----------------------------------------------------------
  output$table_risk <- render_gt({
    
    # filtered data
    filtered_denom <- df_sankey_prep() |> count(calc_eligible, wt = participants) |> pull() # get the denominator
    
    df_filtered <- df_sankey_prep() |> 
      count(calc_risk_assessment, wt = participants) |> # count the number of participants by lhc attendance
      rename(filtered_n = n) |> # rename the field
      mutate(
        filtered_n = round(filtered_n/5)*5, # round to multiple of 5
        filtered_p = filtered_n / filtered_denom, # calculate percent
      )
    
    # combine and format
    df_show <- left_join(
      x = df_all_risk,
      y = df_filtered,
      by = 'calc_risk_assessment'
    ) |> 
      mutate(
        difference_p = filtered_p - all_p
      ) |> 
      gt() |> 
      # add the column labels
      cols_label(
        calc_risk_assessment = 'Risk assessment',
        all_n = 'n',
        all_p = '%',
        filtered_n = 'n',
        filtered_p = '%',
        difference_p = 'Difference'
      ) |> 
      # format the numbers
      fmt_number(
        columns = c(all_n, filtered_n),
        sep_mark = ',',
        decimals = 0
      ) |> 
      # format the percentages
      fmt_percent(
        columns = c(all_p, filtered_p, difference_p)
      ) |> 
      # bold the difference column
      tab_style(
        style = cell_text(weight = 'bold'),
        locations = cells_body(columns = difference_p)
      ) |> 
      # add the spanners
      tab_spanner(
        label = 'All data',
        columns = c(all_n, all_p)
      ) |> 
      tab_spanner(
        label = 'Filtered data',
        columns = c(filtered_n, filtered_p)
      ) |> 
      tab_footnote(
        'Percentages are calculated with reference to the eligible population'
      )
  })

  ### scan status --------------------------------------------------------------
  output$table_scan <- render_gt({

    # filtered data
    filtered_denom <- df_sankey_prep() |> count(calc_eligible, wt = participants) |> pull() # get the denominator

    df_filtered <- df_sankey_prep() |>
      # recode scans to a binary scanned / not scanned
      # mutate(
      #   calc_ldct_count_groups_binary = case_when(
      #     !is.na(calc_ldct_count_groups) ~ 'Scanned'
      #   )
      # ) |>
      count(calc_ldct_count_groups, wt = participants) |> # count the number of participants by scan count
      rename(filtered_n = n) |> # rename the field
      mutate(
        filtered_n = round(filtered_n/5)*5, # round to multiple of 5
        filtered_p = filtered_n / filtered_denom, # calculate percent
      )

    # combine and format
    df_show <- left_join(
      x = df_all_scan,
      y = df_filtered,
      by = 'calc_ldct_count_groups'
    ) |>
      mutate(
        difference_p = filtered_p - all_p
      ) |>
      gt() |>
      # add the column labels
      cols_label(
        calc_ldct_count_groups = 'Scan status',
        all_n = 'n',
        all_p = '%',
        filtered_n = 'n',
        filtered_p = '%',
        difference_p = 'Difference'
      ) |>
      # format the numbers
      fmt_number(
        columns = c(all_n, filtered_n),
        sep_mark = ',',
        decimals = 0
      ) |>
      # format the percentages
      fmt_percent(
        columns = c(all_p, filtered_p, difference_p)
      ) |>
      # bold the difference column
      tab_style(
        style = cell_text(weight = 'bold'),
        locations = cells_body(columns = difference_p)
      ) |>
      # add the spanners
      tab_spanner(
        label = 'All data',
        columns = c(all_n, all_p)
      ) |>
      tab_spanner(
        label = 'Filtered data',
        columns = c(filtered_n, filtered_p)
      ) |>
      tab_footnote(
        'Percentages are calculated with reference to the eligible population'
      )
  })
  
  ## bar charts ----------------------------------------------------------------
  
  #' Create a horizontal barchart for a specified feature
  #' 
  #' Creates a Plotly-based barchart for a specified feature, e.g. project or 
  #' demographic to show the distribution of data
  #'
  #' @param df_all Tibble - the unfiltered data - used to get all unique values for the feature
  #' @param df_prep Tibble - the filtered data - used to get the counts of participants per value
  #' @param variable Variable - the column/variable defining the feature, unquoted 
  #' @param str_var String - the string version of the variable defining the feature, quoted
  #'
  #' @return Plotly plot
  create_breakdown_barchart <- function(df_all, df_prep, variable, str_var) {
    
    # create a dataframe that counts patients by each of variable
    df <- left_join(
      x = df_all |> select({{variable}}) |> unique(), # get a list of all values of variable
      y = df_prep |> count({{variable}}, wt = participants) |> mutate(n = round(n/5)*5), # count rounded to multiple of 5
      by = str_var # field to join on
    ) |> 
      mutate(n = replace_na(n, 0)) # ensure no NAs reported
    
    # create the bar plot
    plot <- plot_ly(
      data = df,
      y = df[[str_var]],
      x = df$n,
      type = 'bar',
      orientation = 'h',
      color = I('#686f73')
    ) |> 
      config(displayModeBar = F) # hide the mode bar as visual clutter
    
    return(plot)
  }
  
  
  #### cancer alliance ----
  output$bar_alliance <- renderPlotly({
    
    # create a plot for our data
    plot <- create_breakdown_barchart(df_all = df_sankey, df_prep = df_sankey_prep(), 
                                      variable = CAName, str_var = 'CAName')
    
    # customise the plot
    plot |> 
      layout(
        yaxis = list(categoryorder = 'category descending'),
        font = list(family = 'system-ui')
      )
  })
  
  #### project ----
  output$bar_project <- renderPlotly({
    
    # create a plot for our data
    plot <- create_breakdown_barchart(df_all = df_sankey, df_prep = df_sankey_prep(), 
                                      variable = project, str_var = 'project')
    
    # customise the plot
    plot |> 
      layout(
        yaxis = list(categoryorder = 'category descending'),
        font = list(family = 'system-ui')
      )
  })
  
  #### phase ----
  output$bar_phase <- renderPlotly({
    
    # create a plot for our data
    plot <- create_breakdown_barchart(df_all = df_sankey |> mutate(phase = fct_rev(phase)), 
                                      df_prep = df_sankey_prep(), 
                                      variable = phase, str_var = 'phase')
    
    # customise the plot
    plot |> 
      layout(
        #yaxis = list(categoryorder = 'category descending'),
        font = list(family = 'system-ui')
      )
  })
  
  #### bar_invite_mode ----
  output$bar_invite_mode <- renderPlotly({
    
    # create a plot for our data
    plot <- create_breakdown_barchart(df_all = df_sankey, df_prep = df_sankey_prep(), 
                                      variable = invite_mode, str_var = 'invite_mode')
    
    # customise the plot
    plot |> 
      layout(
        yaxis = list(categoryorder = 'category descending'),
        font = list(family = 'system-ui')
      )
  })

  #### triage ----
  output$bar_triage <- renderPlotly({
    
    # create a plot for our data
    plot <- create_breakdown_barchart(df_all = df_sankey |> mutate(triage_before_risk_assessment = fct_rev(triage_before_risk_assessment)), 
                                      df_prep = df_sankey_prep(), 
                                      variable = triage_before_risk_assessment, str_var = 'triage_before_risk_assessment')
    
    # customise the plot
    plot |> 
      layout(
        font = list(family = 'system-ui')
      )
  })
  
  #### deprivation ----
  output$bar_imd <- renderPlotly({
    
    # create a plot for our data
    plot <- create_breakdown_barchart(df_all = df_sankey |> mutate(calc_lsoa_imd_decile = fct_rev(calc_lsoa_imd_decile)), 
                                      df_prep = df_sankey_prep(), 
                                      variable = calc_lsoa_imd_decile, str_var = 'calc_lsoa_imd_decile')
    
    # customise the plot
    plot |> 
      layout(
        font = list(family = 'system-ui')
      )
  })
  
  #### rurality ----
  output$bar_rural <- renderPlotly({
    
    # create a plot for our data
    plot <- create_breakdown_barchart(df_all = df_sankey |> mutate(calc_lsoa_rurality_group_category = fct_rev(calc_lsoa_rurality_group_category)), 
                                      df_prep = df_sankey_prep(), 
                                      variable = calc_lsoa_rurality_group_category, str_var = 'calc_lsoa_rurality_group_category')
    
    # customise the plot
    plot |> 
      layout(
        font = list(family = 'system-ui')
      )
  })
  
  #### ethnicity ----
  output$bar_ethnic <- renderPlotly({
    
    # create a plot for our data
    plot <- create_breakdown_barchart(df_all = df_sankey, 
                                      df_prep = df_sankey_prep(), 
                                      variable = calc_ethnic_group, str_var = 'calc_ethnic_group')
    
    # customise the plot
    plot |> 
      layout(
        font = list(family = 'system-ui'),
        yaxis = list(categoryorder = 'category descending')
      )
  })
  
  #### gender ----
  output$bar_gender <- renderPlotly({
    
    # create a plot for our data
    plot <- create_breakdown_barchart(df_all = df_sankey, 
                                      df_prep = df_sankey_prep(), 
                                      variable = calc_sex, str_var = 'calc_sex')
    
    # customise the plot
    plot |> 
      layout(
        font = list(family = 'system-ui'),
        yaxis = list(categoryorder = 'category descending')
      )
  })
  
  #### age ----
  output$bar_age <- renderPlotly({
    
    # create a plot for our data
    plot <- create_breakdown_barchart(df_all = df_sankey |> mutate(calc_age_group_ipsos = fct_rev(calc_age_group_ipsos)), 
                                      df_prep = df_sankey_prep(), 
                                      variable = calc_age_group_ipsos, str_var = 'calc_age_group_ipsos')
    
    # customise the plot
    plot |> 
      layout(
        font = list(family = 'system-ui')
      )
  })
  
  #### smoking ----
  output$bar_smoking <- renderPlotly({
    
    # create a plot for our data
    plot <- create_breakdown_barchart(df_all = df_sankey, 
                                      df_prep = df_sankey_prep(), 
                                      variable = smoking_status, str_var = 'smoking_status')
    
    # customise the plot
    plot |> 
      layout(
        font = list(family = 'system-ui')
      )
  })
  
  #### lhc delivery ----
  output$bar_lhc_delivery <- renderPlotly({
    
    # create a plot for our data
    plot <- create_breakdown_barchart(df_all = df_sankey, 
                                      df_prep = df_sankey_prep(), 
                                      variable = lhc_delivery, str_var = 'lhc_delivery')
    
    # customise the plot
    plot |> 
      layout(
        font = list(family = 'system-ui')
      )
  })
  
  #### admin ----
  output$bar_admin <- renderPlotly({
    
    # create a plot for our data
    plot <- create_breakdown_barchart(df_all = df_sankey, 
                                      df_prep = df_sankey_prep(), 
                                      variable = admin, str_var = 'admin')
    
    # customise the plot
    plot |> 
      layout(
        font = list(family = 'system-ui')
      )
  })
  
  ## data dictionary ----
  output$data_dictionary <- renderReactable({
    
    # render the dictionary
    df_data_dictionary |> 
      reactable(
        filterable = T,
        defaultPageSize = 5,
        columns = list(
          field = colDef(name = 'Field name', minWidth = 70, show = F),
          value = colDef(name = 'Milestone', minWidth = 70),
          source = colDef(name = 'Data source', minWidth = 80),
          description = colDef(name = 'Description', minWidth = 200),
          definition = colDef(name = 'Definition', minWidth = 300)
        )
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
