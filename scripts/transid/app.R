#' -----------------------------------------------------------------------------
#' TLHC TRANSACTION ID VIEWER
#' 
#' This app is designed to display summaries of the latest record-level 
#' submissions from projects.
#' 
#' It is used during the submission phase of each month to track when data is
#' received.
#' 
#' This process requires that 'tlhc_latest_submissions.R' is run first.
#' -----------------------------------------------------------------------------

# libraries --------------------------------------------------------------------
library(shiny)
library(tidyverse)
library(reactable)
library(lubridate)
library(here)

# data -------------------------------------------------------------------------
df <- readRDS(file = here('data', 'tlhc', 'df_latest_transact.Rds')) |> ungroup()
df_pivot <- readRDS(file = here('data', 'tlhc', 'df_latest_transact_pivot.Rds')) |> ungroup()
df_pivot_transid <- readRDS(file = here('data', 'tlhc', 'df_latest_transact_pivot_transid.Rds')) |> ungroup()

# get a list of projects - for use in the selector drop-down
projects <- df |> ungroup() |>  select(Project_name) |> arrange(Project_name)

# get a simple list of files
df_latest_files <- df |> 
  select(
    ReceivedDate,
    TransactionId,
    Project_name,
    UserEmail
  ) |>
  unique() |> 
  arrange(desc(ReceivedDate))

# ui ---------------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("TLHC Recent Transactions"),
    
    # Project selector
    fluidRow(
      
      column(
        7,
        selectInput(
          inputId = 'selected_project',
          label = 'Choose a project',
          choices = projects,
          selected = projects[[1]],
          multiple = F
        )
      ),

    ),
    
    # Tabset -------------------------------------------------------------------
    fluidRow(
      tabsetPanel(
        
        ## latest files --------------------------------------------------------
        tabPanel(
          title = 'Files',
          reactableOutput('latest_files'),
          br(),
          'This sheet displays the latest submisssions.'
        ),
        
        ## include / exclude per project ---------------------------------------
        tabPanel(
          title = 'Includes',
          reactableOutput('includes')
        ),
        
        ## date received -------------------------------------------------------
        tabPanel(
          title = 'Date received',
          reactableOutput('date_received')
        ),
        
        ## full data -----------------------------------------------------------
        tabPanel(
          title = 'Full data',
          reactableOutput('all_data')
        )
      )
    )
)

# server -----------------------------------------------------------------------
server <- function(input, output) {
  
  # reactive expressions -------
  df_includes_selected <- reactive({
    
    df_pivot_transid |> 
      filter(
        Project_name == input$selected_project
      ) |> 
      select(
        Project_name,
        project_code,
        TransactionId,
        Demographics,
        Invites,
        `Lung Health Checks`,
        Measurements,
        `Other History`,
        LDCT,
        Diagnostics,
        `Smoking cessation`
      )
  })
  
  df_date_selected <- reactive({
    
    df_pivot |> 
      filter(
        Project_name == input$selected_project
      ) |> 
      select(
        Project_name,
        project_code,
        TransactionId,
        Demographics,
        Invites,
        `Lung Health Checks`,
        Measurements,
        `Other History`,
        LDCT,
        Diagnostics,
        `Smoking cessation`
      )
  })
  
  df_all_selected <- reactive({
    
    df |> 
      filter(
        Project_name == input$selected_project
      ) |> 
      select(
        Project_name,
        project_code,
        SubmittedFile,
        ReceivedDate,
        TransactionId,
        UserEmail,
        table_name,
        include
      )
  })
  

  
  # output tables --------------------------------------------------------------
  output$includes <- renderReactable({
    reactable(
      data = df_includes_selected(),
      filterable = F,
      pagination = F,
      columns = list(
        Project_name = colDef(name = 'Project', minWidth = 200, resizable = T),
        project_code = colDef(name = 'Code', maxWidth = 70),
        TransactionId = colDef(name = 'Trans ID', maxWidth = 80, align = 'left'),
        Demographics = colDef(name = 'Demographics', align = 'center'),
        Invites = colDef(name = 'Invites', align = 'center'),
        `Lung Health Checks` = colDef(name = 'Lung Health Check', align = 'center'),
        Measurements = colDef(name = 'Measurements', align = 'center'),
        `Other History` = colDef(name = 'Other History', align = 'center'),
        LDCT = colDef(name = 'LDCT', align = 'center'),
        Diagnostics = colDef(name = 'Diagnostics', align = 'center'),
        `Smoking cessation` = colDef(name = 'Smoking cessation', align = 'center')
      ),
      defaultColDef = colDef(
        style = function(value) {
          if (value %in% c('Include')) {
            list(background = 'rgba(0, 255, 0, 0.1)', fontSize = '1em')
          } else if (value %in% c('Exclude')) {
            list(background = 'rgba(255, 0, 0, 0.1)', fontSize = '1em')
          } else {
            list(fontSize = '1em')
          }
        }
      )
    )
  })
  
  output$date_received <- renderReactable({
    reactable(
      data = df_date_selected(),
      filterable = F,
      pagination = F,
      columns = list(
        Project_name = colDef(name = 'Project', minWidth = 200, resizable = T),
        project_code = colDef(name = 'Code', maxWidth = 70),
        TransactionId = colDef(name = 'Trans ID', maxWidth = 80, align = 'left'),
        Demographics = colDef(name = 'Demographics', align = 'center', format = colFormat(datetime = T)),
        Invites = colDef(name = 'Invites', align = 'center', format = colFormat(datetime = T)),
        `Lung Health Checks` = colDef(name = 'Lung Health Check', align = 'center', format = colFormat(datetime = T)),
        Measurements = colDef(name = 'Measurements', align = 'center', format = colFormat(datetime = T)),
        `Other History` = colDef(name = 'Other History', align = 'center', format = colFormat(datetime = T)),
        LDCT = colDef(name = 'LDCT', align = 'center', format = colFormat(datetime = T)),
        Diagnostics = colDef(name = 'Diagnostics', align = 'center', format = colFormat(datetime = T)),
        `Smoking cessation` = colDef(name = 'Smoking cessation', align = 'center', format = colFormat(datetime = T))
      )
    )
  })
  
  output$all_data <- renderReactable({
    reactable(
      data = df_all_selected(),
      filterable = T,
      pagination = F,
      columns = list(
        Project_name = colDef(name = 'Project', minWidth = 200, resizable = T),
        project_code = colDef(name = 'Code', maxWidth = 70),
        SubmittedFile = colDef(name = 'Submitted file', minWidth = 200, resizable = T),
        ReceivedDate = colDef(name = 'Received date', align = 'center', format = colFormat(datetime = T)),
        TransactionId = colDef(name = 'Trans ID', maxWidth = 80, align = 'left'),
        UserEmail = colDef(name = 'User email'),
        table_name = colDef(name = 'Table'),
        include = colDef(name = 'Include', align = 'center', style = function(value) {
          if (value %in% c('Include')) {
            list(background = 'rgba(0, 255, 0, 0.1)', fontSize = '1em')
          } else if (value %in% c('Exclude')) {
            list(background = 'rgba(255, 0, 0, 0.1)', fontSize = '1em')
          } else {
            list(fontSize = '1em')
          }
        }
        )
      )
    )
  })
  
  output$latest_files <- renderReactable({
    reactable(
      data = df_latest_files,
      filterable = T,
      pagination = F,
      columns = list(
        ReceivedDate = colDef(name = 'Received date', align = 'left', maxWidth = 210, format = colFormat(datetime = T)),
        TransactionId = colDef(name = 'Trans ID', maxWidth = 80, align = 'left'),
        Project_name = colDef(name = 'Project', minWidth = 200, resizable = T),
        UserEmail = colDef(name = 'User email')
      )
    )
  })

}

# run --------------------------------------------------------------------------
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
