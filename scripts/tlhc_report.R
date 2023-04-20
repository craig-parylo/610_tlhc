#' -----------------------------------------------------------------------------
#' TLHC REPORT
#' 
#' AIM
#' To produce a TLHC report
#' 
#' OBJECTIVES
#' Load mi data file
#' Cycle over projects and output a pivot_wider table
#' -----------------------------------------------------------------------------

# Libraries ----
library(tidyverse)     # tidy data wrangling
library(here)          # localised file references
library(openxlsx)      # reading / writing excel files
library(lubridate)     # working with dates
library(zoo)           # working with dates (yearmon)
library(tictoc)        # process monitoring

tic()

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Notify user 
cat(rep('\n', 50)) # 50 blank lines to clear the console
cat('== tlhc_report.R ==========================================================\n')

# Functions --------------------------------------------------------------------

#' HELPER CREATE METRIC TABLE --------------------------------------------------
#' 
#' Produces a metric table from a given tibble.
#' 
#' @param df_project tibble containing records for the project
#' @param flag_project boolean flag, TRUE = for specific projects, FALSE = for national summaries
#' 
#' @return TRUE
helper_create_metric_table <- function(df_project, flag_project = TRUE) {
  
  ## 1. project to date summary 
  df_1 <- df_project |> 
    group_by(Data_ID, Data_Name) |> 
    summarise(numerator = sum(Numerator, na.rm = T)) |> 
    mutate(
      
      # display the correct unit
      unit = case_when(
        Data_ID %in% c('10', '11', '12', '13') ~ 'Days',
        TRUE ~ 'People'
      ),
      # display the correct project check
      proj_check = case_when(
        Data_ID %in% c('10', '11', '12', '13') ~ '',
        TRUE ~ 'TRUE'
      ),
      # don't total the median metrics
      numerator = ifelse(
        Data_ID %in% c('10', '11', '12', '13'),
        NA,
        numerator
      ),
      # use the predefined eligible population 
      numerator = ifelse(
        Data_ID == '1b',
        df_project |> select(ccg_code, eligible_population) |> unique() |> summarise(eligible = sum(eligible_population, na.rm = T)) |> select(eligible) |> as.numeric(),
        numerator
      )
    ) |> 
    select(
      Data_ID,
      Data_Name,
      unit,
      proj_check,
      numerator
    )
  
  ## 2. financial year summary 
  # create an empty data frame of metric & year
  df_temp <- tibble(Data_ID = df$Data_ID |> unique())
  df_year <- tibble(financial_year = c('2019-20', '2020-21', '2021-22', '2022-23', '2023-24'))
  df_temp <- merge(
    x = df_temp,
    y = df_year,
    by = character()
  )
  
  # financial year summary
  df_2 <- df_project |> 
    group_by(Data_ID, financial_year) |> 
    summarise(numerator = sum(Numerator, na.rm = T)) |> 
    mutate(
      numerator = ifelse(
        Data_ID %in% c('10', '11', '12', '13', '1b'),
        NA,
        numerator
      )
    )
  
  # merge the actual with the template
  df_2 <- left_join(
    x = df_temp,
    y = df_2,
    by = c('Data_ID', 'financial_year')
  ) |> 
    mutate(
      # ensure we have a zero instead of na for all years
      numerator = ifelse(is.na(numerator), 0, numerator),
      
      # except for the median metrics where we do want NAs
      numerator = ifelse(
        Data_ID %in% c('10', '11', '12', '13'),
        NA,
        numerator
      )
      
    )
  
  # pivot wider
  df_2 <- df_2 |> 
    pivot_wider(
      names_from = financial_year, 
      values_from = numerator
    )
  
  # housekeeping
  rm(df_temp, df_year)
  
  
  ## 3. monthly summary 
  # create an empty template of metric and month
  df_temp <- tibble(Data_ID = df$Data_ID |> unique())
  df_month <- tibble(month = format(seq(from = make_date(year=2019, month=4, day=1), to = make_date(year=2024, month=3, day=1), by = 'month'), '%b %Y'))
  df_temp <- merge(
    x = df_temp,
    y = df_month,
    by = character()
  )
  
  # get details from df
  if(flag_project){
    
    # This is for a project so don't remove NAs - keeps it clear when the project started
    df_3 <- df_project |> 
      group_by(Data_ID, month, month_date) |> 
      summarise(sum = sum(Numerator), median = median(Numerator)) |>
      arrange(month_date) |> 
      select(!month_date) |> 
      ungroup() |> 
      # decide which aggregate (sum or median) to use as the numerator
      mutate(
        numerator = ifelse(
          Data_ID %in% c('10', '11', '12', '13'),
          median,
          sum
        )
      ) |> 
      select(-sum,-median) # remove the temporary fields
    
  }else{
    
    # This is for a national project so remove NAs - covers all periods so we want zeroes
    df_3 <- df_project |> 
      group_by(Data_ID, month, month_date) |> 
      summarise(sum = sum(Numerator, na.rm = T), median = median(Numerator, na.rm = T)) |> # calculate sum and median
      arrange(month_date) |> 
      select(!month_date) |> 
      ungroup() |> 
      # decide which aggregate (sum or median) to use as the numerator
      mutate(
        numerator = ifelse(
          Data_ID %in% c('10', '11', '12', '13'),
          median,
          sum
        )
      ) |> 
      select(-sum,-median) # remove the temporary fields
  }
  
  
  # merge the actual with the template
  df_3 <- left_join(
    x = df_temp,
    y = df_3,
    by = c('Data_ID', 'month')
  )
  
  # pivot wider
  df_3 <- df_3 |> 
    pivot_wider(
      names_from = month,
      values_from = numerator
    )
  
  # housekeeping
  rm(df_temp, df_month)
  
  ## 4. combine
  # combine all parts together
  df_ccg <- left_join(
    x = df_1,
    y = df_2,
    by = 'Data_ID'
  )
  df_ccg <- left_join(
    x = df_ccg,
    y = df_3,
    by = 'Data_ID'
  )
  
  ## 5. standardise column names
  df_ccg <- df_ccg |> 
    rename(
      `Data Item Id` = Data_ID,
      `Metric name` = Data_Name,
      `Unit` = unit,
      `Project to date check` = proj_check,
      `Project to date` = numerator
    )
  
  ## 6. Return the tibble
  return(df_ccg)
  
}

#' ADD PROJECT TABLE -----------------------------------------------------------
#' 
#' Produces a completed worksheet for a project.
#' 
#' Creates a project sheet and populates it with data in the main table area.
#' 
#' @param wb Variable referencing a loaded Excel TLHC report workbook.
#' @param df_project tibble containing records for the project
#' 
#' @return TRUE
add_project_table <- function(wb, df_project, ref_projects) {
  
  ## prepare some information
  head_project_name <- df_project$ccg_name |> unique() |> as.character()
  head_worksheet_name <- str_replace(head_project_name, ' CCG', '')
  head_project_code <- df_project$ccg_code |> unique() |> as.character()
  head_month <- format(max(df_project$month_date), '%B %Y')
  head_project_phase <- df_project$ccg_phase |> unique() |> as.character()
  # combine to a single vector
  header <- c(
    head_project_name,
    head_project_code,
    head_project_phase,
    head_month
  )
  
  # update the user
  cat(paste('📄', Sys.time(), '... working on', head_project_name, '\n', sep = ' '))
  
  
  # generate an output table ---
  df_ccg <- helper_create_metric_table(df_project = df_project, flag_project = TRUE)
  
  # get a list of metrics from the workbook
  df_template <- read.xlsx(
    xlsxFile = wb,
    sheet = 'Original',
    rows = c(24:75),
    cols = c(1:2)
  ) |>
    # standardise column names
    select(
      `Data Item Id` = `Data.Item.Id`,
      `Data Item` = `Data.Item`
    )
  
  # use the template's metric names
  df_ccg <- left_join(
    x = df_template,
    y = df_ccg |> select(-`Metric name`),
    by = 'Data Item Id'
  )
  
  
  ## 6. add to a new worksheet
  # copy the template sheet
  cloneWorksheet(
    wb = wb,
    sheetName = head_worksheet_name,
    clonedSheet = 'template_sheet'
  )
  
  # paste the header information
  writeData(
    wb = wb,
    sheet = head_worksheet_name,
    x = header,
    startCol = 3,
    startRow = 3
  )
  
  # paste the df_ccg into the worksheet
  writeData(
    wb = wb,
    sheet = head_worksheet_name,
    x = df_ccg,
    startCol = 1,
    startRow = 24,
  )
  
}


#' ADD CONTENTS TABLE ----------------------------------------------------------
#' 
#' Fills in details to the contents page
#' 
#' @param wb Variable referencing a loaded Excel TLHC report workbook.
#' @param df tibble containing all records.
#' 
#' @return TRUE
add_contents_table <- function(wb, df) {
  
  # update the user
  cat(paste('📄', Sys.time(), '... working on Contents\n', sep = ' '))
  
  # create a summary 
  df_contents <- df |> 
    mutate(start = format(start_date, '%b %Y')) |> # format the start date as a month
    group_by(ccg_code, ccg_name, ccg_phase, start, eligible_population) |> 
    summarise() |> 
    # put into correct sort order
    arrange(
      ccg_phase,
      ccg_name
    ) |> 
    ungroup() 
  
  ## 1. number invited
  df_contents_invited <- df |> 
    filter(Data_ID=='1a') |> 
    group_by(ccg_code) |> 
    summarise(invited = sum(Numerator, na.rm = T)) |> 
    ungroup()
  
  ## 2. tlhc
  df_contents_attended <- df |> 
    filter(Data_ID %in% c('3a', '3b')) |> 
    group_by(ccg_code) |> 
    summarise(attended = sum(Numerator, na.rm = T)) |> 
    ungroup()
  
  ## 3. attendance rate needs calculating later
  
  ## 4. ldct scans
  df_contents_ldct <- df |> 
    filter(Data_ID=='5a') |> 
    group_by(ccg_code) |> 
    summarise(ldct = sum(Numerator, na.rm = T)) |> 
    ungroup()
  
  ## 5. lung cancers
  df_contents_cancers <- df |> 
    filter(Data_ID %in% c('6a', '6b', '6c', '6d')) |> 
    group_by(ccg_code) |> 
    summarise(cancers = sum(Numerator, na.rm = T)) |> 
    ungroup()
  
  ## new data to report
  df_contents_newdata <- df |> 
    mutate(maxdate = max(month_date)) |>  # get latest month of data overall
    # find latest month with data per ccg
    group_by(ccg_code, month_date, maxdate) |> 
    summarise(total_numerator = sum(Numerator, na.rm = T))|> 
    filter(total_numerator > 0) |>
    ungroup() |> 
    # work out latest month with data 
    group_by(ccg_code) |> 
    summarise(newdata = (max(maxdate) == max(month_date))) |> 
    ungroup()
  
  ## combine
  # df_contents <- left_join(
  #   x = df_contents,
  #   y = df_contents_start,
  #   by = 'ccg_code'
  # )
  # df_contents <- left_join(
  #   x = df_contents,
  #   y = df_contents_eligible,
  #   by = 'ccg_code'
  # )
  df_contents <- left_join(
    x = df_contents,
    y = df_contents_invited,
    by = 'ccg_code'
  )
  df_contents <- left_join(
    x = df_contents,
    y = df_contents_attended,
    by = 'ccg_code'
  )
  # work out attendance rate
  df_contents <- df_contents |> 
    mutate(
      uptake = attended / invited
    )
  df_contents <- left_join(
    x = df_contents,
    y = df_contents_ldct,
    by = 'ccg_code'
  )
  df_contents <- left_join(
    x = df_contents,
    y = df_contents_cancers,
    by = 'ccg_code'
  )
  df_contents <- left_join(
    x = df_contents,
    y = df_contents_newdata,
    by = 'ccg_code'
  )
  
  # insert a blank row
  # df_contents <- df_contents |>
  #   add_row()
  
  # add a total row
  df_contents_total <- df_contents |>
    summarise(
      ccg_code = NA,
      ccg_name = 'Total',
      ccg_phase = NA,
      eligible_population = sum(eligible_population, na.rm = T),
      invited = sum(invited, na.rm = T),
      attended = sum(attended, na.rm = T),
      uptake = attended / invited,
      ldct = sum(ldct, na.rm = T),
      cancers = sum(cancers, na.rm = T)
    )
  
  # add main and totals together
  df_contents <- bind_rows(
    df_contents,
    df_contents_total
  )
  
  # standardise column names
  df_contents <- df_contents |> 
    rename(
      `Project code` = ccg_code,
      `Project` = ccg_name,
      `Phase` = ccg_phase,
      `Project start` = start,
      `Eligible population` = eligible_population,
      `Number invited` = invited,
      `Targeted Lung Health Checks` = attended,
      `Uptake of Lung Health Checks (LHC/Invitations)` = uptake,
      `Initial LDCT scans` = ldct,
      `Number of lung cancers` = cancers,
      `New data included in this report` = newdata
    )
  
  ## add to contents
  
  # # write to the contents data table
  writeData(
    wb = wb,
    sheet = 'Contents',
    x = df_contents,
    startCol = 2,
    startRow = 17
  )
  
  # double-space the total row
  setRowHeights(
    wb = wb,
    sheet = 'Contents',
    rows = (17 + nrow(df_contents)),
    heights = 30
  )
  
  # generate hyperlinks
  proj_names <- df_contents |>
    select(Project) |>
    filter(!Project %in% c(NA, '', 'NA', 'Total')) |> 
    mutate(
      link = makeHyperlinkString(
        sheet = Project,
        row = 1,
        col = 2,
        text = Project
      )
    ) |>
    select(
      link
    )
  
  # make into a vector
  link <- proj_names$link
  # cast as a 'formula' class
  class(link) <- c(class(link), 'formula')
  # write the data to the sheet
  writeData(
    wb = wb,
    sheet = 'Contents',
    x = link,
    startCol = 3,
    startRow = 18
  )
  
}


#' ADD CANCER DIAGNOSIS DATA ---------------------------------------------------
#' 
#' Fills in details to the Cancer diagnosis sheet
#' 
#' @param wb Variable referencing a loaded Excel TLHC report workbook.
#' @param df tibble containing all records.
#' 
#' @return TRUE
add_cancer_diagnosis <- function(wb, df) {
  
  # update the user
  cat(paste('📄', Sys.time(), '... working on Cancer diagnosis\n', sep = ' '))
  
  ### project summary
  df_cancer <- df |> 
    select(
      ccg_code,
      ccg_name,
      ccg_phase
    ) |> unique() |> 
    arrange(
      ccg_phase,
      ccg_name
    )
  
  df_cancer_total <- df |> 
    filter(Data_ID %in% c('6a','6b','6c','6d','6e')) |> 
    group_by(ccg_code) |> 
    summarise(total=sum(Numerator, na.rm = T))
  
  df_cancer_s1 <- df |> 
    filter(Data_ID=='6a') |> 
    group_by(ccg_code) |> 
    summarise(stage_1=sum(Numerator, na.rm = T))
  
  df_cancer_s2 <- df |> 
    filter(Data_ID=='6b') |> 
    group_by(ccg_code) |> 
    summarise(stage_2=sum(Numerator, na.rm = T))
  
  df_cancer_s3 <- df |> 
    filter(Data_ID=='6c') |> 
    group_by(ccg_code) |> 
    summarise(stage_3=sum(Numerator, na.rm = T))
  
  df_cancer_s4 <- df |> 
    filter(Data_ID=='6d') |> 
    group_by(ccg_code) |> 
    summarise(stage_4=sum(Numerator, na.rm = T))
  
  df_cancer_us <- df |>
    filter(Data_ID=='6e') |>
    group_by(ccg_code) |>
    summarise(unstageable=sum(Numerator, na.rm = T))
  
  # combine together
  df_cancer <- left_join(
    x = df_cancer,
    y = df_cancer_total,
    by = 'ccg_code'
  )
  df_cancer <- left_join(
    x = df_cancer,
    y = df_cancer_s1,
    by = 'ccg_code'
  )
  df_cancer <- left_join(
    x = df_cancer,
    y = df_cancer_s2,
    by = 'ccg_code'
  )
  df_cancer <- left_join(
    x = df_cancer,
    y = df_cancer_s3,
    by = 'ccg_code'
  )
  df_cancer <- left_join(
    x = df_cancer,
    y = df_cancer_s4,
    by = 'ccg_code'
  )
  df_cancer <- left_join(
    x = df_cancer,
    y = df_cancer_us,
    by = 'ccg_code'
  )
  
  # standardise column names
  df_cancer <- df_cancer |> 
    rename(
      `Code` = ccg_code,
      `Project` = ccg_name,
      `Phase` = ccg_phase,
      `Total` = total,
      `Stage 1` = stage_1,
      `Stage 2` = stage_2,
      `Stage 3` = stage_3,
      `Stage 4` = stage_4,
      `Unstageable` = unstageable
    )
  
  # housekeeping
  rm(df_cancer_total, df_cancer_s1, df_cancer_s2, df_cancer_s3, df_cancer_s4, df_cancer_us)
  
  
  ### phase summary
  df_cancer_phase_total <- df |> 
    filter(Data_ID %in% c('6a','6b','6c','6d','6e')) |> 
    group_by(ccg_phase) |> 
    summarise(total=sum(Numerator, na.rm = T))
  
  df_cancer_phase_s1 <- df |> 
    filter(Data_ID=='6a') |> 
    group_by(ccg_phase) |> 
    summarise(s1=sum(Numerator, na.rm = T))
  
  df_cancer_phase_s2 <- df |> 
    filter(Data_ID=='6b') |> 
    group_by(ccg_phase) |> 
    summarise(s2=sum(Numerator, na.rm = T))
  
  df_cancer_phase_s3 <- df |> 
    filter(Data_ID=='6c') |> 
    group_by(ccg_phase) |> 
    summarise(s3=sum(Numerator, na.rm = T))
  
  df_cancer_phase_s4 <- df |> 
    filter(Data_ID=='6d') |> 
    group_by(ccg_phase) |> 
    summarise(s4=sum(Numerator, na.rm = T))
  
  df_cancer_unstageable <- df |>
    filter(Data_ID=='6e') |>
    group_by(ccg_phase) |>
    summarise(us=sum(Numerator, na.rm = T))
  
  # combine together
  df_cancer_phase <- left_join(
    x = df_cancer_phase_total,
    y = df_cancer_phase_s1,
    by = 'ccg_phase'
  )
  df_cancer_phase <- left_join(
    x = df_cancer_phase,
    y = df_cancer_phase_s2,
    by = 'ccg_phase'
  )
  df_cancer_phase <- left_join(
    x = df_cancer_phase,
    y = df_cancer_phase_s3,
    by = 'ccg_phase'
  )
  df_cancer_phase <- left_join(
    x = df_cancer_phase,
    y = df_cancer_phase_s4,
    by = 'ccg_phase'
  )
  df_cancer_phase <- left_join(
    x = df_cancer_phase,
    y = df_cancer_unstageable,
    by = 'ccg_phase'
  )
  
  # housekeeping
  rm(df_cancer_phase_total, df_cancer_phase_s1, df_cancer_phase_s2, df_cancer_phase_s3, df_cancer_phase_s4, df_cancer_unstageable)
  
  # process for percentages
  df_cancer_phase <- df_cancer_phase |> 
    mutate(
      total_perc = total / total,
      s1_perc = s1 / total,
      s2_perc = s2 / total,
      s3_perc = s3 / total,
      s4_perc = s4 / total,
      us_perc = us / total
    ) |>
    # standardise column names
    rename(
      `Phase` = ccg_phase,
      `Total` = total,
      `Stage 1` = s1,
      `Stage 2` = s2,
      `Stage 3` = s3,
      `Stage 4` = s4,
      `Unstageable` = us,
      `Total %` = total_perc,
      `Stage 1 %` = s1_perc,
      `Stage 2 %` = s2_perc,
      `Stage 3 %` = s3_perc,
      `Stage 4 %` = s4_perc,
      `Unstageable %` = us_perc
    )
  
  ## Add to workbook
  
  # write df_cancer
  writeData(
    wb = wb,
    sheet = 'Cancer diagnosis',
    x = df_cancer,
    startCol = 2,
    startRow = 7
  )
  
  # generate hyperlinks
  proj_names <- df_cancer |>
    select(Project) |>
    mutate(
      link = makeHyperlinkString(
        sheet = Project,
        row = 1,
        col = 2,
        text = Project
      )
    ) |>
    select(
      link
    )
  
  # make into a vector
  link <- proj_names$link
  # cast as a 'formula' class
  class(link) <- c(class(link), 'formula')
  # write the data to the sheet
  writeData(
    wb = wb,
    sheet = 'Cancer diagnosis',
    x = link,
    startCol = 3,
    startRow = 8
  )
  
  # write df_cancer_phase
  writeData(
    wb = wb,
    sheet = 'Cancer diagnosis',
    x = df_cancer_phase,
    startCol = 12,
    startRow = 7
  )
  
  # generate hyperlinks
  proj_names <- df_cancer_phase |>
    select(Phase) |>
    mutate(
      link = makeHyperlinkString(
        sheet = Phase,
        row = 1,
        col = 2,
        text = Phase
      )
    ) |>
    select(
      link
    )
  
  # make into a vector
  link <- proj_names$link
  # cast as a 'formula' class
  class(link) <- c(class(link), 'formula')
  # write the data to the sheet
  writeData(
    wb = wb,
    sheet = 'Cancer diagnosis',
    x = link,
    startCol = 12,
    startRow = 8
  )
}

#' HELPER ADD METRIC TO PROJECT SUMMARY ----------------------------------------
#' 
#' Helper function to add_summary_project() to calculate the value for a given
#' metric and add to an identified dataframe. 
#' 
#' @seealso add_summary_project()
#' 
#' @param df_recipient tibble to receive the calculated metric.
#' @param df_donor tibble containing the data from which to calculate the metric.
#' @param metrics character vector identifying the Data_IDs that comprise the metric.
#' @param metric_name string name for the metric (will be the column heading in df_recipient).
#' @param grouping optional grouping variable - used to group into projects otherwise ignored.
#' 
#' @return df_recipient - the recipient table with the new metric in.
#' 
helper_ps_add_metric <- function(df_recipient, df_donor, metrics, metric_name, grouping = NULL) {
  
  # NB use of quazi-quotations (!! and :=) comes from here:
  # https://stackoverflow.com/questions/49371260/using-variables-as-arguments-in-summarize
  
  # group_by_at use described here:
  # https://stackoverflow.com/questions/38220963/how-to-pass-a-variable-name-in-group-by
  
  if(is.null(grouping)){
    
    # for total, original, onboarded / phase 3
    df_recipient <- left_join(
      x = df_recipient,
      y = df_donor |> 
        filter(Data_ID %in% metrics) |> 
        summarise(!!metric_name := sum(Numerator, na.rm = T)),
      by = character()
    )
    
  }else{
    
    # for project groups
    df_recipient <- left_join(
      x = df_recipient,
      y = df_donor |> 
        filter(Data_ID %in% metrics) |> 
        group_by_at(grouping) |> 
        summarise(!!metric_name := sum(Numerator, na.rm = T)),
      by = grouping
    ) 
  }
  
  return(df_recipient)
}

#' ADD SUMMARY PROJECT TABLE ---------------------------------------------------
#' 
#' Creates a tibble containing summary figures (total, original, onboarded) and
#' project summary figures for selected metrics. Coordinates the use of
#' helper_ps_add_metric function to calculate and add values to a tibble which
#' is then pasted into the 'Summary by project' sheet of the TLHC report.
#' 
#' @seealso helper_ps_add_metric()
#' 
#' @param wb Variable referencing a loaded Excel TLHC report workbook.
#' @param df tibble containing all records.
#' 
#' @return TRUE
#' 
add_summary_project <- function(wb, df) {
  
  # update the user
  cat(paste('📄', Sys.time(), '... working on Summary by project\n', sep = ' '))
  
  # get the list of projects as reported in the template
  df_ps_spine <- read.xlsx(
    xlsxFile = wb,
    sheet = 'Summary by project',
    startRow = 4,
    colNames = T,
    rows = c(4:58),
    cols = c(1:3)
  ) |> 
    rename(ccg_code = CCG.code, phase = `Original./.Onboarded./.Phase.3`) |> 
    filter(
      !Project %in% c('Original', 'Onboarded / Phase 3', 'Total')
    )
  
  # create templates for original, onboarded and total summaries
  df_ps_original <- tibble(Project = 'Original')
  df_ps_onboarded <- tibble(Project = 'Onboarded / Phase 3')
  df_ps_total <- tibble(Project = 'Total')
  
  # create pre-filtered datasets
  df_original <- df |> filter(ccg_phase=='Original')
  df_onboarded <- df |> filter(!ccg_phase=='Original')
  
  # 1b eligible for a lung health check
  df_project_ref <- df |>   # load the reference table
    select(ccg_code, Numerator = eligible_population, ccg_phase) |> # select and alias our fields
    unique() |> 
    mutate(Data_ID = '1b') # add a reference to the metric for eligible population
  metrics <- c('1b')
  name <- 'metric_1b'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df_project_ref, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df_project_ref, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_project_ref |> filter(ccg_phase == 'Original'), metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_project_ref |> filter(!ccg_phase == 'Original'), metrics, name) 
  
  
  
  # 1a first invites
  metrics <- c('1a')
  name <- 'metric_1a'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 1c second invites
  metrics <- c('1c')
  name <- 'metric_1c'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 2 accepted invitation
  metrics <- c('2')
  name <- 'metric_2'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 3a/b attended (f2f & tel)
  metrics <- c('3a', '3b')
  name <- 'metric_3ab'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)  
  
  # 3a attended f2f
  metrics <- c('3a')
  name <- 'metric_3a'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 3b attended tel
  metrics <- c('3b')
  name <- 'metric_3b'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 3c/d DNA (f2f & tel)
  metrics <- c('3c', '3d')
  name <- 'metric_3cd'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 3c DNA f2f
  metrics <- c('3c')
  name <- 'metric_3c'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 3d DNA tel
  metrics <- c('3d')
  name <- 'metric_3d'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 4 ldct referred
  metrics <- c('4a')
  name <- 'metric_4a'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 4b triggered but ineligible
  metrics <- c('4b')
  name <- 'metric_4b'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 5a initial ldct performed
  metrics <- c('5a')
  name <- 'metric_5a'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 5b DNA ldct
  metrics <- c('5b')
  name <- 'metric_5b'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 5def ldct followups total
  metrics <- c('5d', '5e', '5f')
  name <- 'metric_5def'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 5d 3 month followup
  metrics <- c('5d')
  name <- 'metric_5d'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 5e 12 month followup
  metrics <- c('5e')
  name <- 'metric_5e'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 5f 18 month followup
  metrics <- c('5f')
  name <- 'metric_5f'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 5adef ldct total
  metrics <- c('5a', '5d', '5e', '5f')
  name <- 'metric_5adef'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 14 referred to lc pathway
  metrics <- c('14')
  name <- 'metric_14'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 6abcd lung cancers total
  metrics <- c('6a', '6b', '6c', '6d')
  name <- 'metric_6abcd'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 6a stage 1
  metrics <- c('6a')
  name <- 'metric_6a'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 6b stage 2
  metrics <- c('6b')
  name <- 'metric_6b'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 6c stage 3
  metrics <- c('6c')
  name <- 'metric_6c'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 6d stage 4
  metrics <- c('6d')
  name <- 'metric_6d'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 7 incidental findings
  metrics <- c('7')
  name <- 'metric_7'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 8b offered smoking course
  metrics <- c('8b')
  name <- 'metric_8b'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 8a took up offer
  metrics <- c('8a')
  name <- 'metric_8a'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # 9 completed course
  metrics <- c('9')
  name <- 'metric_9'
  df_ps_spine <- helper_ps_add_metric(df_ps_spine, df, metrics, name, 'ccg_code')
  df_ps_total <- helper_ps_add_metric(df_ps_total, df, metrics, name)
  df_ps_original <- helper_ps_add_metric(df_ps_original, df_original, metrics, name)
  df_ps_onboarded <- helper_ps_add_metric(df_ps_onboarded, df_onboarded, metrics, name)
  
  # bring data together
  df_ps_spine <- bind_rows(
    df_ps_original,
    df_ps_onboarded,
    df_ps_total,
    df_ps_spine
  ) |> 
    # ensure we have metrics in the right order
    select(
      Project,
      ccg_code,
      phase,
      metric_1b:metric_9
    )
  
  # paste data to workbook
  writeData(
    wb = wb,
    sheet = 'Summary by project',
    x = df_ps_spine,
    startCol = 1,
    startRow = 5,
    colNames = FALSE
  )
  
}

#' GENERATE METRIC SUMMARY TABLE -----------------------------------------------
#' 
#' Creates a metric summary table that commonly appears at the top of the tlhc
#' report sheets.
#' 
#' @param df tibble containing all records.
#' 
#' @return TRUE
#' 
generate_metric_summary_table <- function(df) {
  
  # create a template
  df_ms <- tibble(
    data_id = NULL,
    data_name = NULL,
    number = NULL
  )
  
  # 1b eligible
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('1b')) |>
      select(ccg_name, eligible_population) |> 
      unique() |> 
      summarise(number = sum(eligible_population, na.rm = T)) |> 
      mutate(
        metric_id = '1b',
        metric_name = 'Eligible for a Lung Health Check'
      )
  )
  
  # 1ac invited 
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('1a', '1c')) |> 
      #summarise(number = sum(Numerator, na.rm = T)) |> # (feedback that this is confusing so marking as empty)
      summarise(number = NA) |> 
      mutate(
        metric_id = '1ac',
        metric_name = 'Invited to a Lung Health Check'
      )
  )
  
  # 1a invited - first
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('1a')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '1a',
        metric_name = 'Invited to a Lung Health Check (first invites)'
      )
  )
  
  # 1c invited - first
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('1c')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '1c',
        metric_name = 'Invited to a Lung Health Check (follow-up invites)'
      )
  )
  
  # 2 accepted
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('2')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '2',
        metric_name = 'Accepted a Lung Health Check invitation'
      )
  )
  
  # 3ab attended
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('3a', '3b')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '3ab',
        metric_name = 'Attended a Lung Health Check appointment'
      )
  )
  
  # 3a attended f2f
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('3a')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '3a',
        metric_name = 'Attended a face-to-face Lung Health Check appointment'
      )
  )
  
  # 3b attended tel
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('3b')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '3b',
        metric_name = 'Attended a telephone Lung Health Check appointment'
      )
  )
  
  # 3cd DNA
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('3c', '3d')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '3cd',
        metric_name = 'Did not attend a Lung Health Check appointment'
      )
  )
  
  # 3c DNA f2f
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('3c')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '3c',
        metric_name = 'Did not attend a face-to-face Lung Health Check appointment'
      )
  )
  
  # 3d DNA tel
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('3d')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '3d',
        metric_name = 'Did not attend a telephone Lung Health Check appointment'
      )
  )
  
  # 4 referred for ldct
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('4')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '4',
        metric_name = 'Referred for a Low Dose CT Scan (LDCT)'
      )
  )
  
  # 4b triggered but ineligible
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('4b')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '4b',
        metric_name = 'Triggered a risk score for referral but were ineligible for an initial LDCT'
      )
  )
  
  # 5a initial ldct performed
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('5a')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '5a',
        metric_name = 'Initial LDCT scan performed'
      )
  )
  
  # 5b DNA ldct scan
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('5b')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '5b',
        metric_name = 'Did not attend their initial LDCT scan appointment'
      )
  )
  
  # 5def follow-up ldct scan
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('5d', '5e', '5f')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '5def',
        metric_name = 'Follow up scan performed'
      )
  )
  
  # 5d follow-up 3 month
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('5d')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '5d',
        metric_name = '3 month follow up LDCT scan performed'
      )
  )
  
  # 5e follow-up 12 month
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('5e')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '5e',
        metric_name = '12 month follow up LDCT scan performed'
      )
  )
  
  # 5f follow-up 24 month
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('5f')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '5f',
        metric_name = '24 month follow up LDCT scan performed'
      )
  )
  
  # 5adef all scans
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('5a', '5d', '5e', '5f')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '5adef',
        metric_name = 'Total number of scans performed'
      )
  )
  
  # 14 referred
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('14')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '14',
        metric_name = 'Referred to Lung Cancer Pathway following LDCT scan'
      )
  )
  
  # 6abcd lung cancers diagnosed
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('6a', '6b', '6c', '6d')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '6abcd',
        metric_name = 'Number of lung cancers diagnosed'
      )
  )
  
  # 6a stage 1
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('6a')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '6a',
        metric_name = 'Lung cancers diagnosed at stage 1'
      )
  )
  
  # 6b stage 2
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('6b')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '6b',
        metric_name = 'Lung cancers diagnosed at stage 2'
      )
  )
  
  # 6c stage 3
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('6c')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '6c',
        metric_name = 'Lung cancers diagnosed at stage 3'
      )
  )
  
  # 6d stage 4
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('6d')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '6d',
        metric_name = 'Lung cancers diagnosed at stage 4'
      )
  )
  
  # 7 incidental findings
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('7')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '7',
        metric_name = 'Incidental findings'
      )
  )
  
  # 8 smoking cessation - filler - no actual data reported
  df_ms <- bind_rows(
    df_ms,
    tibble(
      metric_id = '8',
      metric_name = 'Smoking cessation'
    )
  )
  
  # 8b offered smoking cessation
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('8b')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '8b',
        metric_name = 'Offered smoking cessation course'
      )
  )
  
  # 8a took up smoking cessation
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('8a')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '8a',
        metric_name = 'Took up an offer of smoking cessation course'
      )
  )
  
  # 9 completed smoking cessation
  df_ms <- bind_rows(
    df_ms,
    df |> 
      filter(Data_ID %in% c('9')) |> 
      summarise(number = sum(Numerator, na.rm = T)) |> 
      mutate(
        metric_id = '9',
        metric_name = 'Completed a smoking cessation course'
      )
  )
  
  # calculate denominator and value
  df_ms <- df_ms |> 
    # put columns in the right order
    select(
      metric_id,
      metric_name,
      number
    ) |> 
    mutate(
      # calculate denominator based on the metric and its counterpart
      denominator = case_when(
        #metric_id == '1ac'   ~ df_ms[df_ms$metric_id=='1b',]$number,
        metric_id == '1a'    ~ df_ms[df_ms$metric_id=='1b',]$number,
        metric_id == '1c'    ~ df_ms[df_ms$metric_id=='1b',]$number,
        metric_id == '2'     ~ df_ms[df_ms$metric_id=='1a',]$number,
        metric_id == '3ab'   ~ df_ms[df_ms$metric_id=='2',]$number,
        metric_id == '3a'    ~ df_ms[df_ms$metric_id=='2',]$number,
        metric_id == '3b'    ~ df_ms[df_ms$metric_id=='2',]$number,
        metric_id == '3cd'   ~ df_ms[df_ms$metric_id=='2',]$number,
        metric_id == '3c'    ~ df_ms[df_ms$metric_id=='2',]$number,
        metric_id == '3d'    ~ df_ms[df_ms$metric_id=='2',]$number,
        metric_id == '4'     ~ df_ms[df_ms$metric_id=='3ab',]$number,
        metric_id == '4b'    ~ df_ms[df_ms$metric_id=='4',]$number,
        metric_id == '5a'    ~ df_ms[df_ms$metric_id=='4',]$number,
        metric_id == '5b'    ~ df_ms[df_ms$metric_id=='4',]$number,
        metric_id == '3b'    ~ df_ms[df_ms$metric_id=='4',]$number,
        metric_id == '14'    ~ df_ms[df_ms$metric_id=='3ab',]$number,
        metric_id == '6abcd' ~ df_ms[df_ms$metric_id=='3ab',]$number,
        metric_id == '6a'    ~ df_ms[df_ms$metric_id=='6abcd',]$number,
        metric_id == '6b'    ~ df_ms[df_ms$metric_id=='6abcd',]$number,
        metric_id == '6c'    ~ df_ms[df_ms$metric_id=='6abcd',]$number,
        metric_id == '6d'    ~ df_ms[df_ms$metric_id=='6abcd',]$number,
        metric_id == '8b'    ~ df_ms[df_ms$metric_id=='3ab',]$number,
        metric_id == '8a'    ~ df_ms[df_ms$metric_id=='8b',]$number,
        metric_id == '9'     ~ df_ms[df_ms$metric_id=='8a',]$number
      ),
      
      # calculate value
      value = case_when(
        !is.na(denominator) ~ number / denominator
      )
    )
  
  # return the metric table
  return(df_ms)
  
}

#' ADD SUMMARY NATIONAL TABLE --------------------------------------------------
#' 
#' Creates a tibble containing summary figures (total, original, onboarded) and
#' project summary figures for selected metrics. Coordinates the use of
#' helper_ps_add_metric function to calculate and add values to a tibble which
#' is then pasted into the 'Summary by project' sheet of the TLHC report.
#' 
#' @seealso helper_ps_add_metric()
#' 
#' @param wb Variable referencing a loaded Excel TLHC report workbook.
#' @param df tibble containing all records.
#' 
#' @return TRUE
#' 
add_summary_national <- function(wb, df) {
  
  # update the user
  cat(paste('📄', Sys.time(), '... working on National summary\n', sep = ' '))
  
  # get latest month's data
  latest_month <- format(max(df$month_date), '%b %Y')
  
  # generate a metric table
  df_nat_sum <- helper_create_metric_table(df_project = df, flag_project = FALSE)
  
  # get a list of metrics from the workbook
  df_template <- read.xlsx(
    xlsxFile = wb,
    sheet = 'National summary',
    rows = c(24:75),
    cols = c(1:2)
  ) |>
    # standardise column names
    select(
      `Data Item Id` = `Data.Item.Id`,
      `Data Item` = `Data.Item`
    )
  
  # add the data to the template
  df_datatable <- left_join(
    x = df_template,
    y = df_nat_sum |> select(-`Metric name`),
    by = 'Data Item Id'
  )
  
  # # generate a summary metric table for each group (national, original, onboarded, phase 3)
  # df_national <- generate_metric_summary_table(df=df) |> 
  #   rename(
  #     number_nat = number,
  #     denom_nat = denominator,
  #     val_nat = value
  #   )
  # df_original <- generate_metric_summary_table(df=df |> filter(ccg_phase=='Original')) |> 
  #   rename(
  #     number_orig = number,
  #     denom_orig = denominator,
  #     val_orig = value
  #   )
  # df_onboarded <- generate_metric_summary_table(df=df |> filter(ccg_phase=='Onboarded')) |> 
  #   rename(
  #     number_onbo = number,
  #     denom_onbo = denominator,
  #     val_onbo = value
  #   )
  # df_phase3 <- generate_metric_summary_table(df=df |> filter(ccg_phase=='Phase 3')) |> 
  #   rename(
  #     number_ph3 = number,
  #     denom_ph3 = denominator,
  #     val_ph3 = value
  #   )
  # 
  # # combine to a single table
  # df_metrics <- left_join(
  #   x = df_national,
  #   y = df_original |> select(!metric_name),
  #   by = 'metric_id'
  # )
  # df_metrics <- left_join(
  #   x = df_metrics,
  #   y = df_onboarded |> select(!metric_name),
  #   by = 'metric_id'
  # )
  # df_metrics <- left_join(
  #   x = df_metrics,
  #   y = df_phase3 |> select(!metric_name),
  #   by = 'metric_id'
  # )
  
  # paste these tables to the workbook
  
  # paste header 
  writeData(
    wb = wb,
    sheet = 'National summary',
    x = latest_month,
    startCol = 3,
    startRow = 5
  )
  
  # paste the metric summary
  # writeData(
  #   wb = wb,
  #   sheet = 'National summary',
  #   x = df_metrics,
  #   startCol = 1,
  #   startRow = 12,
  #   colNames = FALSE
  # )
  
  # paste the metric details
  writeData(
    wb = wb,
    sheet = 'National summary',
    #x = df_nat_sum,
    x = df_datatable,
    startCol = 1,
    startRow = 24,
  )
  
}

#' ADD PHASE SHEETS ------------------------------------------------------------
#' 
#' Adds data to the phase summary sheets (original / onboarded / phase 3)
#' 
#' @param wb Variable referencing a loaded Excel TLHC report workbook.
#' @param df tibble containing records for the project
#' 
#' @return TRUE
add_phase_sheets <- function(wb, df) {
  
  # update the user
  cat(paste('📄', Sys.time(), '... working on Original, Onboarded and Phase 3 sheets\n', sep = ' '))
  
  # get a list of metrics from the workbook
  df_template <- read.xlsx(
    xlsxFile = wb,
    sheet = 'Original',
    rows = c(24:75),
    cols = c(1:2)
  ) |>
    # standardise column names
    select(
      `Data Item Id` = `Data.Item.Id`,
      `Data Item` = `Data.Item`
    )
  
  # Get original metric tables ---
  df_active <- df |> filter(ccg_phase == 'Original')
  df_original_metric <- helper_create_metric_table(df_project = df_active, flag_project = FALSE)
  df_original_latest_month <- format(max(df_active$month_date), '%b %Y')
  
  # use the template's metric names
  df_original_metric <- left_join(
    x = df_template,
    y = df_original_metric |> select(-`Metric name`),
    by = 'Data Item Id'
  )

  # Get onboarded metric tables ---
  df_active <- df |> filter(ccg_phase == 'Onboarded')
  df_onboarded_metric <- helper_create_metric_table(df_project = df_active, flag_project = FALSE)
  df_onboarded_latest_month <- format(max(df_active$month_date), '%b %Y')
  
  # use the templates's metric names
  df_onboarded_metric <- left_join(
    x = df_template,
    y = df_onboarded_metric |> select(-`Metric name`),
    by = 'Data Item Id'
  )
  
  # Get phase 3 metric tables ---
  df_active <- df |> filter(ccg_phase == 'Phase 3')
  df_phase3_metric <- helper_create_metric_table(df_project = df_active, flag_project = FALSE)
  df_phase3_latest_month <- format(max(df_active$month_date), '%b %Y')
  
  # use the templates's metric names
  df_phase3_metric <- left_join(
    x = df_template,
    y = df_phase3_metric |> select(-`Metric name`),
    by = 'Data Item Id'
  )
  
  # Original - paste to workbook ---
  # paste header 
  writeData(
    wb = wb,
    sheet = 'Original',
    x = df_original_latest_month,
    startCol = 3,
    startRow = 4
  )

  # paste the metric details
  writeData(
    wb = wb,
    sheet = 'Original',
    x = df_original_metric,
    startCol = 1,
    startRow = 24,
  )
  
  # Onboarded - paste to workbook ---
  # paste header 
  writeData(
    wb = wb,
    sheet = 'Onboarded',
    x = df_onboarded_latest_month,
    startCol = 3,
    startRow = 4
  )
  
  # paste the metric details
  writeData(
    wb = wb,
    sheet = 'Onboarded',
    x = df_onboarded_metric,
    startCol = 1,
    startRow = 24,
  )
  
  # Phase 3 - paste to workbook ---
  # paste header 
  writeData(
    wb = wb,
    sheet = 'Phase 3',
    x = df_phase3_latest_month,
    startCol = 3,
    startRow = 4
  )

  # paste the metric details
  writeData(
    wb = wb,
    sheet = 'Phase 3',
    x = df_phase3_metric,
    startCol = 1,
    startRow = 24,
  )
  
}

# update the user
cat(paste('☑️', Sys.time(), 'Functions defined\n', sep = ' '))

# Load data --------------------------------------------------------------------

## Reference data ----
## load reference table for eligible population
# ref_projects <- read_excel(path = here('data', 'reference', 'project_reference.xlsx')) |> 
#   unique() |> 
#   select(
#     ccg_code = project_code,
#     eligible = eligible_population
#   ) |> 
#   ungroup()

## MI data ----
df <- read_rds(
  file = here('data', 'tlhc', 'tlhc_mi_data.Rds')
) |> 
  # remove reference to old projects
  filter(
    !ccg_name %in% c('Halton CCG', 'Knowsley CCG', 'Liverpool CCG')
  ) |> 
  # convert some fields to factors to aid sorting
  mutate(
    ccg_code = factor(ccg_code),
    ccg_name = factor(ccg_name),
    ccg_phase = factor(ccg_phase, levels = c('Original', 'Onboarded', 'Phase 3')),
    Data_ID = case_when(Data_ID == '4' ~ '4a', TRUE ~ Data_ID), # convert any 4 metrics to 4a for consistency
    Data_ID = factor(Data_ID, levels = c('1a', '1c', '1b', '2', '3a', '3b', '3c', '3d', '4a', '4b', '5a', '5b', '5d', '5e', '5f', '5g', '6a', '6b', '6c', '6d', '6e', '7', '7a', '7b', '7c', '7d', '7e', '7f', '7g', '7h', '7i', '7j', '7k', '7l', '7m', '7n', '7o', '7p', '7q', '7r', '7s', '7t', '7u', '8b', '8a', '9', '10', '11', '12', '13', '14')),
    month = format(month_date, '%b %Y'),
    financial_year = case_when(
      month(month_date) < 4 ~ paste(year(month_date)-1,year(month_date)-2000, sep = '-'),
      TRUE ~ paste(year(month_date),year(month_date)+1-2000, sep = '-')
    ),
    financial_year = factor(financial_year, levels = c('2019-20', '2020-21', '2021-22', '2022-23', '2023-24'))
  ) |> 
  # sort
  arrange(
    ccg_phase,
    ccg_name,
    month_date,
    Data_ID
  ) |> 
  mutate(
    month = factor(month)
  ) |> 
  # remove metrics not reported in the TLHC report
  filter(
    !is.na(Data_ID)
  )

## add in eligible population reference information
# df <- left_join(
#   x = df,
#   y = ref_projects,
#   by = 'ccg_code'
# )

## MI template workbook ----
wb <- loadWorkbook(
  file = here('data', 'templates', 'tlhc_report_template.xlsx')
)

# update the user
cat(paste('☑️', Sys.time(), 'Data loaded\n', sep = ' '))

# Main logic -------------------------------------------------------------------

## Add contents 
add_contents_table(wb=wb, df=df)

## Add cancer diagnoses
add_cancer_diagnosis(wb=wb, df=df)
 
## Add summary by project
add_summary_project(wb=wb, df=df)

## Add national summary
add_summary_national(wb=wb, df=df)

# ## Add original / onboarded / phase 3 summaries
add_phase_sheets(wb=wb, df=df)

## Loop over projects ----
# get a list of projects (in the correct order: phase then name)
df_projects <- df |>
  select(
    ccg_code,
    ccg_name,
    ccg_phase,
    eligible_population
  ) |>
  unique() |>
  arrange(
    ccg_phase,
    ccg_name
  )

# conduct the loop over ccg codes
for(project in df_projects$ccg_code) {

  # get a dataframe for just this ccg
  df_project <- df |> filter(ccg_code == project)

  # pass this df to the function to create a report
  add_project_table(wb=wb, df_project=df_project, ref_projects=ref_projects)
}

# remove the template sheet
removeWorksheet(
  wb = wb,
  sheet = 'template_sheet'
)

## save the workbook ----
# update the user
cat(paste('⏱️', Sys.time(), 'Saving workbook\n', sep = ' '))

# do the saving
saveWorkbook(
  wb = wb,
  file = here('outputs', paste0('TLHC key metrics ', Sys.Date(),'.xlsx')
  ),
  overwrite = TRUE,
  returnValue = TRUE
)

# open the file for review
url = here('outputs', paste0('TLHC key metrics ', Sys.Date(), '.xlsx'))
browseURL(url = url)

# update the user
cat(paste('☑️', Sys.time(), 'Script complete ===================================\n', sep = ' '))
toc()