library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
library(here)          # localize file references
library(lubridate)     # date functions
library(zoo)           # date functions
library(progressr)     # progress bar
library(tictoc)        # timing monitor

cat(rep('\n', 50)) # 50 blank lines to clear the console
cat('== tlhc_compare_reports.R ===============================================\n')
tic()

df_current_report <- read.csv(
  ### Please change this file date to current report
  file = here('outputs','tlhc_midata_flatfile_2023-03-22.csv' )
  # filter only interested metrics and renaming metric for easy call 
) |> filter(MetricId %in% c('1a', '3a', '3b', '5a')) |>                 
  mutate(                                                                       
    Metric_Label = case_when(
      MetricId == '1a' ~ 'lhc_invited_first',
      MetricId == '3a' ~ 'lhc_attended',
      MetricId == '3b' ~ 'lhc_attended',
      MetricId == '5a' ~ 'lhc_initial_scan'
    ),
    Metric_Label = factor(Metric_Label, levels = c(
      'lhc_invited_first',
      'lhc_attended',
      'lhc_initial_scan'
    ))
  )                                      

df_last_report <- read.csv(
  ### Please change this file date to last report
  file = here('outputs',"22-02-2023",'tlhc_midata_flatfile_2023-02-22.csv' ) 
  # filter only interested metrics and renaming metric for easy call
) |> filter(MetricId %in% c('1a', '3a', '3b', '5a')) |>                 
  mutate(                                                                        
    Metric_Label = case_when(
      MetricId == '1a' ~ 'lhc_invited_first',
      MetricId == '3a' ~ 'lhc_attended',
      MetricId == '3b' ~ 'lhc_attended',
      MetricId == '5a' ~ 'lhc_initial_scan'
    ),
    Metric_Label = factor(Metric_Label, levels = c(
      'lhc_invited_first',
      'lhc_attended',
      'lhc_initial_scan'
    )),
    # make sure all Luton metrics are labelled as Luton South Bedforshire and everything else gets reported as it is
    Project = case_when(
      Project == 'Luton' ~ 'Luton South Bedfordshire',            
      TRUE ~ Project                                                
    )
  ) 

######### First Invites ###
# filter to only invites in current report
# Arrange project to desc order. so it will be the same like last report project
first_letter_current <- df_current_report |> 
  filter(Metric_Label == "lhc_invited_first") |>  
    select(Project,month,Numerator)|>                                 
      pivot_wider(                                                                   
        names_from = month,
        values_from = Numerator ) |> 
      arrange(desc(Project)) |> 
      mutate_if(is.numeric, ~replace_na(., 0))

# filter to only invites in last report
first_letter_last <- df_last_report |> filter(Metric_Label == "lhc_invited_first") |> 
  select(Project,month,Numerator)|>                                 
  pivot_wider(                                                                  
    names_from = month,
    values_from = Numerator ) 

#comparing project name difference between last vs current report
Compares <- setdiff(first_letter_current$Project,first_letter_last$Project)
# create an object to add new coloumn for Jan23 and new projects
add_data <- data.frame(col1_name = 0, Project = Compares)
# placing new months and new projects into last month report
first_letter_last <- bind_rows(first_letter_last, add_data)

# Arranging projects and jan23 in the last report to be formatted in the first 2 coloumns
first_letter_last <- first_letter_last |>  select('Project','col1_name', everything()) |> 
  arrange(desc(Project)) |> mutate_if(is.numeric, ~replace_na(., 0))

# calculate the difference between current report and last month report 
first_letter <- (first_letter_current [,-1]-first_letter_last [,-1]) |> mutate(project = first_letter_current$Project) |>  select('project', everything())


###### LHC #####
# filter to only LHC in current report
# Arrange project to desc order. so it will be the same like last report project
lhc_scan_current <- df_current_report |> filter(Metric_Label == 'lhc_attended')|> 
  group_by(Project,month) |>  
  summarise(Number = sum(Numerator, na.rm = T)) |> arrange(desc(my(month))) |>                                 
  pivot_wider(                                                                  
    names_from = month,
    values_from=Number) |> arrange(desc(Project))|> mutate_if(is.numeric, ~replace_na(., 0))



# filter to only LHC in last report
lhc_scan_last <- df_last_report |> filter(Metric_Label == 'lhc_attended')|> 
  group_by(Project,month) |>  
  summarise(Number = sum(Numerator, na.rm = T)) |> arrange(desc(my(month))) |>        
  pivot_wider(                                                                 
    names_from = month,
    values_from=Number)

#comparing project name difference between last vs current report
Compares2 <- setdiff(lhc_scan_current$Project,lhc_scan_last$Project)
# create an object to add new coloumn for Jan23 and new projects
add_data2 <- data.frame(col1_name = 0, Project = Compares2)
# placing new months and new projects into last month report
lhc_scan_last <- bind_rows(lhc_scan_last, add_data2)

# Arranging projects and jan23 in the last report to be formatted in the first 2 coloumns
lhc_scan_last <- lhc_scan_last |>  select('Project','col1_name', everything()) |> 
  arrange(desc(Project)) |> mutate_if(is.numeric, ~replace_na(., 0))

# calculate the difference between current report and last month report 
LHC <- (lhc_scan_current [,-1]-lhc_scan_last [,-1]) |> mutate(project = lhc_scan_current$Project) |>  select('project', everything()) 

######################## scan ######
# filter to only Scans in current report
# Arrange project to desc order. so it will be the same like last report project
scan_current <- df_current_report |> filter(Metric_Label == 'lhc_initial_scan') |> 
   select(Project,month,Numerator)|>                                 
   pivot_wider(                               
     names_from = month,
     values_from = Numerator ) |> arrange(desc(Project)) |> mutate_if(is.numeric, ~replace_na(., 0))

# filter to only Scans in last report 
scan_last <- df_last_report |> filter(Metric_Label == 'lhc_initial_scan') |> 
   select(Project,month,Numerator)|>           
   pivot_wider(                                            
     names_from = month,
     values_from = Numerator ) 

#comparing project name difference between last vs current report
 Compares3 <- setdiff(scan_current$Project,scan_last$Project)
# create an object to add new coloumn for Jan23 and new projects
 add_data3 <- data.frame(col1_name = 0, Project = Compares3)
# placing new months and new projects into last month report
 scan_last <- bind_rows(scan_last, add_data3)
 
# Arranging projects and jan23 in the last report to be formatted in the first 2 coloumns 
 scan_last <- scan_last |>  select('Project','col1_name', everything()) |> 
   arrange(desc(Project)) |> mutate_if(is.numeric, ~replace_na(., 0))

# calculate the difference between current report and last month report 
scans <- (scan_current[,-1]- scan_last [,-1]) |> mutate(project = scan_current$Project) |>  select('project', everything())
 
 ####### Saving #### 

 write_xlsx(
   list(Invites = first_letter,LHC = LHC,Scans = scans),
    here('outputs', paste0('tlhc_compare_reports_', today(), '.xlsx'))
 )

# done!
cat(paste('🔚', Sys.time(), '== Script complete ================================\n', sep = ' '))
toc()
