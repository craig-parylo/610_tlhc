#' -----------------------------------------------------------------------------
#' FUNCTION: NAME PROJECTS
#' 
#' Names projects based on two sets of codes; the code of the organisation that
#' submitted the data and the ccg code of the participant the record relates to.
#' 
#' In most cases the submitting organisation is the same as the project, however,
#' there are some cases where an organisation submits on behalf of two or more
#' projects, in these cases we use the participant's ccg code to decide which
#' project their activity falls under.
#' 
#' A reference file is used to control which projects are split by ccg code.
#' 
#' @param df tibble dataframe containing project and ccg codes to be named, must
#' contain columns called 'place_code' and 'ccg_code'
name_projects <- function(df) {
  
  # libraries
  library(tidyverse)
  library(here)
  library(readxl)
  
  # ensure we have a column called 'project_name' (we need this for the coalesce naming step)
  if (!'project_name' %in% colnames(df)) {
    df <- df |> mutate(project_name = NA) # create a column with empty data 
  }
  
  # ensure we have columns called place_code and ccg_code
  if ((!'place_code' %in% colnames(df)) && (!'ccg_code' %in% colnames(df))) {
    warning('df must have columns named [place_code] and [ccg_code]')
  }
  
  # load new reference data
  ref_project_lu <- read_excel(
    path = here('data', 'reference', 'tlhc_project_name_lookup.xlsx')
  )
  
  # get a reference table of submitting org code to project name
  temp_ref_submitting_project_name <- ref_project_lu |> 
    select(
      place_code = calc_submitting_organisation_code,
      project_name_place
    ) |> 
    unique()
  
  # get a reference table of ccg codes to project name (only where required to split by ccg)
  temp_ref_ccg_project_name <- ref_project_lu |> 
    filter(split_by_ccg == 1) |> 
    select(
      ccg_code = CCG_Code,
      project_name_ccg
    )
  
  # look up project names from place and ccg codes
  df <- left_join(
    x = df,
    y = temp_ref_submitting_project_name,
    by = 'place_code'
  )
  df <- left_join(
    x = df,
    y = temp_ref_ccg_project_name,
    by = 'ccg_code'
  )
  
  # determine a final project name
  df <- df |> 
    mutate(
      project = coalesce(project_name, project_name_ccg, project_name_place)
    ) |> 
    # exclude the temp versions of the project name
    select(-project_name, -project_name_ccg, project_name_place)
  
  # return the result
  return(df)
  
}