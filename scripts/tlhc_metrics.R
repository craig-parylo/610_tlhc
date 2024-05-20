#'------------------------------------------------------------------------------
#' TLHC CALCULATE METRICS
#' 
#' Calculate metrics based on downloaded files which have been processed.
#' 
#'------------------------------------------------------------------------------

# Libraries --------------------------------------------------------------------
library(tidyverse)     # tidy data processing
library(here)          # localise file references
library(lubridate)     # date functions
library(zoo)           # date functions
library(progressr)     # progress bar
library(tictoc)        # timing monitor
library(fuzzyjoin)     # joining on fuzzy word matches
source(here('scripts', 'func_name_projects.R')) # naming projects
source(here('scripts', 'tlhc_metric_functions.R')) # functions for loading metric data
source(here('scripts', 'tlhc_general_functions.R')) # general functions
readRenviron(here('.Renviron')) # project level environment variables

# Notify user 
update_user(stage = 'start', message = 'tlhc_metrics.R')
tic()


# load metric data -------------------------------------------------------------

# set up the progress indicator
handlers(handler_progress(format='[:bar] :percent :eta :message')) # set up the progress indicator

## base tables -----------------------------------------------------------------
#' load base tables into memory ready for use by the other metric calculations
#' nb, loading these beforehand makes the rest of the metrics quicker to work 
#' out.

with_progress({
  
  update_user(message = 'Loading base tables', icon = '⏱️')
  p <- progressor(steps = 14)
  
  df_demo <- load_df_demo()
  df_demo_join <- load_df_demo_join()
  df_invites <- load_df_invites()
  df_lhc <- load_df_lhc()
  df_meas <- load_df_measurements()
  df_ldct <- load_df_ldct()
  df_ldct_incid_other <- extract_other_incidental_data()
  df_ldct_incid_pulm <- extract_pulmonary_incidental_data()
  df_smoking <- load_df_smoking()
  df_diag <- load_df_diagnostics()
  df_canc_registry <- load_df_cancer_tumour()
  df_canc_rr_pathway <- load_df_cancer_pathway()
  df_canc_rr_tumour <- load_df_cancer_reg()
  df_cancer <- get_df_cancer()
  
})


## metric dataframes -----------------------------------------------------------
#' prepare pre-filtered dataframes for each metric, so that all the details of 
#' the tables are preserved but are limited to the numerators for each metric.
#' This enables the same dataframe to be aggregated in multiple ways e.g. by age
#' groups for the MI report and also by age groups for the ipsos reports.

with_progress({
  
  update_user(message = 'Loading metric dataframes', icon = '⏱️')
  p <- progressor(steps = 46)
  
  df_metric_1a_invites_first <- get_df_metric_1a_invites_first()
  df_metric_1c_invites_followup <- get_df_metric_1c_invites_followup()
  df_metric_2_invites_accepted <- get_df_metric_2_invites_accepted()
  
  df_lhc_first_attended <- get_first_attended_lhc_per_participant() # helper data frame for 3a, 3b, 4a, 10 and 12
  df_metric_3a_attend_f2f <- get_df_metric_3a_attend_f2f()
  df_metric_3b_attend_tel <- get_df_metric_3b_attend_tel()
  df_metric_3c_dna_f2f <- get_df_metric_3c_dna_f2f()
  df_metric_3d_dna_tel <- get_df_metric_3d_dna_tel()
  
  df_metric_4a_ldct_referral <- get_df_metric_4a_ldct_referral()
  df_metric_4b_ldct_ineligible <- get_df_metric_4b_ldct_ineligible()
  df_metric_5a_ldct_initial <- get_df_metric_5a_ldct_initial()
  df_metric_5b_ldct_initial_dna <- get_df_metric_5b_ldct_initial_dna()
  df_metric_5d_ldct_3_month <- get_df_metric_5d_ldct_3_month()
  df_metric_5e_ldct_12_month <- get_df_metric_5e_ldct_12_month()
  df_metric_5f_ldct_24_month <- get_df_metric_5f_ldct_24_month()
  df_metric_5g_ldct_48_month <- get_df_metric_5g_ldct_48_month()
  df_metric_5h_ldct_24_month_surveillance <- get_df_metric_5h_ldct_24_month_surveillance
  # df_metric_6a6f <- get_df_metric_6_lung_cancers_from_aggregate() # nb, not required as already aggregate
  
  df_metric_7a_incidental_consolidation <- get_df_metric_7a_incidental_consolidation()
  df_metric_7b_incidental_tuberculosis <- get_df_metric_7b_incidental_tuberculosis()
  df_metric_7c_incidental_mediastinalmass <- get_df_metric_7c_incidental_mediastinal_mass()
  df_metric_7d_incidental_coronarycalcification <- get_df_metric_7d_incidental_coronary_calcification()
  df_metric_7e_incidental_aorticvalvecalcification <- get_df_metric_7e_incidental_aortic_valve_calcification()
  df_metric_7f_incidental_thoracicaorticaneurysm <- get_df_metric_7f_incidental_thoracic_aortic_aneurysm()
  df_metric_7g_incidental_pleuraleffusions <- get_df_metric_7g_incidental_pleural_effusions_thickening()
  df_metric_7h_incidental_suspiciousbreastlesion <- get_df_metric_7h_incidental_suspicious_breast_lesion()
  df_metric_7i_incidental_tyroidlesion <- get_df_metric_7i_incidental_thyroid_lesion()
  df_metric_7j_incidental_liverspleniclesions <- get_df_metric_7j_incidental_liver_or_splenic_lesions()
  df_metric_7k_incidental_renallesions <- get_df_metric_7k_incidental_renal_lesions()
  df_metric_7l_incidental_adrenallesions <- get_df_metric_7l_incidental_adrenal_lesions()
  df_metric_7m_incidental_abdominalaorticaneurysm <- get_df_metric_7m_incidental_abdominal_aortic_aneurysm()
  df_metric_7n_incidental_boneabnormalities <- get_df_metric_7n_incidental_bone_abnormalities()
  df_metric_7o_incidental_osteoporosis <- get_df_metric_7o_incidental_osteoporosis()
  df_metric_7p_incidental_fracturesnotrauma <- get_df_metric_7p_incidental_fractures_with_no_trauma_history()
  df_metric_7q_incidental_bronchiectasis <- get_df_metric_7q_bronchiectasis()
  df_metric_7r_incidental_respiratorybronchiolitis <- get_df_metric_7r_respiratory_bronchiolitis()
  df_metric_7s_incidental_interstitiallungabnormalities <- get_df_metric_7s_interstitial_lung_abnormalities()
  df_metric_7t_incidental_othercancers <- get_df_metric_7t_incidental_other_cancers()
  #df_metric_7t_incidental_othercancers_alt <- get_df_metric_7t_incidental_other_cancers_alternate() # nb, using the value supplied on the lung cancer data
  df_metric_7u_incidental_emphysema <- get_df_metric_7u_incidental_emphysema()
  df_metric_7_incidental_any <- get_df_metric_7_incidental_any() # nb, do this at the end of metric 7 series as relies on data loaded
  
  df_metric_8b_smoking_offered <- get_df_metric_8b_smoking_offered()
  df_metric_8a_smoking_started <- get_df_metric_8a_smoking_started()
  df_metric_9_smoking_completed <- get_df_metric_9_smoking_completed()
  
  df_metric_10_median_inv_to_lhc <- get_df_metric_10_median_inv_to_lhc()
  df_metric_11_median_inv_to_ldct <- get_df_metric_11_median_inv_to_ldct()
  df_metric_12_median_lhc_to_ldct <- get_df_metric_12_median_lhc_to_ldct()
  df_metric_13_median_inv_to_diag <- get_df_metric_13_median_inv_to_diag()
  
  df_metric_14a_lc_pathway_following_ldct <- get_df_metric_14a_lc_pathway_following_ldct()
  
})




## metric aggregation ----------------------------------------------------------
#' This process calculates aggregated performance for each metric.
#' The standard aggregation process groups by project and month, however, future
#' aggregations can include:
#' project only (for MI project summary sheet)
#' project and financial year (for MI project metric table)
#' demographics (e.g. ethnicity) for the support of demographic reports

## standard aggregation --------------------------------------------------------
# aggregates at project and month
with_progress({
  
  update_user(message = 'Aggregating metric performance', icon = '⏱️')
  p <- progressor(steps = 45)
  
  df_metric_1a <- calculate_metric_1a()
  df_metric_1c <- calculate_metric_1c()
  df_metric_2  <- calculate_metric_2()
  
  df_metric_3a <- calculate_metric_3a()
  df_metric_3b <- calculate_metric_3b()
  df_metric_3c <- calculate_metric_3c()
  df_metric_3d <- calculate_metric_3d()
  
  df_metric_4a <- calculate_metric_4a()
  df_metric_4b <- calculate_metric_4b()
  df_metric_5a <- calculate_metric_5a()
  df_metric_5b <- calculate_metric_5b()
  df_metric_5d <- calculate_metric_5d()
  df_metric_5e <- calculate_metric_5e()
  df_metric_5f <- calculate_metric_5f()
  df_metric_5g <- calculate_metric_5g()
  df_metric_5h <- calculate_metric_5h()
  df_metric_6a6f <- calculate_metric_6a6f()
  
  df_metric_7a <- calculate_metric_7a()
  df_metric_7b <- calculate_metric_7b()
  df_metric_7c <- calculate_metric_7c()
  df_metric_7d <- calculate_metric_7d()
  df_metric_7e <- calculate_metric_7e()
  df_metric_7f <- calculate_metric_7f()
  df_metric_7g <- calculate_metric_7g()
  df_metric_7h <- calculate_metric_7h()
  df_metric_7i <- calculate_metric_7i()
  df_metric_7j <- calculate_metric_7j()
  df_metric_7k <- calculate_metric_7k()
  df_metric_7l <- calculate_metric_7l()
  df_metric_7m <- calculate_metric_7m()
  df_metric_7n <- calculate_metric_7n()
  df_metric_7o <- calculate_metric_7o()
  df_metric_7p <- calculate_metric_7p()
  df_metric_7q <- calculate_metric_7q()
  df_metric_7r <- calculate_metric_7r()
  df_metric_7s <- calculate_metric_7s()
  #df_metric_7t <- calculate_metric_7t() nb, using the value from the lung cancer aggregate report
  df_metric_7u <- calculate_metric_7u()
  df_metric_7 <- calculate_metric_7() # nb, more efficient to do this after 7a-7u
  
  df_metric_8b <- calculate_metric_8b()
  df_metric_8a <- calculate_metric_8a()
  df_metric_9 <- calculate_metric_9()
  
  df_metric_10 <- calculate_metric_10()
  df_metric_11 <- calculate_metric_11()
  df_metric_12 <- calculate_metric_12()
  df_metric_13 <- calculate_metric_13()
  
  df_metric_14 <- calculate_metric_14a()
  
  
  ### combine metrics into a single table ---
  df_metrics <- bind_rows(
    df_metric_1a,
    df_metric_1c,
    df_metric_2,
    
    df_metric_3a,
    df_metric_3b,
    df_metric_3c,
    df_metric_3d,
    
    df_metric_4a,
    df_metric_4b,
    df_metric_5a,
    df_metric_5b,
    df_metric_5d,
    df_metric_5e,
    df_metric_5f,
    df_metric_5g,
    df_metric_5h, # NEW metric - surveillance scans following 24-month scan
    df_metric_6a6f,
    
    df_metric_7,
    df_metric_7a,
    df_metric_7b,
    df_metric_7c,
    df_metric_7d,
    df_metric_7e,
    df_metric_7f,
    df_metric_7g,
    df_metric_7h,
    df_metric_7i,
    df_metric_7j,
    df_metric_7k,
    df_metric_7l,
    df_metric_7m,
    df_metric_7n,
    df_metric_7o,
    df_metric_7p,
    df_metric_7q,
    df_metric_7r,
    df_metric_7s,
    #df_metric_7t, # nb, using the figures supplied in the lung cancer aggregate (df_metric_6a6f)
    df_metric_7u,
    
    df_metric_8b,
    df_metric_8a,
    df_metric_9,
    
    df_metric_10,
    df_metric_11,
    df_metric_12,
    df_metric_13,
    df_metric_14
    
  ) |>
    arrange(
      project,
      metric_id,
      month
    )
  
})


update_user(message = 'Metrics calculated')

# save metric output
df_metrics |>
  saveRDS(here('data', 'tlhc', 'tlhc_metrics.Rds'))
update_user(message = 'Metrics file saved')

# testing ----
# df_metric_7u_incidental_emphysema |> count(project, calc_ldct_date_corrected_yearmon) |> view()

# done!
update_user(stage = 'end')
toc()