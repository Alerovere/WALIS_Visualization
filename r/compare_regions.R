extract_slip_region <- function(area_sli, sl_stack = c(),n_sampling=10000){
  
  ## 1. Define Sea level peaks 
  sl_peaks = c()
  if (length(sl_stack)>0){
    sl_peaks <- define_peaks_ranges(sl_stack)
  }
  
  ## 2. Sample Sea level indicators
  
  if(nrow(area_sli)>0){
  
  # Age
  pboptions(type = "timer")
  system.time(age <- pblapply(unique(area_sli$WALIS_ID), function(x) 
    extract_age(area_sli[area_sli$WALIS_ID==x,], n_samples = n_sampling, peaks= sl_peaks)))
  
  # RSL
  
  system.time(rls <- pblapply(unique(area_sli$WALIS_ID), function(x) 
    extract_rsl(area_sli[area_sli$WALIS_ID==x,], n_samples = n_sampling)))
  
  # Join Age and RSL
  age_rsl_area <- lapply(1:length(age),function(x) join_age_rsl(age[[x]],rls[[x]]))
  
  
  # 3. Extract features
  # Extract Sea level indicators
  
  sli_sample <- lapply(age_rsl_area, '[[','sli_sample')
  sli_sample <- sli_sample[!sapply(sli_sample,function(x) is.null(x))]
  sli_area <- bind_rows(sli_sample)
  }
  else{
    sli_area = data.frame()
  }
  return(sli_area)
  
}
