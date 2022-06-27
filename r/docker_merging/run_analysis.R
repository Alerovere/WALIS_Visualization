library(pracma)
library(pbapply)
library(dplyr)
args = commandArgs(trailingOnly=TRUE)
source('r/extract_age.R')
source('r/extract_rsl.R')
source('r/join_age_rsl.R')
source('r/define_peaks_ranges.R')

if (length(args)!=3) {
  stop("All arguments need to be suplied", call.=FALSE)
} else {
  dataset_file <- args[1]
  n_sampling <- strtoi(args[2])
  sl_peaks_s <- args[3]
}

processing <- function(dataset_file,n_sampling,sl_peaks_s){
  
  set.seed(200)
  # Load dataset
  walis_df <- read.csv(dataset_file,sep=',')
  
  #Patch
  
  walis_df[walis_df$Timing.constraint == "Equal to ", "Timing.constraint"] = "Equal to"
  walis_df <- walis_df[walis_df$Type.of.datapoint == "Sea Level Indicator" & walis_df$Timing.constraint=='Equal to',]
  walis_df <- walis_df[is.na(walis_df$Elevation..m.) == FALSE,]
  
  # Define sea level peaks for sampling 
  
  if(sl_peaks_s=='T'){
    
    sl_stack <- read.csv('Data/sea_level_stack_spratt.csv')
    sl_peaks <- define_peaks_ranges(sl_stack)
    
  }else{
    sl_peaks = c()
  }
  
  # Starts processing
  
  # Extract Age
  
  print('Merging Age values ....')
  age <- pblapply(unique(walis_df$WALIS_ID), function(x) {
    extract_age(walis_df[walis_df$WALIS_ID==x,], n_samples = n_sampling, peaks = sl_peaks, shiny = FALSE)})
  
  # Extract RSL
  set.seed(100)
  print('Merging RSL values ....')
  rsl <- pblapply(unique(walis_df$WALIS_ID), function(x) {extract_rsl(walis_df[walis_df$WALIS_ID==x,], n_samples = n_sampling,shiny = FALSE)})
  
  # Join Age and RSL
  age_rsl_area <-
    lapply(1:length(age), function(x)
      join_age_rsl(age[[x]], rsl[[x]]))
  
  # 3. Extract features
  # Extract Sea level indicators
  
  sli_sample <- lapply(age_rsl_area, '[[', 'sli_sample')
  sli_sample <-
    sli_sample[!sapply(sli_sample, function(x)
      is.null(x))]
  sli_area <- bind_rows(sli_sample)
  return(sli_area)
}

p <- processing(dataset_file,n_sampling,sl_peaks_s)
write.csv(p,file = 'cloudpoint_output.csv')
f <- read.csv('cloudpoint_output.csv')
print(f)



