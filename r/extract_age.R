combine_peaks <- function(initial_dist,peaks_dist){
  distri<-c()
  if(nrow(peaks_dist[[1]])>0){
    peaks_dist[[1]]$prob<-unlist(rep(initial_dist['prob']/nrow(peaks_dist[[1]]),nrow(peaks_dist[[1]])))
    distri<-peaks_dist
  }
  else{
    distri<-initial_dist[c('Lower.age','Upper.age','prob')]
  }
  distri<-data.frame(distri)
  return(distri)
}

sampling<-function(df,n_samples){
  my_sample<-lapply(1:n_samples, function(x) dist_funct(df[sample(1:nrow(df),1,prob=df$prob),]))
  return(unlist(my_sample))
}

intersec<-function(a,b,c,d){
  in_range <- if(a<d && c<b) TRUE else FALSE
  return(in_range)
}

peak_intersection<-function(peaks,upp,low){
  int<-lapply(1:nrow(peaks),function(x) intersec(peaks[x,'up'],peaks[x,'low'],upp,low))
  peaks_in_range <- peaks[unlist(int),c('up','low')]
  colnames(peaks_in_range) <- c('Lower.age','Upper.age')
  return(peaks_in_range)
}

extract_age <- function(df, n_samples, peaks= c()) {
  
  # Sample around peaks FALSE
  uniform_peaks <-  FALSE
  
  # If peaks are provided, sample around peaks
  if (length(peaks)>0){
    uniform_peaks <- TRUE
  }
  
  merge <- c()
  merge$distributions$total <- df[c('WALIS_ID','Type.of.datapoint','RSL.Indicator',
                                  'Age.calculation.from','Age_mu','Age_2s',
                                  'Lower.age','Upper.age')]  %>% st_drop_geometry()
  
  rsl_type <- unique(merge$distributions$total$Type.of.datapoint)[[1]]
  merge$type.of.datapoint <- rsl_type
  merge$distributions$total$prob <- 1 / nrow(merge$distributions$total)
  df_temp <- merge$distributions$total
  
  merge$distributions$uniform <- df_temp[df_temp$Age.calculation.from ==
                                    'Uniform distribution from MIS assignment',]
  merge$distributions$normal <- df_temp[df_temp$Age.calculation.from ==
                                     'Radiometric dating', ]
  if (uniform_peaks == TRUE && nrow(merge$distributions$uniform) > 0) {
    uniform <- merge$distributions$uniform
    ranges <-
      lapply(1:nrow(uniform), function(x)
        peak_intersection(peaks, uniform[x, 'Lower.age'], uniform[x, 'Upper.age']))
    uniform_peaks_list <-
      lapply(1:nrow(uniform), function(x)
        combine_peaks(uniform[x, ], ranges[x]))
    merge$distributions$uniform_peaks <-
      do.call("rbind", uniform_peaks_list)
    merge$distributions$uniform_peaks$WALIS_ID <- unique(df_temp$WALIS_ID)[1]
    merge$distributions$uniform_peaks$Type.of.datapoint <- rsl_type
    merge$distributions$uniform_peaks$RSL.Indicator <- unique(uniform$RSL.Indicator)[1]
    merge$distributions$uniform_peaks$Age.calculation.from <- "Uniform distribution from MIS assignment"
    merge$distributions$uniform_peaks$Age_mu <- NA
    merge$distributions$uniform_peaks$Age_2s <- NA
    merge$distributions$total <-
      merge$distributions$total[merge$distributions$total$Age.calculation.from != 'Uniform distribution from MIS assignment', ]
    merge$distributions$total <-
      rbind(merge$distributions$total,
            merge$distributions$uniform_peaks)
  }
  
  if (rsl_type == 'Sea Level Indicator') {
    merge$sample<-c(sampling(merge$distributions$total,n_samples = n_samples))
  }
  
  else{
    if(nrow(merge$distributions$total)==1){
      if(nrow(merge$distributions$normal)>0){
        merge$parameters<-list(distribution='Normal',Age_mu=merge$distributions$total[1,'Age_mu'],Age_2s=merge$distributions$total[1,'Age_2s'])
      }
      else{
        merge$parameters<-list(distribution='Uniform',Lower.age=merge$distributions$total[1,'Lower.age'],Upper.age=merge$distributions$total[1,'Upper.age'])
      }
    }
    else{
      merge$sample<-c(sampling(merge$distributions$total,n_samples = n_samples))
    }
  }
  return(merge)
}

dist_funct<-function(dist){
  if (dist[['Age.calculation.from']] == 'Radiometric dating'){
    s = rnorm(1,dist[['Age_mu']],dist[['Age_2s']]/2)
  }
  else if (dist[['Age.calculation.from']] == 'Uniform distribution from MIS assignment'){
    s = runif(1,dist[['Lower.age']],dist[['Upper.age']])
  }
  return(s)
}
