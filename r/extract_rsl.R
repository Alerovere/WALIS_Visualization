### Gamma parameters 
## Based on : https://gist.github.com/andrie/2ea43547ee02f3aa0e36/revisions

errorGamma <- function(p=c(1, 3), quantiles, exp){
  gx <- qgamma(p=quantiles, shape=p[1], rate=p[2])
  sum((gx-exp)^2)
}

gamma_parameters<- function(quantiles=c(0.023,0.977),values=c()){
  values <- values*-1
  solution <- nlm(f=errorGamma, p=c(1, 2), quantiles=quantiles, exp=values)
  parameters <- list(shape=solution$estimate[1],rate=solution$estimate[2])
  return(parameters)
}

dist_funct_rsl<-function(dist){
  if (dist[['rsl.calculation.from']] == 'Normal'){
    s <-  rnorm(1,dist[['Paleo.RSL..m.']],dist[['Paleo.RSL.uncertainty..m.']])
  }
  else if (dist['rsl.calculation.from'] == 'Combine'){
    n <-  rnorm(1,dist[['Elevation..m.']],dist[['Elevation.error..m.']]) 
    k <-  rgamma(1,dist[['gamma.shape']],dist[['gamma.rate']]) # RWL
    s <- n+k
  }
  return(s)
}

sampling_rsl<-function(df,n_samples){
  my_sample<-lapply(1:n_samples, function(x) dist_funct_rsl(df[sample(1:nrow(df),1,prob=df$prob),]))
  return(unlist(my_sample))
}

extract_rsl <- function(df,n_samples){
  result<- c()
  result$distribution$total <- df[c('WALIS_ID','Type.of.datapoint','RSL.Indicator',
                                    'Upper.limit.of.living.range..m.',
                                    'Lower.limit.of.living.range..m.',
                                    'Paleo.RSL..m.',
                                    'Paleo.RSL.uncertainty..m.',
                                    'Elevation..m.',
                                    'Elevation.error..m.')] %>% st_drop_geometry()
  rsl_type <- unique(result$distribution$total$Type.of.datapoint)[[1]]
  result$type.of.datapoint <- rsl_type
  
  if(rsl_type=='Marine Limiting'){
    limit_max <- which.max(result$distribution$total$Paleo.RSL..m.)
    result$param <- list(type='Marine Limiting',
                         Paleo.RSL..m.= result$distribution$total$Paleo.RSL..m.[limit_max ],
                         Paleo.RSL..uncertainty..m = result$distribution$total$Paleo.RSL.uncertainty..m.[limit_max])}
  else if (rsl_type=='Terrestrial Limiting'){
    limit_min <- which.min(result$distribution$total$Paleo.RSL..m.)
    result$param <- list(type='Terrestrial Limiting',
                         Paleo.RSL..m. = result$distribution$total$Paleo.RSL..m.[limit_min ],
                         Paleo.RSL..uncertainty..m. =  result$distribution$total$Paleo.RSL.uncertainty..m.[limit_min])}
  else{
    result$distribution$total$rsl.calculation.from <- ifelse(result$distribution$total$RSL.Indicator!='Single Coral', 'Normal', 'Combine')
    df_temp <- result$distribution$total
    parameters_list <-
      sapply(1:nrow(df_temp), function(x) 
        if (df_temp[x, 'RSL.Indicator'] == 'Single Coral') {
          gamma_parameters(values = c(df_temp[x, 'Upper.limit.of.living.range..m.'], df_temp[x, 'Lower.limit.of.living.range..m.']))
        }
        else{c(NA,NA)})
    matrix_param <- matrix(unlist(parameters_list),ncol=2,byrow=TRUE)
    result$distribution$total$gamma.shape <- matrix_param[,1]
    result$distribution$total$gamma.rate <- matrix_param[,2]
    result$distribution$total$prob <- 1/nrow(result$distribution$total)
    result$sample <- sampling_rsl(result$distribution$total, n_samples = n_samples)
  }
  return(result)
}

