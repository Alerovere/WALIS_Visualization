join_age_rsl <- function(age,rsl,age_percentiles = c(0.001,0.023,0.159,0.50,0.841,0.977,0.995)){
  result <- c()
  if(age$type.of.datapoint == rsl$type.of.datapoint){
    type <- age$type.of.datapoint
    names_q <- paste0('Age.',age_percentiles*100,'.perc')
    if (type != 'Sea Level Indicator'){
      
      if(!is.null(age$sample)){
        quant <- quantile(age$sample,age_percentiles)
        names(quant) <- names_q
        quant <- c(quant,c(Age.percentiles.from = 'Sample'))
      }
      
      else{
        
        if(age$parameters$distribution == 'Uniform'){
          
          quant <- qunif(age_percentiles,age$parameters$Lower.age,age$parameters$Upper.age)
          names(quant) <- names_q
        }
        else if (age$parameters$distribution == 'Normal'){
          quant <- qnorm(age_percentiles,mean = age$parameters$Age_mu,age$parameters$Age_2s)
          names(quant) <- names_q
        }
        quant_age <- c(quant,c(Age.percentiles.from = 'Parameters'))
        
        indicator <- c(WALIS_ID = unique(age$distributions$total$WALIS_ID),
                       type.of.datapoint = type,
                       Paleo.RSL..m. = rsl$param$Paleo.RSL..m.,
                       Paleo.RSL..uncertainty..m. = rsl$param$Paleo.RSL..uncertainty..m.)
        indicator <- c(indicator,quant_age)
        result$limiting <- indicator
        
      }

    }
    
    else {
      indicator <- data.frame(WALIS_ID = unique(age$distributions$total$WALIS_ID),
                              RSL = rsl$sample,
                              AGE = age$sample)
      result$sli_sample <- indicator
    }
    
  }
  return(result)
}
