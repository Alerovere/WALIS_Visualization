library(pracma)

# Function to extract peaks from stack of RSL
define_peaks_ranges <- function(df,age_name='Age.ka.',mean_rsl='X50.',width=0.2){
  peaks <- data.frame(pracma::findpeaks(df[[mean_rsl]]))
  peaks$low<-peaks$X2+(peaks$X4-peaks$X3)*(width/2)
  peaks$up<-peaks$X2-(peaks$X4-peaks$X3)*(width/2)
  return(peaks)
}


