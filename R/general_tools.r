#' Data frame loader
#' @description Loads a csv file via read.csv into a data frame with headers. 
#' Saves a few characters typing, so worth it in my book.
#' @return A data frame read in via read.csv with column headings taken from 
#' the first row of the file.
#' @export loader

loader<- function(){
  frame <-read.csv(file.choose(),header=T)
  return(frame)
}

#' Summary of data
#' @description Gives count, mean, standard deviation, standard error of the 
#' mean, and confidence interval (default 95%).
#' @param data A data frame containing the data to be summarised.
#' @param measurevar The name of a column that contains the variable to be 
#' summarized.
#' @param groupvars A vector containing names of columns that contain 
#' grouping variables.
#' @param error Error type (sd, se or ci) to be used. Defaults to sd.
#' @param na.rm A boolean that indicates whether to ignore NA's
#' @param conf.interval The percent range of the confidence interval (default 
#' is 95%).
#' @return A summary of the data, with the required measure of variability.
#' @export summarySD
summarySD <- function(data=NULL, measurevar, groupvars, error="sd", na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun= function(xx, col, na.rm) {
                   c( N    = length2(xx[,col], na.rm=na.rm),
                      mean = mean   (xx[,col], na.rm=na.rm),
                      sd   = sd     (xx[,col], na.rm=na.rm)
                   )
                 },
                 measurevar,
                 na.rm
  )
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult    
  if (error=="sd"){
    datac$low <- datac$mean - datac$sd
    datac$up <- datac$mean + datac$sd
  }
  else if (error=="se"){
    datac$low <- datac$mean - datac$se
    datac$up <- datac$mean + datac$se
  }
  else if (error=="ci"){
    datac$low <- datac$mean - datac$ci
    datac$up <- datac$mean + datac$ci
  }
  datac <- rename(datac, c("mean"=measurevar))
  
  return(datac)
}

#' Plot means and error bars for a single factor
#' @description Plots a standard 'mean and error bar figure for levels of 
#' a single factor. Factors represented as continuous variables must be 
#' converted to an actual factor (using as.factor) before use.
#' @param data The dataset containing the values to be plotted.
#' @param bar The column of the dataset representing the values to be averaged.
#' @param factor The column representing the grouping factor.
#' @param error The type of error value to be calculated. Options are 
#' "sd", "se" or "ci".
#' @return A bar plot (dynamite plot) of means and error bars for a single 
#' factor.
#' @export plotbar


plotbar<-function(data, bar, factor, error){
  require(ggplot2)
  summdata<-summarySD(data, measurevar=bar, groupvars=factor, error, na.rm=TRUE)
  ggplot(summdata, aes_string(x = factor, y = bar)) + 
    geom_bar(stat="identity", colour="black", size=.3) +
    geom_errorbar(aes(ymin=low, ymax=up), colour="black", width=.1) +
    ##    xlab("") +
    ##    ylab("") +
    theme_bw()
  
}
## plots mean and error bar as bar chart, for two factors, first as axis, second as legend
## must convert continuous factors to factors (as.factor) in original dataframe before use
#' Plot means and error bars for two factors
#' @description Plots a standard 'mean and error bar figure for levels of 
#' two factors. Factors represented as continuous variables must be 
#' converted to an actual factor (using as.factor) before use. First factor 
#' is represented as categories on the x-axis and the second as levels in 
#' the plot legend.
#' @param data The dataset containing the values to be plotted.
#' @param bar The column of the dataset representing the values to be averaged.
#' @param factor The column representing the grouping factor to be shown on 
#' the x-axis.
#' @param legend The column representing the grouping factor to be shown in 
#' the legend.
#' @param error The type of error value to be calculated. Options are 
#' "sd", "se" or "ci".
#' @return A bar plot (dynamite plot) of means and error bars for two factors.
#' @export plotbar2

plotbar2<-function(data, bar, factor, legend, error){
  summdata<-summarySD(data, measurevar=bar, groupvars=c(factor,legend), error, na.rm=TRUE)
  ggplot(summdata, aes_string(x = factor, y = bar, fill=legend)) + 
    geom_bar(stat="identity", position=position_dodge(), colour="black", size=.3) +
    geom_errorbar(aes(ymin=low, ymax=up), position=position_dodge(.9), colour="black", width=.1) +
    ##    xlab("") +
    ##    ylab("") +
    theme_bw()
  
}


#' Plot mean and error bar as line chart, for single factor
#' @description Plots a standard 'mean and error bar figure for levels of 
#' a single factor as a line plot. Factors represented as continuous variables 
#' must be converted to an actual factor (using as.factor) before use.
#' @param data The dataset containing the values to be plotted.
#' @param val The column of the dataset representing the values to be averaged.
#' @param factor The column representing the grouping factor (categories on 
#' the x-axis.
#' @param error The type of error value to be calculated. Options are 
#' "sd", "se" or "ci". Defaults to "sd".
#' @return A line plot of means and error bars for a single factor.
#' @export plotbar

## plots mean and error bar as line chart, for single factor

plotline<-function(data, val, factor, error="sd"){
  summdata<-summarySD(data, measurevar=val, groupvars=factor, error, na.rm=TRUE)
  ggplot(summdata, aes_string(x=factor, y=val)) + 
    geom_errorbar(aes(ymin=low, ymax=up), colour="black", width=.1) +
    geom_line() +
    geom_point(size=3, shape=21, fill="white") + # 21 is filled circle
    theme_bw()
}

## plots mean and error bar as line chart, for two factors, first as axis, second as legend

plotline2<-function(data, val, factor, legend, error){
  summdata<-summarySD(data, measurevar=val, groupvars=c(factor,legend), error, na.rm=TRUE)
  ggplot(summdata, aes_string(x=factor, y=val, colour=legend)) + 
    geom_errorbar(aes(ymin=low, ymax=up), colour="black", width=.1) +
    geom_line() +
    geom_point(size=3, shape=21, fill="white") + # 21 is filled circle
    theme_bw()
}


checkme<-function(model, resp){
  plot(model)
  plot(model,resp~fitted(.))
  qqnorm(model,~ resid(.))
}

consum<-function(){
  options(contrasts=c('contr.sum','contr.poly'))
}
contre<-function(){
  options(contrasts=c('contr.treatment','contr.poly'))
}


#' Transpose data layout
#' @description Transposes a dataset, correctly processing column and
#' row labels.
#' @param df A dataframe containing abundances of invertebrate taxa in
#' different samples.
#' @return A data frame transposing the input data, with row and column
#' labels processed correctly.
#' @export transposedata
#' @examples
#' # transpose the built-in River Almond dataset
#' # this would have to be transposed back to original format for calculation
#'
#' transposedata(almond)

transposedata<-function(df){
  
  rowlabs<-df[,1]
  
  collabs<-names(df)
  
  df_t<-as.data.frame(t(df[,-1]))
  
  names(df_t)<-rowlabs
  
  df_t<-cbind(as.factor(collabs[-1]), df_t)
  
  row.names(df_t)<-NULL
  
  names(df_t)[1]<-""
  
  return(df_t)
}
