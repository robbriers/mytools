#' Chi-squared contingency table by randomisation
#' @description  function to determine significance of chi-squared goodness 
#' of fit by randomisation to deal with low frequencies.
#' @param observed A dataframe with a single column of observed frequencies
#' @param rands The number of randomisations to perform
#' 
chisq.randomised<-function(observed, rands){
  
  # function to determine significance of chi-squared goodness of fit by
  # randomisation to deal with low frequencies.
  # input is a dataframe with a single column of observed frequencies 
  # (observed) and a specification of the number of randomisations (rands)
  
  # set sig to zero
  sig<-0
  # convert dataframe input to numeric vector
  observed<-as.vector(observed[[1]], mode="numeric")
  # set total number of observations
  total<-sum(observed)
  # turn off warnings to avoid chi-sq warnings about low freqs
  oldw<-getOption("warn")
  options(warn=-1)
  # calculate value of chisq for observed data
  obschi<-chisq.test(observed)
  # actual chisq value is first part of the list
  obschi<-obschi[[1]]
  
  # set up loop for randomisation, probably should use 'apply' for speed
  for (i in 1:rands){
    # create a random sample 
    samp<-sample(1:length(observed), total, replace=TRUE)
    # convert to frequency table and then to numeric vector
    samp<-table(samp)
    samp<-as.data.frame(samp)
    samp<-samp$Freq
    # calculate chi-sq for random sample
    randchi<-chisq.test(samp)
    randchi<-randchi[[1]]
    # if random chi sq is greater than observed, increment sig
    if (randchi>obschi){
      sig<-sig+1
    }
    # end of for loop  
  }
  # output results
  print("Observed chi-squared")
  print(obschi)
  if (sig > 0){
    sig<-sig/rands
    print("Significance of observed chi-squared is")
    print(sig)
  } else {
    sig<-1/rands
    print("Significance of observed chi-squared is less than")
    print(sig)}
  # turn warnings back on
  options(warn=oldw)
}