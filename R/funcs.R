# probability of event
hindcastSSO <- function(t0, t1, dw, dp, indvar0, modin){
  
  ##
  # sanity checks
  if(dw > 0.5)
    stop('Water level too high, dw mustbe < 0.5 m')
  if(dp > 1.8)
    stop('2-d precipitation too high, dp must be < 1.8 cm')

  # get mean, sd of water level, precip for training data
  mnw <- mean(indvar0$water, na.rm = T)
  mnp <- mean(indvar0$precp, na.rm = T)
  sdw <- sd(indvar0$water, na.rm = T)
  sdp <- sd(indvar0$precp, na.rm = T)
  
  # input data for predictions
  indvar <- indvar0
  indvar$precp <- indvar$precp + dp
  indvar$water <- indvar$water + dw

  # subset indvar by input date range
  ok <- with(indvar, date >= t0 & date< t1)
  indvar <- indvar[ok, ]
  time <- indvar$date[ok]

  # find which water levels are below the mean
  w2 <- indvar[, 3]
  low <- w2 < mnw

  # get predictions from model, assign prob of zero if water level below mean
  prds <- predict(rskmod, newdata = indvar, type = 'resp')
  prds[low] <- 0
  
  # output
  indvar$risk <- prds
  
  return(indvar)

}
