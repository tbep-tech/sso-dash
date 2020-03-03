# probability of event
hindcastSSO <- function(t0, t1, dw, dp, indvar0, modprm){
  
  ##
  # sanity checks
  if(dw > 0.5)
    stop('Water level too high, dw mustbe < 0.5 m')
  if(dp > 1.8)
    stop('2-d precipitation too high, dp must be < 1.8 cm')

  # get mean, sd of water level, precip for training data
  mnw <- mean(indvar0[, 3], na.rm = T)
  mnp <- mean(indvar0[, 1], na.rm = T)
  sdw <- sd(indvar0[, 3], na.rm = T)
  sdp <- sd(indvar0[, 1], na.rm = T)
  

  indvar <- indvar0
  indvar[, 1] <- indvar[, 1] + dp
  indvar[, 3] <- indvar[, 3] + dw

  # subset indvar by input date range
  ok <- with(indvar, stime0 >= t0 & stime0 < t1)
  indvar <- indvar[ok, ]
  time <- indvar$stime0[ok]

  # find which water levels are below the mean
  w2 <- indvar[, 3]
  low <- w2 < mnw

  #------------------------------------------------
  # execute regression to calculate p
  # probability of non-event =(1-p)
  #------------------------------------------------

  coeff <- modprm$coeff
  stats <- modprm$stats
  
  # [pihat,dlow,dhi] = mnrval(coeff,indvar,stats);
  # p = pihat(:,2);  #probability of event
  # p(low) = 0;

}
