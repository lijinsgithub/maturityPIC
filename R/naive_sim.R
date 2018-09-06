#' naive_sim: R function to simulate survival outcomes for a two arm study
#' @export
#' @usage naive.sim(nevents=88, lambda0=log(2)/10, lambda1=log(2)/20, Ta, N, lfu=0, p1=0.5)
#' @param nevents: # of events (a value from Ea() function),  a numeric value in (0, 1000)
#' @param msurv1: median survival time in treatment arm, a numeric value in (0, 100)
#' @param msurv0: median survival time in control arm, a numeric value in (0, 100)
#' @param Ta: accrual duration (can compute with get_NTset() function), a numeric value in (0, 10000)
#' @param N: sample size (can compute with get_NTset() function), a positive integer value in (nevents, 1000)
#' @param lfu: rate of loss-to-follow-up, a numeric value in (0, 1)
#' @param p1: allocation rate to treatment arm, a numeric value in (0, 1) (p0 = 1 - p1)
#'
#' @return data= [group, time to enroll, time to observe, event.index (1=event, 0=censored)]
#'
#' @references Joo L. et al. ``Data Maturity and its Implications in Confirmatory Trials with Time-to-Event Endpoints." (2018)


naive_sim <- function(nevents, msurv0, msurv1, Ta, N, lfu, p1) {

  lambda0 <- log(2)/msurv0
  lambda1 <- log(2)/msurv1
  arm1 = round(p1*N)
  arm0 = N - arm1

  trt <- c(rep(0, arm0), rep(1, arm1))

  time.surv <- c(rexp(arm0, lambda0), rexp(arm1, lambda1))

  time.enroll <-  runif(N, 0, Ta)                                   # uniform accrual time #
  censor.lf <- rep(1, N)                                            # censoring indicator  #

  if (lfu!=0){
    lambda.loss <- (1-lfu)/lfu                                      # individual loss hazard rates
    time.loss   <-  c(rexp(arm0, lambda0*lfu), rexp(arm1, lambda1*lfu))                             # follow-up time
    censor.lf[time.loss<time.surv] <- 0                             # censored due to loss of follow-up
    time.obs    <- pmin(time.loss,time.surv)                        # observed survival time
  } else {
    time.obs <- time.surv
  }

  time.total <- time.enroll+time.obs                                # include time of enrollment, needed for final analysis

  stop.index <- which(cumsum(censor.lf)==nevents)[1]
  time.anal  <- sort(time.total)[stop.index]

  event.index <- ifelse(time.total>time.anal, 0, censor.lf)

  final.time   <- time.total*I(time.total <= time.anal) + time.anal*I(time.total > time.anal)   # censor observations that go beyond study period
  obs.survival <- ifelse((final.time - time.enroll) > 0, final.time - time.enroll, 0)

  data <- cbind(trt, time.enroll,  obs.survival, event.index)
  data <- data.frame(data)

}
