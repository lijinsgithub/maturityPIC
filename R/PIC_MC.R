#' PIC_MC: R function to compute PIC by MC
#' @export
#' @usage PIC_MC(n.iter = 10000, msurv0=10, msurv1=20, nevents=88, Ta=16.6, N=166, p1=0.5)
#' @param n.iter: # of iterations, a positive integer value in (0, 10000)
#' @param msurv1: median survival time in treatment arm, a numeric value in (0, 100)
#' @param msurv0: median survival time in control arm, a numeric value in (0, 100)
#' @param nevents: # of events (can be computed by Ea() function),  a numeric value in (0, 1000)
#' @param Ta: accrual duration (can be computed by get_NTset() function), a numeric value in (0, 10000)
#' @param N: sample size (can be computed by get_NTset() function), a positive integer value in (nevents, 1000)
#' @param p1: allocation rate to treatment arm, a numeric value in (0, 1) (p0 = 1 - p1)
#'
#' @return a tuple of (mean of end of study time (T.m), sd of end of study time (T.sd), a probability of an immature curve (Pic))
#'
#' @references Joo L. et al. ``Data Maturity and its Implications in Confirmatory Trials with Time-to-Event Endpoints." (2018)

PIC_MC<-function(n.iter, msurv0=10, msurv1=20, nevents, Ta, N, p1=0.5){

  one.arm = round(N*p1)
  N1 = one.arm
  N0 = N - N1

  lambda0 <- log(2)/msurv0
  lambda1 <- log(2)/msurv1

  seq.e <-c()
  seq.t <-c()

  seq.eos <-c()
  seq.eos1 <-c()

  for(i in 1:n.iter){

    entering <- runif(N, 0, Ta)
    survival <- c(rexp(N1, lambda1), rexp(N0, lambda0))
    Tobs <- entering  + survival
    trt  <- c(rep(1, N1), rep(0, N0))
    ordered.trt<-trt[order(Tobs)]
    eos = sort(Tobs)[nevents]

    censored <- ifelse(Tobs >= eos, eos, Tobs)
    fu <- censored - entering
    eos1 <- max(fu[ordered.trt==1])

    seq.eos <- c(seq.eos, eos)
    seq.eos1 <- c(seq.eos1, eos1)
    seq.t <- c(seq.t, median(survival[trt==1]))
  }


  return(c(T.m = mean(seq.eos), T.sd = sd(seq.eos), Pic= mean(seq.eos1 < seq.t)))
}

