#' PIC_naive: R function to compute PIC by naive approach
#' @export
#' @usage PIC_Naive(n.iter = 10000, msurv0=10, msurv1=20, nevents=88, Ta=16.6, N=166, lfu=0, p1=0.5)
#' @param n.iter: # of iterations, a positive integer value in (0, 100000)
#' @param msurv1: median survival time in treatment arm, a numeric value in (0, 100)
#' @param msurv0: median survival time in control arm, a numeric value in (0, 100)
#' @param nevents: # of events (can be computed by Ea() function),  a numeric value in (0, 1000)
#' @param Ta: accrual duration (can be computed by get_NTset() function), a numeric value in (0, 10000)
#' @param N: sample size (can be computed by get_NTset() function), a positive integer value in (nevents, 1000)
#' @param lfu: rate of loss-to-follow-up, a numeric value in (0, 1)
#' @param p1: allocation rate to treatment arm, a numeric value in (0, 1) (p0 = 1 - p1)
#'
#' @return a tuple of (mean of end of study time (T.m), sd of end of study time (T.sd), a probability of an immature curve (Pic))
#'
#' @references Joo L. et al. ``Data Maturity and its Implications in Confirmatory Trials with Time-to-Event Endpoints." (2018)

PIC_Naive <- function(n.iter=10000, msurv0, msurv1, nevents, Ta, N, lfu=0, p1=0.5) {


  results <- c()
  for (r in 1:n.iter) {
    breakpoint <- round(quantile(seq(n.iter),seq(1/3,1, length.out=3)))


    if (r %in% breakpoint) {
      print(paste(names(which(breakpoint==r)),"complete"),quote=F)
      }

  sim.data <- naive_sim(nevents, msurv0, msurv1, Ta, N, lfu, p1)
  kmfit   <- survival::survfit(survival::Surv(obs.survival, event.index)~trt,data=sim.data)
  tab=c(tildeT = max(sim.data$obs.survival), S_T=min(summary(kmfit)$surv[summary(kmfit)$strata=="trt=1"]))
  results <- rbind(results, tab)

  }
  stats <- c(T.m = mean(results[,1], na.rm=T), T.sd = sd(results[,1], na.rm=T), Pic = mean(results[,2]>0.5))
  return(stats)
  }




