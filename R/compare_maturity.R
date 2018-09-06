#' compare_maturity: R function to compute PIC, a probability for M1, M2 and M3 conditions by simulations
#'
#' @export
#' @usage compare_maturity(n.iter = 1000, msurv0=10, msurv1=20, nevents=88, Ta=16.6, N=166, lfu=0, p1=0.5)
#' @param n.iter: # of iterations, a positive integer value in (0, 100000)
#' @param msurv1: median survival time in treatment arm, a numeric value in (0, 100)
#' @param msurv0: median survival time in control arm, a numeric value in (0, 100)
#' @param nevents: # of events (can be computed by Ea() function),  a numeric value in (0, 1000)
#' @param Ta: accrual duration (can be computed by get_NTset() function), a numeric value in (0, 10000)
#' @param N: sample size (can be computed by get_NTset() function), a positive integer value in (nevents, 1000)
#' @param lfu: rate of loss-to-follow-up, a numeric value in (0, 1)
#' @param p1: allocation rate to treatment arm, a numeric value in (0, 1) (p0 = 1 - p1)
#'
#' @return T.m: mean of end of study time
#' @return Pic: a probability of an immature curve
#' @return M1: Proportion of events among all patients
#' @return M2: Median Follow-up among censored patiently only (m, sd)
#' @return M3: reverse KM for all patients (m, sd)
#'
#' @references Joo L. et al. ``Data Maturity and its Implications in Confirmatory Trials with Time-to-Event Endpoints." (2018)

compare_maturity <- function(n.iter=1000, msurv0, msurv1, nevents, Ta, N, lfu=0, p1=0.5) {
  results <- c()
  for (r in 1:n.iter) {
    breakpoint <- round(quantile(seq(n.iter),seq(1/3,1, length.out=3)))


    if (r %in% breakpoint) {
      print(paste(names(which(breakpoint==r)),"complete"),quote=F)
    }

    sim.data <- naive_sim(nevents, msurv0, msurv1, Ta, N, lfu, p1)
    kmfit   <- survival::survfit(survival::Surv(obs.survival, event.index)~trt,data=sim.data)
    obs0 <-sim.data$obs.survival[sim.data$event.index==0]-sim.data$time.enroll[sim.data$event.index==0]
    mfu0 <-median(ifelse(obs0>0, obs0, 0), na.rm=TRUE)
    rkmfit  <- survival::survfit(survival::Surv(obs.survival, 1-event.index)~1,data=sim.data)
    rKM <- c("NA")
    rKM  <- summary(rkmfit)$table['median']
    tab=c(tildeT = max(sim.data$obs.survival), S_T=min(summary(kmfit)$surv[summary(kmfit)$strata=="trt=1"]), nevents/N, mfu0, rKM)

   results<-rbind(results, tab)

  }
   stats <- c(T.m = mean(results[,1], na.rm=T),  Pic = mean(results[,2]>0.5, na.rm=T), M1=mean(results[,3], na.rm=T),
            M2.m=mean(results[,4], na.rm=T), M2.sd=sd(results[,4], na.rm=T), M3=mean(results[,5], na.rm=T), M3.sd=sd(results[,5], na.rm=T))
  return(stats)
}

