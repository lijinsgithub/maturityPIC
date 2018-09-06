#' get_N: R function to compute the number of events in two study arms under the PH assumption
#' @export
#' @usage get_N(Ts = 24, ra = 10, msurv1 = 20, msurv0 = 10)
#' @param Ta: accrual duration,  a numeric value in (0, 1000)
#' @param ra: accural rate, a numeric value in (0, 100)
#' @param msurv1: median survival time in treatment arm, a numeric value in (0, 100)
#' @param msurv0: median survival time in control arm, a numeric value in (0, 100)
#' @param alpha: type I error of one-sided log-rank test, a numeric value in (0, 1)
#' @param power: power required to observe the significant result under current settings, a numeric value in (0, 1)
#' @param p1: allocation rate to treatment arm, a numeric value in (0, 1) (p0 = 1 - p1)
#'
#' @return N: sample size, a positive integer value (rounded-up)
#'
#' @references Kim, Kyungmann, and Anastasios A. Tsiatis. ``Study duration for clinical trials with survival response and early stopping rule." Biometrics (1990): 81-92.


get_N <- function (Ts, ra, msurv0, msurv1, alpha =0.025, power = 0.9, p1 =0.5){

  Ts = Ts

  lam0 = log(2)/msurv0
  lam1 = log(2)/msurv1
  alpha = alpha
  power = power
  p1 = p1

  HR0 = lam1/lam0
  event <- Ea(HR=HR0, alpha=alpha, power=power, p1=p1)


  obj<-function(n) {E2(t = Ts, Ta = n/ra, ra =ra, lambda0 = lam0, lambda1=lam1) - event}

  oldw <- getOption("warn")
  options(warn = -1)

  sol <- uniroot(obj, c(0, 10000))$root

  options(warn = oldw)
  if(is.na(sol)==TRUE){
    print("no solution found for N")
  } else if(sol < event){
    print("N < events, non-sense design")
    break
  }
  ceiling(sol)

}


