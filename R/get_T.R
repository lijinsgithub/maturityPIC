#' get_T: R function to compute the study duration in two study arms under the PH assumption
#' @usage get_T(Ta = 12, ra = 10, msurv1 = 20, msurv0 = 10, d=1)
#' @param Ta: accrual duration, a numeric value in (0, 10000)
#' @param ra: accural rate, a numeric value in (0, 100)
#' @param msurv1: median survival time in treatment arm, a numeric value in (0, 100)
#' @param msurv0: median survival time in control arm, a numeric value in (0, 100)
#' @param alpha: type I error of one-sided log-rank test, a numeric value in (0, 1)
#' @param power: power required to observe the significant result under current settings, a numeric value in (0, 1)
#' @param p1: allocation rate to treatment arm, a numeric value in (0, 1) (p0 = 1 - p1)
#' @param d: a number of decimal places, an positive integer value, default = 1
#' @return Ts, study duration, a numeric value in (0, 1000)
#'
#' @references Kim, Kyungmann, and Anastasios A. Tsiatis. ``Study duration for clinical trials with survival response and early stopping rule." Biometrics (1990): 81-92.


get_T <- function (Ta, ra, msurv0, msurv1, alpha = 0.025, power = 0.9, p1 = 0.5, d=1){

  Ta = Ta
  ra = ra
  lam0 = log(2)/msurv0
  lam1 = log(2)/msurv1
  a0 = alpha
  power = power
  p1 = p1
  d=d
  event <- Ea(HR=lam1/lam0, alpha=a0, power=power, p1=p1)

  if(Ta <= event/ra){
    print("Ta is too short for current HR and ra")
    break
  }
  obj <- function(t) E2(t = t, Ta = Ta, ra = ra, lambda0=lam0, lambda1=lam1) - event

  oldw <- getOption("warn")
  options(warn = -1)
  sol <- rootSolve::uniroot.all(obj, c(0, 10000))[1]
  options(warn = oldw)
  if(is.na(sol)==TRUE){
    print("no solution found for Ts")

  } else if(sol<Ta){
    print("Ts < Ta, need to increase ra or shorten Ta")
   break
  }
  round(sol, d)

}

