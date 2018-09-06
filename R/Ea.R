#' Ea: R function to compute the number of events to reject a log-rank test with given power and alpha.
#' @export
#' @usage Ea(HR=0.5, alpha=0.025, power=0.9, p1=0.5)
#' @param HR: Hazard Ratio, a numeric value in (0, 1)
#' @param alpha: type I error of one-sided log-rank test, a numeric value in (0, 1)
#' @param power: power required to observe the significant result under current settings, a numeric value in (0, 1)
#' @param p1: allocation rate to treatment arm, a numeric value in (0, 1) (p0 = 1 - p1)
#' @return # of exepected events, an integer value (rounding-up)
#'
#' @references Schoenfeld, David. "Sample-size formula for the proportional-hazards regression model." Biometrics (1983): 499-503.
Ea <- function (HR, alpha, power, p1){
  HR = HR
  alpha = alpha
  power = power
  p1 = p1
  if(HR > 1){
    HR <- 1/HR
  }
 num = (abs(qnorm(alpha)) + abs(qnorm(power)))^2
 denom = p1*(1-p1)*(log(HR))^2
   ceiling(num/denom)
}
