#' E2: R function to compute the number of events in two study arms under the PH assumption
#' @export
#' @usage E2(t = 24, Ta = 12, ra = 10, lambda1 = log(2)/20, lambda0 = log(2)/10)
#' @param t: time of analysis, a numeric value in (0, 1000)
#' @param Ta: accrual time, a numeric value in (0, 1000)
#' @param ra: accural rate, a numeric value in (0, 100)
#' @param lambda1: hazard rate for treatment arm, a numeric value in (0, 1)
#' @param lambda0: hazard rate for control arm, a numeric value in (0, 1)
#' @return # of exepected events, an integer value (rounding-up)
#'
#' @references Kim, Kyungmann, and Anastasios A. Tsiatis. ``Study duration for clinical trials with survival response and early stopping rule." Biometrics (1990): 81-92.


E2 <- function (t, Ta, ra, lambda0, lambda1){
  lam0 = lambda0
  lam1 = lambda1
  ee0 <- E1(t, Ta, ra, lam0)
  ee1 <- E1(t, Ta, ra, lam1)
  ee  <- (ee0 + ee1)/2
  ceiling(ee)
}
