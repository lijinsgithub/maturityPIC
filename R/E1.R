#' E1: R function to compute the number of events in one study arm under the PH assumption
#' @export
#' @usage E1(t = 24, Ta = 12, ra = 10, lambda =log(2)/10)
#' @param t: time of analysis, a numeric value in (0, 1000)
#' @param Ta: accrual duration, a numeric value in (0, 1000)
#' @param ra: accural rate, a numeric value in (0, 100)
#' @param lambda: hazard rate, a numeric value in (0, 1)
#' @return # of exepected events, an integer value (rounded-up)
#'
#' @references Kim and Tsiatis. ``Study duration for clinical trials with survival response and early stopping rule." Biometrics (1990): 81-92.

E1 <- function (t, Ta, ra, lambda){
  t = t
  Ta = Ta
  ra = ra
  lambda = lambda
  if(t >= Ta){
    ee = ra*(Ta - exp(-lambda*t)/lambda*(exp(lambda*Ta) - 1))
  } else {

    ee = ra*(t - (1-exp(-lambda*t))/lambda)
  }
  ceiling(ee)
}

