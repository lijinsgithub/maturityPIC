#' get_NTset: R function to generate a series of sets of N and T when ra, lambda0, lambda1 are given.
#' @export
#' @usage get_NTset(ra=10, msurv0=10, msurv1=20, alpha=0.025, power=0.9, p1=0.5, ta.d=0.1)
#' @param ra: accural rate, a numeric value in (0, 100)
#' @param msurv1: median survival time in treatment arm, a numeric value in (0, 100)
#' @param msurv0: median survival time in control arm, a numeric value in (0, 100)
#' @param alpha: type I error of one-sided log-rank test, a numeric value in (0, 1)
#' @param power: power required to observe the significant result under current settings, a numeric value in (0, 1)
#' @param p1: allocation rate to treatment arm, a numeric value in (0, 1) (p0 = 1 - p1)
#' @param d: a number of decimal places (used for get_T), an positive integer value, default = 2
#' @param ta.d: Ta grid size , a numeric value in (0, 1)
#'
#' @return a tuple of (accrual duration (Ta), study duration (Ts), sample size (N))
#'
#' @references Joo L. et al. ``Data Maturity and its Implications in Confirmatory Trials with Time-to-Event Endpoints." (2018)


get_NTset <- function(ra, msurv0, msurv1, alpha=0.025, power=0.9, p1=0.5, d=2, ta.d=0.5){
  lambda1 = log(2)/msurv1
  lambda0 = log(2)/msurv0

  HR <- lambda1/lambda0
  p1 = p1
  event = Ea(HR, alpha, power, p1)
  seq.sa0 <- c(seq(round(event/ra), round(event/ra)*10, ta.d))
  T.seq <-c()
  N.seq <-c()

  oldw <- getOption("warn")
  options(warn = -1)

  for (i in 1:length(seq.sa0)){

    Ts <- tryCatch({get_T(Ta = seq.sa0[i], ra = ra, msurv1 = msurv1, msurv0 = msurv0 , alpha = alpha, power = power, p1 = p1, d=d)},
                   error=function(e){NA})

    if(is.na(Ts) == TRUE){
      N <- NA
    } else {
      N <- tryCatch({get_N(Ts =Ts, ra = ra,  msurv1 = msurv1, msurv0 = msurv0)}, error=function(e){NA})
      }

    if(is.na(N)==FALSE & N == 10000){
      break
    }
      N.seq <-c(N.seq, N)
      T.seq <-c(T.seq, Ts)

  }

  tab <- cbind(seq.sa0[seq(1, length(N.seq))], T.seq, N.seq)
  tab <- tab[is.na(T.seq)==FALSE,]
  colnames(tab) <- c("Ta", "Ts", "N")
  return(tab)
  options(warn = oldw)
}
