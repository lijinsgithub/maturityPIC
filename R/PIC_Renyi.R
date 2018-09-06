#' PIC_Renyi: R function to compute PIC by MC with Renyi's distribution
#' @export
#' @usage PIC_Renyi(n.iter = 10000, msurv0=10, msurv1=20, nevents=88, Ta=16.6, N=166, p1=0.5)
#' @param n.iter: # of iterations, a positive integer value in (0, 100000)
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

PIC_Renyi<-function(n.iter, msurv0=10, msurv1=20, nevents, Ta, N, p1=0.5){

  one.arm = round(N*p1)
  N1 = one.arm
  N0 = N - N1

  lambda0 <- log(2)/msurv0
  lambda1 <- log(2)/msurv1

  each<-function(n, i){1/(n-i+1)}
  each2<-function(n, i){1/(n-i+1)^2}

  all<-c(1/lambda0 *cumsum(each(N0, seq(1, N0))),1/lambda1 *cumsum(each(N1, seq(1, N1))))
  ordered<-sort(all)
  eos <-ordered[nevents]
  g1 <- all[-seq(1,N0)]
  eos1 <- max(g1[g1 <= eos])

  all2<-c(1/lambda0^2 *cumsum(each2(N0, seq(1, N0))),1/lambda1^2 *cumsum(each2(N1, seq(1, N1))))
  ordered2<-all2[order(all)]


  e <-  Ta/2
  e2 <- Ta^2/12

  entering <- rnorm(n.iter, e, sqrt(e2))
  event   <- rnorm(n.iter, eos, sqrt(ordered2[nevents]))
  tilde.t <-  event + entering

  m1 <- log(2)/lambda1

  return(c(T.m=mean(tilde.t), T.sd =sd(tilde.t), Pic= mean(tilde.t < m1)))

}
