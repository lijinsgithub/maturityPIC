#' plot_compare: R function to draw the profiles of PIC and M1-M3 over designs
#' @export
#' @usage plot_compare(design, Pic=pic1, m1 = m1, m2 = m2, m3 = m3, n.plot.min=FALSE, n.plot.max=FALSE, t.plot.min=FALSE, t.plot.max=FALSE, col.d=2, col1=2, col2=3, col3=4, col.p="magenta")
#' @param design: a tuple of three design parameters (Ta, Ts, N)
#' @param Pic: a vector of PIC under various desings of consideration
#' @param m1: a vector of M1 statistics under various desings of consideration
#' @param m2: a vector of M2 statistics under various desings of consideration
#' @param m3: a vector of M3 statistics under various desings of consideration
#' @param col.d : color used for design curve
#' @param col.p : color used for PIC curve
#' @param n.plot.min (optional) : adjusting the beginning of N grid (x-axis) in the plot
#' @param n.plot.max (optional) : adjusting the ending of N grid (x-axis) in the plot
#' @param n.plot.min (optional) : adjusting the beginning of Ts grid (y-axis) in the plot
#' @param t.plot.max (optional) : adjusting the ending of Ts grid (y-axis) in the plot
#'
#' @return a plot with
#'
#' @references Joo L. et al. ``Data Maturity and its Implications in Confirmatory Trials with Time-to-Event Endpoints." (2018)


plot_compare <- function(design, Pic, m1, m2, m3, n.plot.min=FALSE, n.plot.max=FALSE, t.plot.min=0, t.plot.max=FALSE, col.d="gray", col1=2, col2=3, col3=4, col.p="magenta"){


  plotN1 = design[,3]

  plotT1 = design[,2]

  if(n.plot.min==FALSE){
    n.plot.min = min(plotN1)-5
  } else {
    n.plot.min = n.plot.min
  }

  if(n.plot.max==FALSE){
    n.plot.max = max(plotN1) + 5
  } else {
    n.plot.max =n.plot.max
  }

  if(t.plot.min==FALSE){
    t.plot.min = min(plotT1)-5
  } else {
    t.plot.min =  t.plot.min
    }


  if(t.plot.max==FALSE){
    t.plot.max = max(plotT1) + 5
  } else {
    t.plot.max = t.plot.max
  }

  curve1<-smooth.spline(plotN1, plotT1)
  curve.m1<-smooth.spline(plotN1, m1)
  curve.m2<-smooth.spline(plotN1, m2)
  curve.m3<-smooth.spline(plotN1, m3)


   par(mar=c(4, 4, 2, 4))
  plot(curve1, xlim=c(n.plot.min, n.plot.max), ylim=c(0, t.plot.max), ty="l", xlab="N", ylab="Time", col=col.d, lwd=3, xaxs="i", yaxs="i")
  lines(curve.m2, col=col2, lwd=3)
  lines(curve.m3, col=col3, lwd=3)
  par(new=T)

  Pic <- ifelse(Pic == "NaN", NA, Pic)
  imm1<-smooth.spline( plotN1, Pic)
  plot(imm1, type="l",  lwd=2, axes=F, xlab=NA, ylab=NA, xlim=c(n.plot.min, n.plot.max), ylim=c(0, 1),  col=col.p, xaxs="i", yaxs="i")
  lines(curve.m1, col=col1, lwd=2)

  axis(side = 4)
  mtext(side = 4, line = 3, "(%)")

}
