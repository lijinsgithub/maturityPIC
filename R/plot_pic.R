#' plot_pic: R function to draw the profile of PIC over designs
#' @export
#' @usage plot_pic(design, Pic=pic1, n.plot.min=FALSE, n.plot.max=FALSE, t.plot.min=FALSE, t.plot.max=FALSE,   col.d=2, col.p="magenta")
#' @param design: a tuple of three design parameters (Ta, Ts, N)
#' @param Pic: a vector of PIC under various desings of consideration
#' @param col.d : color used for design curve
#' @param col.p : color used for PIC curve
#' @param n.plot.min (optional) : adjusting the beginning of N grid (x-axis) in the plot
#' @param n.plot.max (optional) : adjusting the ending of N grid (x-axis) in the plot
#' @param n.plot.min (optional) : adjusting the beginning of Ts grid (y-axis) in the plot
#' @param t.plot.max (optional) : adjusting the ending of Ts grid (y-axis) in the plot
#'
#' @return a plot of two curves: one for (N, Ts) and the other for (N, PIC)
#'
#' @references Joo L. et al. ``Data Maturity and its Implications in Confirmatory Trials with Time-to-Event Endpoints." (2018)


plot_pic <- function(design, Pic, n.plot.min=FALSE, n.plot.max=FALSE, t.plot.min=FALSE, t.plot.max=FALSE,   col.d=2, col.p="magenta"){


    plotN1 = design[,3]

    plotT1 = design[,2]

    if(n.plot.min==FALSE){
      n.plot.min = min(plotN1)-5
      }

    if(n.plot.max==FALSE){
      n.plot.max = max(plotN1) + 5
    }

    if(t.plot.min==FALSE){
      t.plot.min = min(plotT1)-5}

    if(t.plot.max==FALSE){
      t.plot.max = max(plotT1) + 5
    }

    curve1<-smooth.spline(plotN1, plotT1)
    par(mar=c(4, 4, 2, 4))
    plot(curve1, xlim=c(n.plot.min, n.plot.max), ylim=c(t.plot.min, t.plot.max), ty="l", xlab="N", ylab="Ts", col=col.d, lwd=3, xaxs="i", yaxs="i")

      par(new=T)

      Pic <- ifelse(Pic == "NaN", NA, Pic)
      imm1<-smooth.spline( plotN1, Pic)
      plot(imm1, type="l",  lwd=2, axes=F, xlab=NA, ylab=NA,xlim=c(n.plot.min, n.plot.max), ylim=c(0, 1),  col=col.p, xaxs="i", yaxs="i")


      axis(side = 4)
      mtext(side = 4, line = 3, "PIC")

    }


