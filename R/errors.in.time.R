errors.in.time <- function(xval, result, solution, show.months=FALSE,
new.order=1:solution, x.range=1:length(xval),
pmax=max(c(result$measured, result$modelled), na.rm=TRUE),
data.colors=data.frame(measured=c("grey"), modelled=c("black"), rain=c("black")),
clusterPalette= rainbow(solution),
color.cut.off=0, frac.max=0.7, frac.min=0.4,
legend.pos="topleft", ...
 ){


    pmax.orig <- pmax
    pmin<-frac.min*pmax
    pmax<-frac.max*pmax
    pdist <- (pmax-pmin)/solution
    old.pal <- palette(clusterPalette)

    plot(range(xval[x.range]), range(0,pmax.orig), ylab="discharge/mm/h", type="n", ...)

    if(show.months){
        min.month <- strftime(min(as.POSIXlt(xval[x.range])), "%m.%y")
        my.min <- strptime(paste(1,min.month,sep="."), "%d.%m.%y")
        max.month <- strftime(max(as.POSIXlt(xval[x.range])), "%m.%y")
        my.max <- strptime(paste(1,max.month,sep="."), "%d.%m.%y")
        month.ticks <-seq(my.min,my.max,by="month") 
        month.lab <- strftime(as.POSIXlt(month.ticks), "%b")
        month.lab[strftime(as.POSIXlt(month.ticks), "%m")=="01"] <- NA
        axis(1, at=month.ticks, label=month.lab, cex.axis=0.4, tcl=-0.15, mgp=c(3,0.2,0))
    }

    
    if(result$multi.model){
        for(i in 1:result$count.model){
            reduced.lines(xval[x.range], result$model[i,x.range], type="l", lwd=2, col=as.character(data.colors$modelled))
        }
    } else {
        reduced.lines(xval[x.range], result$model[x.range], type="l", lwd=2, col=as.character(data.colors$modelled))
    }
    reduced.lines(xval[x.range], result$measured[x.range], type="l", lwd=1.5, col=as.character(data.colors$measured))

    region.proportion <- cluster.proportion(result=result, solution=solution, new.order=new.order)
    #redefine x.range according to step size
    x.range <- seq(min(x.range), max(x.range), by=result$step.size)
    stopifnot(length(x.range)==dim(region.proportion)[2])
    min.diff <- min(diff(xval[x.range]))
    try(units(min.diff) <- "secs", silent=TRUE)
    min.diff <- as.numeric(min.diff)
    myFrom <- xval - min.diff/2
    myTo <- xval + min.diff/2

    for(the.region in 1:solution){
       currmin <- pmax-(the.region-1)*pdist
       currmax <- pmax-the.region*pdist
       cols <- color.factor(color=palette()[the.region],
                            value= region.proportion[the.region,],
                            max=1)
       cols[region.proportion[the.region,]<=color.cut.off] <- NA
       rect(myFrom[x.range],currmin, myTo[x.range], currmax, col=cols, border="transparent")

       #lines(xval[sel][subsel], result$measured[sel][subsel], type="l", col=the.region)
    }
    legend(legend.pos, inset=0.05, lty=c(1,1), lwd=c(2,1.5),col=c("black","grey"),  legend=c("simulated", "observed"))
    axis(side=4, at= seq((pmax-pdist/2), pmin+pdist/2, length.out = solution), labels=LETTERS[1:solution], mgp=c(3,0.7,0), las=2, cex.axis=0.6 )

     palette( old.pal )

}
