errors.in.time <- function(xval, result, solution, show.months=FALSE, new.order=1:solution){
    regions<-change.order.clusters(result$cluster.assignment[[solution]], new.order )$cluster

    xval.red <- xval[!result$na.rows]
    stopifnot(length(regions)==length(xval.red))

    pmax<-max(result$modelled, na.rm=TRUE)
    pmin<-0.4*pmax
    pmax<-0.7*pmax
    pdist <- (pmax-pmin)/solution
    palette( rainbow(solution))

    plot(xval, result$model, ylab="discharge/mm/h", type="n")

    if(show.months){
        month.ticks <-seq(min(xval),max(xval),by="month") 
        month.lab <- strftime(as.POSIXlt(month.ticks), "%b")
        month.lab[month.lab==1] <- NA
        axis(1, at=month.ticks, label=month.lab, cex.axis=0.4, tcl=-0.15, mgp=c(3,0.2,0))
    }

    lines(xval, result$model, type="l", lwd=2)
    lines(xval, result$measured, type="l", lwd=1.5, col="grey")
    for(the.region in unique(regions)){
       subsel <- regions==the.region
       subsel[ is.na(subsel)] <- FALSE
       if(sum(subsel)==0) next
       min.diff <- min(diff(xval.red))
       myTo <- c(which(diff(xval.red[subsel])>1.2*min.diff), length(xval.red[subsel]))
       myFrom <- c(1, which(diff(xval.red[subsel])>1.2*min.diff) +1)
       #i<-1
       #xval[sel][subsel][myFrom][i]; xval[sel][subsel][myTo][i]+3600
       #xval[sel][subsel]
       currmin <- pmax-(the.region-1)*pdist
       currmax <- pmax-the.region*pdist
       #tw <- xval[result$window.size+1] - xval[1]
       rect(xval.red[subsel][myFrom],currmin, xval.red[subsel][myTo], currmax, col=the.region , border="transparent")

       #lines(xval[sel][subsel], result$measured[sel][subsel], type="l", col=the.region)
    }
    legend("topleft", inset=0.05, lty=c(1,1), lwd=c(2,1.5),col=c("black","grey"),  legend=c("simulated", "observed"))
    axis(side=4, at= seq((pmax-pdist/2), pmin+pdist/2, length.out = max(regions)), labels=LETTERS[1:(max(regions))], mgp=c(3,0.7,0), las=2, cex.axis=0.6 )


}
