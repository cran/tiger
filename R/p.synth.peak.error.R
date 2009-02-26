p.synth.peak.error <- function(peaks, y.max=(max(peaks,na.rm=TRUE))){
    n.errors=dim(peaks)[2]
    n.levels=dim(peaks)[3]
    palette(grey(c(0,0.6,0.8)))
    nf <- layout(matrix(1:10,2,5,byrow=TRUE), c(1.5, 1,1,1,1), c(1,1.55), TRUE)
    #layout.show(nf)
    par(oma=c(1,1,0,0)+0.1)
    for(error in 1:n.errors){
       if(error > 5) {
           b.mar <-4
           x.axt = "s"
       } else {
           b.mar <-0
           x.axt = "n"
       }
       if(error%%5==1){
           l.mar <-4
           y.axt = "s"
       } else {
           l.mar <-0
           y.axt = "n"
       }
       par(mar=c(b.mar,l.mar,0,0)+0.1 )
       plot(peaks[2,error,1,],type="n", xlab="",ylab="", xaxt=x.axt, yaxt=y.axt, ylim=c(0,y.max))
       text(x=150, y=0.8*y.max, error, cex=2)
       for(level in 1:n.levels){
           lines(peaks[1,error,level,], col=((level-1)%/%3)+2)
           lines(peaks[2,error,level,])
       }
    }
    mtext(outer=TRUE, side=2, line=-1, text="specific discharge/mm/h")
    mtext(outer=TRUE, side=1, line=-1, text="time/h")
}

