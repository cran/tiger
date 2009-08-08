peaks.measures <- function(result, show.measures=1:num.measures,
synthetic.peaks.col = c(2:8,2:8),mfrow=c(2,3),col.best.match="black", do.out=rep(TRUE, length(show.measures)) ){
    n.level <- dim(result$measures.synthetic.peaks)[2]
    n.errors <- dim(result$measures.synthetic.peaks)[1]
    num.measures <- length(result$measures)

    my.min <- function(x, na.rm, do.out=TRUE){
         return(min(x[!x %in% boxplot.stats(x, do.out=do.out)$out], na.rm=na.rm))
    }
    my.max <- function(x, na.rm, do.out=TRUE){
         return(max(x[!x %in% boxplot.stats(x, do.out=do.out)$out], na.rm=na.rm))
    }
    palette("default")

par(mfrow=mfrow,mar=c(2,2,3,1)+0.1, oma=c(0,0,0,0)+0.1)
for(measure in show.measures){
        data <- result$measures.synthetic.peaks[,,measure]
        lab <- result$names[[measure]]
        lab <- substitute(" "*a, list(a=lab))
        best <- result$best.value.location$all.values[measure]
        plot(1:n.level, seq(my.min(c(data,best), na.rm=TRUE, do.out=do.out[which(show.measures %in% measure)]), my.max(c(data,best), na.rm=TRUE, do.out=do.out[which(show.measures %in% measure)]), length.out=n.level), type="n", main=lab, xlab="", ylab=lab)
        lines(c(0,n.level+1),c(best, best) , lwd=2, col=col.best.match)
        for(error in 1:n.errors){
            points(data[error,], pch=error, col=synthetic.peaks.col[error])
        }
}
        plot(c(0,1),c(0,1), type="n", xaxt="n", yaxt="n", ylab="", bty="n")
        legend("top",result$error.names, pch=1:n.errors, col=synthetic.peaks.col, cex=0.8)
}
