`diagnostic_series` <-
function(
measured,
modelled,
duration,
integral_correction=FALSE, use_qualV=FALSE
){
t.pos<-1:(NROW(modelled)-duration)
dim(t.pos)<-c((NROW(modelled)-duration),1)

t.diff<-measured-modelled
diff.ecdf <- ecdf(t.diff)

res <- lapply(t.pos, FUN=diagnostic_window, duration=duration, measured=measured, modelled=modelled,use_qualV=use_qualV, diff.ecdf=diff.ecdf)
r.diag<-do.call("rbind", res)


#multiplicative correction of integrated flow
#time series need the same number of NA
if(integral_correction){
    correctA<-measured
    correctA[is.na(modelled)]<-NA
    correctB<-modelled
    correctB[is.na(correctA)]<-NA

    correct.integral<-sum(correctB,na.rm=TRUE)/sum(correctA,na.rm=TRUE)
    r.diag$I<-r.diag$I*correct.integral
}

toAppend<-data.frame(matrix(nrow=duration,ncol=NCOL(r.diag)) )
names(toAppend) <- names(r.diag)
r.diag<-rbind(toAppend,r.diag)

return(r.diag)

}

