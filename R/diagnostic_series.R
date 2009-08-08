`diagnostic_series` <-
function(
measured,
modelled,
window.size,
step.size=1,
integral_correction=FALSE, use_qualV=FALSE
){
t.pos<- seq(1, (NROW(modelled)-window.size), by=step.size)
dim(t.pos)<-c(length(t.pos),1)

t.diff<-measured-modelled
diff.ecdf <- ecdf(t.diff)

res <- lapply(t.pos, FUN=diagnostic_window, window.size=window.size, measured=measured, modelled=modelled,use_qualV=use_qualV, diff.ecdf=diff.ecdf)
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

toAppend<-data.frame(matrix(nrow=floor(window.size/step.size),ncol=NCOL(r.diag)) )
names(toAppend) <- names(r.diag)
r.diag<-rbind(toAppend,r.diag)

return(r.diag)

}

