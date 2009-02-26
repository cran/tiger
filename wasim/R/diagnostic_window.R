`diagnostic_window` <-
function(position, duration,measured,modelled, use_qualV=FALSE, diff.ecdf = NA ){
    if(position %% 100==0){
           cat("At position", position, "\n")
           gc()
    }
         wind<-position:(position+duration-1)
         w.dat<-measured[wind]
         #dim(w.dat)<-c(1,duration)
         toReturn<-diagnostic_dawson(measured=w.dat, modelled=modelled[wind], use_qualV=use_qualV, diff.ecdf=diff.ecdf)
         return(toReturn)

}

