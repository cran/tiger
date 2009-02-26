`diagnostic` <-
function(modelled,measured, use_qualV=FALSE){
   if(is.null(dim(modelled))){
        dim(modelled) <- c(1,length(modelled))
        the_nrow <- 1
   } else {
        the_nrow <- NROW(modelled)
   }
   lcs_slope <- function(o,p){
       time <- 1:length(o)
       o_fl <- f.slope(time,o)
       p_fl <- f.slope(time,p)
       return(LCS(o_fl,p_fl)$QSI)
   }
   size=10
   if(use_qualV){
        require(qualV)
        size=33
   }
   result<-array(dim=c(the_nrow,size), NA)
   if(sum(is.na(modelled))>0 |sum(is.na(measured)) ){
       warning("Detailed handling of NA not implemented. Returning NA")
   } else {
       result[,1]<-apply(modelled,MARGIN=1, FUN=cor, y=measured)
       result[,2]<-apply(modelled,MARGIN=1, FUN=nashS, measured=measured)
       result[,3]<-apply(modelled,MARGIN=1, FUN=sum)/sum(measured)
       result[,4]<-apply(modelled,MARGIN=1, FUN=lagtime, y=measured)
       result[,5]<-apply(modelled,MARGIN=1, FUN=max_rel_diff, y=measured)
       result[,6]<-apply(modelled,MARGIN=1, FUN=diff_diff, y=measured)
       result[,7]<-apply(modelled,MARGIN=1, FUN=k_rel, y=measured)
       result[,8]<-apply(modelled,MARGIN=1, FUN=rmse, b=measured)
       result[,9]<-apply(modelled,MARGIN=1, FUN=span)
       result[,10]<-rep(0.5,the_nrow)
       if(use_qualV){
           result[,11]<-apply(modelled,MARGIN=1, FUN=CMAE, p=measured)
           result[,12]<-apply(modelled,MARGIN=1, FUN=CMSE, p=measured)
           result[,13]<-apply(modelled,MARGIN=1, FUN=MAE, p=measured)
           result[,14]<-apply(modelled,MARGIN=1, FUN=MAGE, p=measured)
           result[,15]<-apply(modelled,MARGIN=1, FUN=MALE, p=measured)
           result[,16]<-apply(modelled,MARGIN=1, FUN=MAOE, p=measured)
           result[,17]<-apply(modelled,MARGIN=1, FUN=MAPE, p=measured)
           result[,18]<-apply(modelled,MARGIN=1, FUN=MSE, p=measured)
           result[,19]<-apply(modelled,MARGIN=1, FUN=MSLE, p=measured)
           result[,20]<-apply(modelled,MARGIN=1, FUN=MSOE, p=measured)
           result[,21]<-apply(modelled,MARGIN=1, FUN=RCMSE, p=measured)
           result[,22]<-apply(modelled,MARGIN=1, FUN=lcs_slope, p=measured)
           result[,23]<-apply(modelled,MARGIN=1, FUN=RMSGE, p=measured)
           result[,24]<-apply(modelled,MARGIN=1, FUN=RMSLE, p=measured)
           result[,25]<-apply(modelled,MARGIN=1, FUN=RMSOE, p=measured)
           result[,26]<-apply(modelled,MARGIN=1, FUN=RSMSE, p=measured)
           result[,27]<-apply(modelled,MARGIN=1, FUN=RSMSGE, p=measured)
           result[,28]<-apply(modelled,MARGIN=1, FUN=RSMSLE, p=measured)
           result[,29]<-apply(modelled,MARGIN=1, FUN=SMAE, p=measured)
           result[,30]<-apply(modelled,MARGIN=1, FUN=SMAGE, p=measured)
           result[,31]<-apply(modelled,MARGIN=1, FUN=SMALE, p=measured)
           result[,32]<-apply(modelled,MARGIN=1, FUN=SMSE, p=measured)
           result[,33]<-apply(modelled,MARGIN=1, FUN=SMSLE, p=measured)
       }
   }
   result<-data.frame(result)
   if(use_qualV){
        names(result)<-c("cor","NSE","Integral", "lagtime","rel_diff", "sum_diff",
        "rel.k", "rmse", "span", "err.quant", "CMAE", "CMSE", "MAE", "MAGE", "MALE",
        "MAOE", "MAPE", "MSE", "MSLE", "MSOE", "RCMSE", "lcs_slope",
        "RMSGE", "RMSLE", "RMSOE", "RSMSE", "RSMSGE", "RSMSLE",
        "SMAE", "SMAGE", "SMALE", "SMSE", "SMSLE" )
   } else {
        names(result)<-c("cor","NSE","Integral",
        "lagtime","rel_diff","sum_diff", "rel.k", "rmse", "span",
        "err.quant")
   }
   return(result)
}

