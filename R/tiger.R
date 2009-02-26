tiger <- function(modelled, measured, window.size,   
maxc = 15,
synthetic.errors=NA
)
{
    answer <- list()
    answer$has.peaks <- FALSE # First calculate SOM without peaks
    answer$window.size <- window.size
    answer$modelled <- modelled
    answer$measured <- measured

    #Performance for time series
    diag.calib <- diagnostic_series(modelled=modelled,
    measured=measured, duration=window.size,use_qualV=TRUE )



    #Remove unnecessary measures
    diag.calib <- diag.calib[,-which(names(diag.calib) %in% c("AIC","BIC","span"))]  #AIC BIC weg





    #relative diff nicht Inf (symmetrisches Verhalten von 0-Werten in Steigung)
    diag.calib$rel_diff[is.nan(diag.calib$rel_diff)] <- 0 
    diag.calib$krel[is.nan(diag.calib$krel)] <- 0 
    diag.calib$rel_diff[is.infinite(diag.calib$rel_diff)] <- 0 
    diag.calib$krel[is.infinite(diag.calib$krel)] <- 0 

    answer$measures <- diag.calib

    na.rows <- is.na(rowSums(diag.calib))
    answer$na.rows <- na.rows

    theNames <- names(diag.calib)
    theNames[names(diag.calib)=="lagtime"] <- expression(t[L])
    theNames[names(diag.calib)=="rel_diff"] <- expression(r[d])
    theNames[names(diag.calib)=="krel"] <- expression(r[k])
    theNames[names(diag.calib)=="lcs_slope"] <- "LCS"

    answer$names <- theNames

    diag.calib.rer <- data.frame(sapply(diag.calib, FUN=to.uniform))
    answer$measures.uniform <- diag.calib.rer
    

    #Bestimmen der Anzahl Cluster
    answer$maxc <- maxc
    values <- answer$measures.uniform[!answer$na.rows,]

    require(e1071)
    validity <- rep(NA, maxc)
    all.cluster <- list()
    for(centers in 2:maxc){
        cluster<-cmeans(x=values, centers=centers, method="cmeans", m=2)
        #print(cluster)
        validity[centers] <- validityIndex(cluster , values)
        all.cluster[[centers]] <- cluster
    }
    
    answer$validityMeasure <- validity
    answer$cluster.assignment <- all.cluster


    #where are the no-error-values of the measures with values to both
    #sides of this no-error-values
    t.diff <-  answer$measured- answer$modelled
    quant.best<- rank(t.diff)[which.min(abs(t.diff))]/length(t.diff)
    centers <- rep(NA, NCOL(diag.calib))
    centers[names(diag.calib)=="PDIFF"] <- 0
    centers[names(diag.calib)=="PEP"] <- 0
    centers[names(diag.calib)=="ME"] <- 0
    centers[names(diag.calib)=="t_test"] <- 0
    centers[names(diag.calib)=="I"] <- 1
    centers[names(diag.calib)=="lagtime"] <- 0
    centers[names(diag.calib)=="rel_diff"] <- 1
    centers[names(diag.calib)=="krel"] <- 1
    centers[names(diag.calib)=="EQ"] <- quant.best

    # no-error-value in uniform distribution
    centers.rer <- centers
    for(i in 1:length(centers)){
       centers.rer[i]<- to.uniform(c(diag.calib[,i],centers[i]))[length(diag.calib[,i])+1] 
    }


    # no-error-value in uniform distribution for "one-sided" measures
    centers.box <- centers
    centers.box.rer <- centers.rer
    centers.box.rer[names(diag.calib) %in% c("CE", "PI",
    "Rsqr", "IoAd", "lcs_slope")] <- 1
    centers.box.rer[names(diag.calib) %in% c("AME", "MAE", "MRE", "RVE", "RAE", "NSC", "MARE", "MSLE", "MSDE", "DE", "CMSE", "MAOE",
       "MALE", "CMAE", "SMAE", "SMALE",
      "SMSLE", "MAPE", "RSMSE",  "RMSLE", "RSMSLE", "RMSOE",
      "RMSE", "RCMSE", "MSE", "MSOE", "MSRE", "MdAPE", "R4MS4E",
      "SMSE", "IRMSE",  "MAGE", "SMAGE","RMSGE","RSMSGE", "GRI")] <- 0
    centers.box[names(diag.calib) %in% c("CE", "PI",
    "Rsqr", "IoAd",  "MAGE", "SMAGE","RMSGE","RSMSGE", "lcs_slope", "GRI")] <- 1
    centers.box[names(diag.calib) %in% c("AME", "MAE", "MRE", "RVE", "RAE", "NSC", "MARE", "MSLE", "MSDE", "DE", "CMSE", "MAOE",
       "MALE", "CMAE", "SMAE", "SMALE",
      "SMSLE", "MAPE", "RSMSE",  "RMSLE", "RSMSLE", "RMSOE",
      "RMSE", "RCMSE", "MSE", "MSOE", "MSRE", "MdAPE", "R4MS4E",
      "SMSE", "IRMSE")] <- 0

    

    answer$best.value.location <- data.frame(names = names(diag.calib), all.values = centers.box, all.values.reranged = centers.box.rer, central.best.reranged = centers.rer, central.best= centers)

    answer$has.peaks <-
            !(length(synthetic.errors)==1 
                  && is.na(synthetic.errors))
    
    if(answer$has.peaks)
          answer <- tiger.peaks(answer, synthetic.errors)
       
    return(answer)

}
