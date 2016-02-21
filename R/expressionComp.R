## figures to compare expression estimates across methods
expressionComp <- function(object1, qcThreshold1, 
                           object2, qcThreshold2, 
                           label1=NULL, label2=NULL,
                           diffThreshold=2,
                           plotType=c("scatterplot","MAplot")){
    plotType <- match.arg(plotType)

    object1 <- checkObject(object1)
    cts1 <- object1$ct
    qc1 <- object1$qc

    object2 <- checkObject(object2)
    cts2 <- object2$ct
    qc2 <- object2$qc

    ## quality filtering    
    i.rm1 <- which(apply(qc1<qcThreshold1,1,any) | apply(is.na(cts1),1,any))
    i.rm2 <- which(apply(qc2<qcThreshold2,1,any) | apply(is.na(cts2),1,any))
    i.rm <- union(i.rm1,i.rm2)
    cts1 <- cts1[-i.rm,]
    cts2 <- cts2[-i.rm,]
    
    ## points that exceed the diff threshold
    ind <- matrix(which(abs(cts2-cts1) > diffThreshold, arr.ind=TRUE), ncol=2)
    
    if(plotType=="scatterplot"){
      smoothScatter(x=cts1, y=cts2, main="", nrpoints = 0,
           xlab=ifelse(is.null(label1), "Expression Estimate (object1)", label1),
           ylab=ifelse(is.null(label2), "Expression Estimate (object2)", label2))
      points(x=cts1[ind], y=cts2[ind], pch=20)
    }
    
    if(plotType=="MAplot"){
      smoothScatter(x=(cts1+cts2)/2, y=cts2-cts1, main="", nrpoints = 0,
           ylab=ifelse(is.null(label2), "Difference in Expression Estimates", label2),
           xlab=ifelse(is.null(label1), "Average Expression Estimate", label1))
      points(x=(cts1[ind]+cts2[ind])/2, y=cts2[ind]-cts1[ind], pch=20)
    }
    
    return(data.frame(feature=rownames(cts1)[ind[,1]],
                      sample=colnames(cts1)[ind[,2]],
                      stringsAsFactors = FALSE))
}
       

    
