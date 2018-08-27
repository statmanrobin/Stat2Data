#' Empirical logit plot for one quantitative variable
#'
#' @param formula A formula of the form (binary) Response~Predictor
#' @param data A dataframe
#' @param ngroups Number of groups to use (not needed if breaks is used), ngroups="all" uses all unique values
#' @param breaks A vector of endpoints for the bins (not needed if ngroups is used)
#' @param yes Set a value for the response to be counted for proportions (optional)
#' @param padj Should proportions be adjusted to avoid zero and one? (default is TRUE)
#' @param out Should the function return a dataframe with group information? (default is FALSE)
#' @param showplot Show the plot? default is TRUE
#' @param showline Show the regression line? default is TRUE
#' @param dotcol Color for the dots (default is "black")
#' @param linecol Color for the line (default is "blue")
#'
#' @return A dataframe with group information (if out=TRUE)
#' @description
#' This function produces an empirical logit plot for a binary response variable and a
#' single quantitative predictor variable.
#' @details
#'
#' @examples
#' data(MedGPA)
#' emplogitplot1(Acceptance~GPA,data=MedGPA)
#'
#' GroupTable=emplogitplot1(Acceptance~MCAT,ngroups=5,out=TRUE,data=MedGPA)
#'
#' emplogitplot1(Acceptance~MCAT,data=MedGPA,breaks=c(0,34.5,39.5,50.5),dotcol="red",linecol="black")
#'
#' data(Putts1)
#' emplogitplot1(Made~Length,data=Putts1,ngroups="all")

emplogitplot1=function(formula,data=NULL,ngroups=3,breaks=NULL,
                       yes=NULL,padj=TRUE,out=FALSE,showplot=TRUE,showline=TRUE,
                       ylab="Log(Odds)",xlab=NULL,
                       dotcol="black",linecol="blue",pch=16,main="",
                       ylim=NULL,xlim=NULL,lty=1,lwd=1,cex=1){
  mod=glm(formula,family=binomial,data)
  newdata=mod$model[,1:2]
  oldnames=names(newdata)
  if(is.null(xlab)){xlab=oldnames[2]}   #Need a label for x-axis
  names(newdata)=c("Y","X")
  newdata=na.omit(newdata)      #get rid of NA cases for either variable
  #if needed find the value for "success"
  newdata$Y=factor(newdata$Y)
  if(is.null(yes)){yes=levels(newdata$Y)[2]}
  if(ngroups=="all"){breaks=unique(sort(c(newdata$X,min(newdata$X)-1)))}
  if(is.null(breaks)){
    breaks= quantile(newdata$X, probs = (0:ngroups)/ngroups)
    breaks[1] <- breaks[1]-1
  }
  ngroups=length(breaks)-1
  newdata$XGroups=cut(newdata$X,breaks=breaks,labels=1:ngroups)
  Cases=as.numeric(table(newdata$XGroups))
  XMean=as.numeric(aggregate(X~XGroups,data=newdata,mean)$X)
  XMin=as.numeric(aggregate(X~XGroups,data=newdata,min)$X)
  XMax=as.numeric(aggregate(X~XGroups,data=newdata,max)$X)
  NumYes=as.numeric(table(newdata$Y,newdata$XGroups)[yes,])
  Prop=round(NumYes/Cases,3)
  AdjProp=round((NumYes+0.5)/(Cases+1),3)
  Logit=as.numeric(log(AdjProp/(1-AdjProp)))
  if(!padj){Logit=as.numeric(log(Prop/(1-Prop)))}
  if(showplot){plot(Logit~XMean,ylab=ylab,col=dotcol,pch=pch,
       ylim=ylim,xlim=xlim,xlab=xlab,cex=cex,main=main)
  if(showline){abline(lm(Logit~XMean),col=linecol,lty=lty,lwd=lwd)}}
  GroupData=data.frame(Group=1:ngroups,Cases,XMin,XMax,XMean,NumYes,Prop,AdjProp,Logit)
  if(out){return(GroupData)}
}
