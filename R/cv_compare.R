#' Some tests of different cv techniques...
#' @examples
#' x<-rnorm(1000,10,2)
#' y<-x+rnorm(1000,0,2)
#' holdout(x,y,0.75,1000)
#' loo(x,y)

#'Holdout
#'@export
holdout<-function(x,y,perc,R=1){
  xdf<-data.frame(x,y)
  rmse<-vector("numeric",R)
  for(i in 1:R){
    idx<-sample(1:nrow(xdf),nrow(xdf)*perc)
    train<-xdf[idx,]
    test<-xdf[-idx,]
    xlm<-lm(y~x,data=train)
    test_prd<-predict(xlm,test)
    rmse[i]<-sqrt(mean((test_prd-test$y)^2))
  }
  return(summary(rmse))
}

#'Leave one out
#'@export
loo<-function(x,y){
  xdf<-data.frame(x,y)
  preds<-vector("numeric",nrow(xdf))
  for(i in 1:nrow(xdf)){
    xlm<-lm(y~x,data=xdf[-i,])
    preds[i]<-predict(xlm,xdf[i,])
  }
  rmse<-sqrt(mean((preds-xdf$y)^2))
  return(rmse)
}