LRbarplot<-function(mod,parIndex=2,yName="response",xLevels=paste(c(1:length(c(1,parIndex)))),main="",...){
int<-summary(mod)$coefficients[1,1]
est<-summary(mod)$coefficients[parIndex,1]
intse<-summary(mod)$coefficients[1,2]
estse<-summary(mod)$coefficients[parIndex,2]

bp<-barplot(c(backLog(int),backLog(int+est)),ylim=c(0,1),ylab=paste("Probability of",yName),names.arg=xLevels,...)
arrows(bp, c(backLog(int+intse),backLog(int+est+estse)), bp, c(backLog(int-intse),backLog(int+est-estse)), angle = 90,code=3)
}