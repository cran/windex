nodeDist<-function(trees,sp1,sp2,relTime=F,fillcol="blue",main="",...){
ages<-c()
for (i in 1:length(trees)){
time.from.root<-findMRCA(trees[[i]],tips=c(sp1,sp2),type="height")
age.of.root<-max(nodeHeights(trees[[i]]))
if (relTime==F) ages[i]<-age.of.root-time.from.root
else ages[i]<-(age.of.root-time.from.root)/age.of.root
}
agedist<-density(ages)
plot(agedist,xlab="Age (mya)",main=main,...)
polygon(agedist,col=fillcol)
}