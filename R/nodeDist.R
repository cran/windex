nodeDist<-function(trees,sp1,sp2,relTime=F,fillcol="blue",xlabel="Age (mya)",main="",return.ages=F,plot=T,add=F,...){
ages<-c()
for (i in 1:length(trees)){
time.from.root<-findMRCA(trees[[i]],tips=c(sp1,sp2),type="height")
age.of.root<-max(nodeHeights(trees[[i]]))
if (relTime==F) ages[i]<-age.of.root-time.from.root
else ages[i]<-(age.of.root-time.from.root)/age.of.root
}
if (plot==T) {
agedist<-density(ages)
if (add==F) plot(agedist,xlab=xlabel,main=main,...)
else {
lines(agedist)
print("As you are adding a distribution to an existing plot, make sure xlim and ylim on original plot are set to accomodate all plotted distributions")
}
polygon(agedist,col=fillcol)
}
if (return.ages==T) ages
}