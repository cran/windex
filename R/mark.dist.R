mark.dist<-function(marks,plot=TRUE,col="light grey",main=NULL,xlab="Marks",xlim=c(0,100),showBounds=FALSE,y=20,digits=2,...){
if (plot==TRUE) {hist(marks,xlab=xlab,col=col,main=main,xlim=xlim,...)}
if (showBounds==TRUE) {
abline(v=c(40,50,60,70),lwd=2,lty=2)
text(x=c(20,45,55,65,85),y=y,labels=c("Fail","3rd","2.2","2.1","1st"),font=2)
}
first<-(table(marks>=70)["TRUE"])/length(marks)
twoone<-(table(marks>=60&marks<70)["TRUE"])/length(marks)
twotwo<-(table(marks>=50&marks<60)["TRUE"])/length(marks)
third<-(table(marks>=40&marks<50)["TRUE"])/length(marks)
fail<-(table(marks<40)["TRUE"])/length(marks)
mk<-data.frame(c(table(marks>=70)["TRUE"],first),c(table(marks>=60&marks<70)["TRUE"],twoone),c(table(marks>=50&marks<60)["TRUE"],twotwo),c(table(marks>=40&marks<50)["TRUE"],third),c(table(marks<40)["TRUE"],fail))
names(mk)<-c("1st","2.1","2.2","3rd","Fail")
rownames(mk)<-c("Number","Proportion")
return(summary=list(Summary=summary(marks),NormalityTest=shapiro.test(marks),GradeBreakdown=round(mk,digits=digits)))
}