mark.dist<-function(marks,plot=TRUE,col="light grey",main=NULL,xlab="Marks",xlim=c(0,100),...){
if (plot==TRUE) {hist(marks,xlab=xlab,col=col,main=main,xlim=xlim,...)}
first<-(table(marks>=70)["TRUE"])/length(marks)
twoone<-(table(marks>=60&marks<70)["TRUE"])/length(marks)
twotwo<-(table(marks>=50&marks<60)["TRUE"])/length(marks)
third<-(table(marks>=40&marks<50)["TRUE"])/length(marks)
fail<-(table(marks<40)["TRUE"])/length(marks)
mk<-data.frame(first,twoone,twotwo,third,fail)
names(mk)<-c("1st","2.1","2.2","3rd","Fail")
rownames(mk)<-"Proportion"
return(summary=list(Summary=summary(marks),NormalityTest=shapiro.test(marks),GradeBreakdown=mk))
}