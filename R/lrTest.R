lrTest<-function(small,big,df){
likrat<--2*(small-big)
p<-1-pchisq(likrat,df)
return(list(LR=likrat,P=p))
}