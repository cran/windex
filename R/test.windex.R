test.windex <-
function(dat, tree, traits, focal=dat[,2],SE = TRUE, reps,plot=TRUE, fossil=FALSE, ...){
w<-windex(dat, tree, traits, focal,SE=SE,fossil=fossil) #calculate Wheatsheaf index
w.index<-w$w
l.ci<-w$low95
u.ci<-w$up95

t.vec<-c() #set up empty vector for bootstrapped samples
 pb <- txtProgressBar(min = 0, max = reps, style = 3)
for(i in 1:reps){
info <- sprintf("%d%% done", round((i/reps)*100))
    setTxtProgressBar(pb,i)  
X<-dat[,-1]
new<- dat[sample(nrow(X),replace=TRUE),-1] #randomly permutate focal and traits
newdat<-cbind(species=dat$species,new)

w.rep<-windex(newdat, tree, traits, focal,fossil=fossil) #recalculate Wheatsheaf index for new data
t.vec[i]<-w.rep$w

}
p.val<-length(which(t.vec>=w.index))/reps #calculate p value

if (plot==TRUE) {
 span<-max(c(t.vec,u.ci))-min(c(t.vec,l.ci))
leeway<-0.1*span

defaultArgs <- list(xlab="Wheatsheaf Index",main="",xlim=c(min(c(t.vec,l.ci))-leeway,max(c(t.vec,u.ci))+leeway))
extraArgs <- list(...)
defaultArgs[names(extraArgs)] <- extraArgs
do.call(hist, c(list(x=t.vec), defaultArgs))

abline(v=w.index)
abline(v=l.ci,lty=3)
abline(v=u.ci,lty=3)
legend('top',horiz=TRUE,inset=c(0,-0.1),xpd=TRUE,bty = "n",c('Observed Wheatsheaf index','CI'),lty=c(1,3))

}
close(pb)
return(list("w"=w.index,"low95"=l.ci,"up95"=u.ci,"P"=p.val,"boot.dist"=t.vec))
}
