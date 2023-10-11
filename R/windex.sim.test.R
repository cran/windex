windex.sim.test <-
function(dat, tree, traits, focal=dat[,2],SE = TRUE, Nsims, plot=TRUE, fossil=FALSE, ...){
print("Calculating Wheatsheaf index from data")
w<-windex(dat, tree, traits, focal,SE=SE,fossil=fossil) #calculate Wheatsheaf index
w.index<-w$w
l.ci<-w$low95
u.ci<-w$up95

print("Simulating traits evolving by BM over input tree, based on parameters estimated from real data")
rownames(dat)<-dat$species
if(length(traits)==1){
traits<-dat[,traits]
names(traits)<-rownames(dat)
ratmat<-ratematrix(tree,traits)
} else ratmat<-ratematrix(tree,dat[,traits])
simtraits<-sim.char(tree,ratmat,nsim=Nsims,model="BM")

print("Calculating Wheatsheaf index for each simulated dataset")
t.vec<-c() #set up empty vector for simulation samples
 pb <- txtProgressBar(min = 0, max = Nsims, style = 3)
for(i in 1:Nsims){
info <- sprintf("%d%% done", round((i/Nsims)*100))
    setTxtProgressBar(pb,i)  
newdat<-cbind(species=dat$species,focal=focal,as.data.frame(simtraits[,,i]))

maxcols<-2+attributes(simtraits)$dim[2]
w.sim<-windex(newdat, tree, traits=3:maxcols, focal=newdat[,2],fossil=fossil) #recalculate Wheatsheaf index for new data
t.vec[i]<-w.sim$w

}
close(pb)
p.val<-length(which(t.vec>=w.index))/Nsims #calculate p value

if (plot==TRUE) {
 span<-max(c(t.vec,u.ci))-min(c(t.vec,l.ci))
leeway<-0.1*span
hist(t.vec,xlab='Wheatsheaf Index',main="",xlim=c(min(c(t.vec,l.ci))-leeway,max(c(t.vec,u.ci))+leeway),...)
abline(v=w.index)
abline(v=l.ci,lty=3)
abline(v=u.ci,lty=3)
legend('top',horiz=TRUE,inset=c(0,-0.1),xpd=TRUE,bty = "n",c('Observed Wheatsheaf index','CI'),lty=c(1,3))

}
return(list("w"=w.index,"low95"=l.ci,"up95"=u.ci,"P"=p.val,"sim.dist"=t.vec))
}
