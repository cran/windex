modSel.fitMk<-function(...,tree=NULL,type="AICc"){
mods<-list(...)
getnpars<-function(obj){attributes(logLik(obj))$df}
k<-sapply(mods,getnpars)
lik<-sapply(mods,logLik)

if (type=="AICc"){
if (is(tree,"phylo")) n<-Ntip(tree) else if (is(tree,"numeric")) n<-tree else stop("tree must be either an object of class phylo or a numeric value giving the number of species in the tree used for fitting the models in fitMk")
aic<-sapply(mods,AIC)
aicc<-aic+(2*k*(k+1)/(n-k-1))
delta<-aicc-min(aicc)
w<-c()
for (i in 1:length(delta)){
w[i]<-(exp(-0.5*delta[i]))/sum(exp(-0.5*delta[1:length(delta)]))
}
er<-max(w)/w
w<-round(w,digits=4)
sel<-data.frame(k,lik,aicc,delta,w,er)
dots<-substitute(list(...))[-1]
rownames(sel)<-sapply(dots,deparse)
names(sel)<-c("K","logLik","AICc","deltaAICc","Weight","Evidence ratio")
}

else if (type=="AIC"){
aic<-sapply(mods,AIC)
delta<-aic-min(aic)
w<-c()
for (i in 1:length(delta)){
w[i]<-(exp(-0.5*delta[i]))/sum(exp(-0.5*delta[1:length(delta)]))
}
er<-max(w)/w
w<-round(w,digits=4)
sel<-data.frame(k,lik,aic,delta,w,er)
dots<-substitute(list(...))[-1]
rownames(sel)<-sapply(dots,deparse)
names(sel)<-c("K","logLik","AIC","deltaAIC","Weight","Evidence ratio")
}

sel<-sel[order(delta),]
sel
}