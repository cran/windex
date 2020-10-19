modSel.rayDISC<-function(...,type="AICc"){
mods<-list(...)
getnpars<-function(obj){max(na.omit(as.numeric(obj$index.mat)))}
liks<-function(obj){obj$loglik}

if (type=="AIC"){
aics<-function(obj){obj$AIC}
}

else if (type=="AICc"){
aics<-function(obj){obj$AICc}
}

k<-sapply(mods,getnpars)
lik<-sapply(mods,liks)
aic<-sapply(mods,aics)
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

if (type=="AIC"){
names(sel)<-c("K","logLik","AIC","deltaAIC","Weight","Evidence ratio")
}

else if (type=="AICc"){
names(sel)<-c("K","logLik","AICc","deltaAICc","Weight","Evidence ratio")
}

sel<-sel[order(delta),]
sel
}