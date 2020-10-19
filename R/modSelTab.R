modSelTab<-function(...,type="AIC"){
mods<-list(...)
getnpars<-function(obj){attributes(logLik(obj))$df}
k<-sapply(mods,getnpars)
lik<-sapply(mods,logLik)

if (type=="AIC"){
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

else if (type=="BIC"){
bic<-sapply(mods,BIC)
delta<-bic-min(bic)
w<-c()
for (i in 1:length(delta)){
w[i]<-(exp(-0.5*delta[i]))/sum(exp(-0.5*delta[1:length(delta)]))
}
er<-max(w)/w
w<-round(w,digits=4)
sel<-data.frame(k,lik,bic,delta,w,er)
dots<-substitute(list(...))[-1]
rownames(sel)<-sapply(dots,deparse)
names(sel)<-c("K","logLik","BIC","deltaBIC","Weight","Evidence ratio")
}

sel<-sel[order(delta),]
sel
}