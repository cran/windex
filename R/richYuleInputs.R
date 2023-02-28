richYuleInputs<-function(tree,x,rich=NULL){
if(is(tree,"phylo")==FALSE) stop('Tree must be of class phylo')
intNodes=(Ntip(tree)+1L):(Ntip(tree)+Nnode(tree))
sisters=Children(tree,intNodes)
sisters=matrix(unlist(sisters),byrow=TRUE,ncol=2,dimnames=list(intNodes,NULL))
desc=Descendants(tree)
x=(x=="1")

if (is.null(rich)==T){
l=sapply(desc,length)
yes=sapply(desc,function(d,x)sum(x[d]),x)
}
else {
nom=sapply(desc,function(x,tree)tree$tip.label[x],tree)
l=sapply(nom,function(x,sprich)sum(sprich[x]),rich)
yes=sapply(nom,function(d,sprich,x)sum(sprich[x][d],na.rm=T),rich,x)
}

no=(l-yes)
allno=(no==l)
allyes=(yes==l)
res=rbind(cbind(no[sisters[,1]],yes[sisters[,2]])[allno[sisters[,1]]&allyes[sisters[,2]],],cbind(no[sisters[,2]],yes[sisters[,1]])[allno[sisters[,2]]&allyes[sisters[,1]],])
res=as.data.frame(res)
res<-data.frame("with"=res[,2],"without"=res[,1])
sisnodes=c(rownames(sisters)[allno[sisters[,1]]&allyes[sisters[,2]]],rownames(sisters)[allno[sisters[,2]]&allyes[sisters[,1]]])

if (is.null(rich)==T) {
nodeages<-max(node.depth.edgelength(tree))-node.depth.edgelength(tree)
}
else {
nodeages<-max(node.depth.edgelength(tree))-node.depth.edgelength(tree)[intNodes]
}

tree$node.label<-nodeages[(length(nodeages)-(Nnode(tree)-1)):length(nodeages)]
sisdivtim=as.numeric(tree$node.label[as.numeric(sisnodes)-Ntip(tree)])
return(list(sisRich=res,divTimes=sisdivtim))
}