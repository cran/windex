treecheck<-function(trees){
if(is(trees,"multiPhylo")){
b<-c()
u<-c()
for (i in 1:length(trees)){
if(is.binary.phylo(trees[[i]])!= TRUE) warning(paste("Tree number",i,"is not binary"))
b[i]<-is.binary.phylo(trees[[i]])
if(is.ultrametric(trees[[i]])!= TRUE) warning(paste("Tree number",i,"is not ultrametric"))
u[i]<-is.ultrametric(trees[[i]])
}
if(all(b==TRUE)) print("All trees are binary")
if(all(u==TRUE)) print("All trees are ultrametric")
}

else if(is(trees,"phylo")) {
if(is.binary.phylo(trees)==TRUE) print("Tree is binary") else print("Tree is not binary")
if(is.ultrametric(trees)==TRUE) print("Tree is ultrametric") else print("Tree is not ultrametric")
}

else stop('Trees must be of class multiPhylo or phylo')
}