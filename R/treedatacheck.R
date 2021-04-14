treedatacheck<-function(trees,species){
if(class(trees)=="multiPhylo"){
b<-c()
u<-c()
m<-c()
for (i in 1:length(trees)){
if(is.binary.phylo(trees[[i]])!= TRUE) warning(paste("Tree number",i,"is not binary"))
b[i]<-is.binary.phylo(trees[[i]])
if(is.ultrametric(trees[[i]])!= TRUE) warning(paste("Tree number",i,"is not ultrametric"))
u[i]<-is.ultrametric(trees[[i]])
if(all(sort(trees[[i]]$tip.label)==sort(species))!=TRUE) warning(paste("Mismatch between species in tree number",i,"and the data"))
m[i]<-all(sort(trees[[i]]$tip.label)==sort(species))
}
if(all(b==TRUE)) print("All trees are binary")
if(all(u==TRUE)) print("All trees are ultrametric")
if(all(m==TRUE)) print("All tip labels match data")
}

else if(class(trees)=="phylo") {
if(is.binary.phylo(trees)==TRUE) print("Tree is binary") else print("Tree is not binary")
if(is.ultrametric(trees)==TRUE) print("Tree is ultrametric") else print("Tree is not ultrametric")
if(all(sort(trees$tip.label)==sort(species))) print("All tip labels match data") else print("Tip labels do not match data")
}

else stop('Trees must be of class multiPhylo or phylo')
}