treedatacheck<-function(trees,species){
if(is(trees,"multiPhylo")){
b<-c()
u<-c()
m<-c()
for (i in 1:length(trees)){
if(is.binary.phylo(trees[[i]])!= TRUE) warning(paste("Tree number",i,"is not binary"))
b[i]<-is.binary.phylo(trees[[i]])
if(is.ultrametric(trees[[i]])!= TRUE) warning(paste("Tree number",i,"is not ultrametric"))
u[i]<-is.ultrametric(trees[[i]])
if(length(trees[[i]]$tip.label) > length(species)) print(paste("Tree number",i,"contains more species than data. Perhaps the prune2data function could be useful?")) else
if(length(trees[[i]]$tip.label) < length(species)) print(paste("Data contains more species than tree number",i)) else
if(all(sort(trees[[i]]$tip.label)==sort(species))!=TRUE) print(paste("Mismatch between species in tree number",i,"and the data. Running the function on this tree individually will provide troubleshooting options")) else
m[i]<-all(sort(trees[[i]]$tip.label)==sort(species))
}
if(all(b==TRUE)) print("All trees are binary")
if(all(u==TRUE)) print("All trees are ultrametric")
if(length(m)==length(trees) & all(m==TRUE)) print("All tip labels match data")
}

else if(is(trees,"phylo")) {
if(is.binary.phylo(trees)==TRUE) print("Tree is binary") else print("Tree is not binary")
if(is.ultrametric(trees)==TRUE) print("Tree is ultrametric") else print("Tree is not ultrametric")
if(length(trees$tip.label) > length(species)) print("Tree contains more species than data. Perhaps the prune2data function could be useful?") else
if(length(trees$tip.label) < length(species)) print("Data contains more species than tree") else
if(all(sort(trees$tip.label)==sort(species))) print("All tip labels match data") else { print("Tip labels do not match data")
tsdf<-data.frame(Tree=sort(trees$tip.label),Data=sort(species))
tsq<-readline("Do you want to list names side-by-side to troubleshoot [y/n]? ")
if(tsq=="y") print(tsdf)
idq<-readline("Do you want to identify mismatches from this list [y/n]? ")
if(idq=="y") print(tsdf[which(tsdf$Tree!=tsdf$Data),])
}
}

else print("Trees must be of class multiPhylo or phylo")
}