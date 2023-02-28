prune2data<-function(tree,species){
if(is(tree,"phylo")==FALSE) stop("Tree must be of class phylo")
allnames<-c(species,tree$tip.label)
droplist<-which(!tree$tip.label%in%species)
drop<-tree$tip.label[droplist]
redtree<-drop.tip(tree,drop)
redtree
}