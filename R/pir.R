pir<-function(tree,trait1,trait2=NULL){
if(class(tree)!="phylo") stop('Tree must be of class phylo')
if(class(trait1) == "factor") x<-as.character(trait1) else x<-trait1
if(is.null(trait2) == FALSE){
if(class(trait2) == "factor") y<-as.character(trait2) else y<-trait2
if(class(y) != "character") stop("trait(s) must be a named factor or character vector")
x<-paste(x,y,sep="")
}
names(x)<-names(trait1)
if(class(x) != "character") stop("trait(s) must be a named factor or character vector")

ci<-CI(tree,as.phyDat(x,type="USER",levels=unique(x)))
nir<-(max(table(x))-min(table(x)))/sum(table(x))
pir<-ci*nir

return(list("CI"=ci,"NIR"=nir,"PIR"=pir))
}