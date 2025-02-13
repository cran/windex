windex <-
function(dat, tree, traits, focal=dat[,2],SE=TRUE,fossil=FALSE,species.col="species"){

if(is(tree,"phylo")==FALSE) stop('Tree must be of class phylo') #The tree is of the appropriate class

if(all(tree$edge.length=="NULL")) stop('Tree must contain branch lengths')
 
if(is.ultrametric(tree)!=TRUE & fossil==FALSE) stop('Your tree is not ultrametric, so if this is intentional make sure you set fossil=TRUE') #
 
if (is.vector(dat[,traits]) ==  TRUE) if(is.numeric(dat[,traits]) != TRUE) stop("Trait data must be numeric")
if (is.vector(dat[,traits]) !=  TRUE) for (i in 1:length(dat[,traits])){
if(is.numeric(dat[,traits[i]]) != TRUE) stop("Trait data must be numeric")
}
 
if(sort(unique(focal))[1]!=0) stop('Focals must be a binary string denoting non-focals as 0 and focals as 1')
if(sort(unique(focal))[2]!=1) stop('Focals must be a binary string denoting non-focals as 0 and focals as 1')
if(length(unique(focal))!=2) stop('Focals must be a binary string denoting non-focals as 0 and focals as 1')
 
rownames(dat)<-dat[,species.col] #sort dat by tree species name order
match<-match(tree$tip.label, rownames(dat))
dat<-dat[,][match,]

if(any(sort(rownames(dat))!=sort(tree$tip.label))) stop('Rownames of the data do not match the tip labels from the tree. Check species.col is specified correctly or perhaps the treedatacheck function might be useful')

if(SE==TRUE){
if(length(traits)==1){
  se<-sd(dat[,traits])/sqrt(length(dat[,1]))
} else{
 se<-sqrt(diag(var(dat[,traits]))/(length(dat[,1]))) #calculate standard errors for trait col independently
}
data.w<-dat[,traits]/rep(se,each=length(dat[,1]))  #adjust trait cols by standard error of that col
 
} else data.w<-dat[,traits]

dij<-as.matrix(dist(data.w, method = "euclidean",diag=T,upper=T)) #calculate matrix of Euclidean distances for phenotypic
Tree1<-tree
Tree1$edge.length<-Tree1$edge.length/max(nodeHeights(Tree1))
      pij<-vcv(Tree1) #shared proportional distance 
	if(fossil==TRUE) {
	cophenmat<-1-(cophenetic.phylo(Tree1)/2)
	cophenmat[which(round(cophenmat,digits=10)!=round(pij,digits=10))]<-1-cophenmat[which(round(cophenmat,digits=10)!=round(pij,digits=10))]
	pij<-cophenmat
	}
      dij.<-dij/(1-log(pij+0.01)) #calculate corrected phenotypic distance matrix
da<-mean(dij.)
df<-mean(dij.[which(focal==1),which(focal==1)])
w.index<-da/df #calculate Wheatsheaf index


w.vec<-c() #create empty vector to put in jackknife
n<-length(dat[,1])
for(i in 1:n){
data<-data.w
if(length(traits)==1){
 data[i]<-"NA"
} else data[i,]<-rep("NA",length(traits))# replace each row with NAs for trait col independently
options(warn=-1) #turn off warnings temporarily
dijJ<-as.matrix(dist(data, method = "euclidean",diag=T,upper=T))
options(warn=+1) #turn warnings back on
Tree1<-tree
Tree1$edge.length<-Tree1$edge.length/max(nodeHeights(Tree1))
     pij<-vcv(Tree1) #shared proportional distance 
     	if(fossil==TRUE) {
	cophenmat<-1-(cophenetic.phylo(Tree1)/2)
	cophenmat[which(round(cophenmat,digits=10)!=round(pij,digits=10))]<-1-cophenmat[which(round(cophenmat,digits=10)!=round(pij,digits=10))]
	pij<-cophenmat
	}
     dij.J<-dijJ/(1-log(pij+0.01)) #calculate corrected phenotypic distance matrix
daJ<-mean(dij.J,na.rm = TRUE)
dfJ<-mean(dij.J[which(focal==1),which(focal==1)],na.rm = TRUE)
w<-daJ/dfJ #calculate Wheatsheaf index
w.vec[i]<-w
}
ave<-mean(w.vec)
low<-quantile(w.vec,probs=0.025,names=F)
high<-quantile(w.vec,probs=0.975,names=F)

return(list("w"=w.index,"low95"=low,"up95"=high))
}
