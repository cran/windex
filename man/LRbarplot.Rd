\name{LRbarplot}
\alias{LRbarplot}
\title{
Plots a barplot from logistic regression parameter estimates
}
\description{
Takes a fitted logistic regression model (e.g. with glm or phyloglm functions) and plots a barplot for a categorical predictor of the probability of the response variable based on estimated coefficients and standard errors.
}
\usage{
LRbarplot(mod,parIndex=2,yName="response",
xLevels=paste(c(1:length(c(1,parIndex)))),main="",...)
}
\arguments{
  \item{mod}{
Fitted logistic regression model.
}
  \item{parIndex}{
Index giving the parameter(s) for the variable to be plotted, in order of the model output with the intercept being 1 (but not included in this argument). Default of 2 will work for a model with a binary predictor as the first (or only) explanatory variable in the model, as this is the second parameter listed. Note that for predictors with more than two levels this will be a vector of length k-1, where k is the number of levels, to specify each parameter estimated in addition to the intercept.
}
  \item{yName}{
Name of response variable in the model to be used as part of y-axis label (doesn't have to be identical to the model input).
}
  \item{xLevels}{
Vector of names of levels of the categorical variable to be used as x-axis labels. Levels must be given in the same order they appear in the model, but they don't have to be identical to the model input. These should be changed as the default is simply to use sequential numbers and so is fairly uninformative.
}
  \item{main}{
Main title for plot (defaults to no title).
}
  \item{\dots}{
Arguments to be passed to barplot() to customise appearance of plot.
}
}

\note{
Because this is plotting directly from estimated coefficients, the probabilities will be conditional on the intercept (which gives the reference level as the first one plotted). For models with a single explanatory variable this will always be unproblematic, and should often be good for a small number of covariates, but as the model gets more complex this approach may become a poorer representation of the data. The function was originally intended for plotting models for which the raw data often do not clearly show effects (e.g. when raw data doesn't consider phylogenetic effects or covariates included in the model).
}

\author{
Kevin Arbuckle
}

\examples{
bitten<-sample(c(0,1),100,replace=TRUE,prob=c(0.4,0.6))
sex<-sample(c("Male","Female"),100,replace=TRUE,prob=c(0.5,0.5))
hab<-sample(c("Forest","Savannah"),100,replace=TRUE,prob=c(0.3,0.7))
sp<-sample(c("Cobra","Viper","Mamba","Boomslang"),100,replace=TRUE,
prob=c(0.4,0.25,0.2,0.15))
testdat<-data.frame(bitten,sex,hab,sp)

# Single binary variable in model
testmod1<-glm(bitten~sex,data=testdat,family=binomial)
LRbarplot(testmod1,parIndex=2,yName="receiving bite",
xLevels=levels(as.factor(testdat$sex)),cex.lab=1.5,cex.axis=1.5,
cex.names=1.5,col=c("coral1","cornflowerblue"))
mtext("Sex",line=3,adj=0.5,font=2,cex=1.5, side=1)

# Single 4-level variable in model
testmod2<-glm(bitten~sp,data=testdat,family=binomial)
LRbarplot(testmod2,parIndex=2:4,yName="receiving bite",
xLevels=levels(as.factor(testdat$sp)),cex.lab=1.5,cex.axis=1.5,
cex.names=1.5,col=c("green","tan2","ivory4","sienna"))
mtext("Species",line=3,adj=0.5,font=2,cex=1.5, side=1)

# Plotting the second of two binary variables in the model
testmod3<-glm(bitten~sex+hab,data=testdat,family=binomial)
LRbarplot(testmod3,parIndex=3,yName="receiving bite",
xLevels=levels(as.factor(testdat$hab)),cex.lab=1.5,cex.axis=1.5,
cex.names=1.5,col=c("dark green","light yellow"))
mtext("Habitat",line=3,adj=0.5,font=2,cex=1.5, side=1)

}