\name{modSelTab}
\alias{modSelTab}
\title{
Model selection table based on information theoretical measures.
}
\description{
Creates a model selection table based on either AICc, AIC or BIC for a range of model types. The function is written for GLM style models (e.g. using glm, lm, aov, lmer, or glmer functions) but should work for any model to which the base R functions logLik(), model.frame() and AIC() or BIC() can be applied.
}
\usage{
modSelTab(...,type="AICc")
}
\arguments{
  \item{\dots}{
A set of fitted models you want to compare.
}
  \item{type}{
Type of information theoretical measure you want to use (AICc, AIC and BIC are allowed), defaults to AICc.
}
}

\value{
Model selection table with rownames corresponding to input model names and columns for K (number of parameters), logLik (log-likelihood), AICc (or AIC or BIC), deltaAICc (or deltaAIC or deltaBIC, the difference between each model and the best model), Weight (Akaike weights, aka model probabilities), and Evidence ratio (the amount of evidence for the best model relative to each model such that, for instance, 4 would mean that model has 4x less evidence supporting it than the best model).
}

\author{
Kevin Arbuckle
}

\examples{
# Simulating some variables
y<-rnorm(mean=100,sd=30,500)
x1<-0.5*y+10+rnorm(mean=20,sd=10,500)
x2<-3*y-45+rnorm(mean=40,sd=150,500)

# Fitting GLMs to those variables to give three models for comparison
m1<-glm(y~x1)
m2<-glm(y~x2)
m3<-glm(y~x1+x2)

# Model selection table using AICc
modSelTab(m1,m2,m3)

# Model selection table using AIC
modSelTab(m1,m2,m3,type="AIC")

# Model selection table using BIC
modSelTab(m1,m2,m3,type="BIC")
}