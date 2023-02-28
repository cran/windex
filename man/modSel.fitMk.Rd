\name{modSel.fitMk}
\alias{modSel.fitMk}
\title{
Model selection table for categorical trait evolution models fit in the fitMk function in the package phytools, based on information theoretical measures.
}
\description{
Creates a model selection table based on either AIC or AICc for categorical trait evolution models fit using the fitMk() function in the phytools package.
}
\usage{
modSel.fitMk(...,tree=NULL,type="AICc")
}
\arguments{
  \item{\dots}{
A set of categorical trait evolution models fit with the fitMk function in the package phytools, which you want to compare.
}
  \item{tree}{
Either an object of class phylo used to fit the models being compared, or a numerical value giving the number of species in the tree used to fit the models being compared. Only necessary when type="AICc".
}
  \item{type}{
Type of information theoretical measure you want to use (AICc or AIC are allowed), defaults to AICc.
}
}

\value{
Model selection table with rownames corresponding to input model names and columns for K (number of parameters), logLik (log-likelihood), AICc (or AIC), deltaAICc (or deltaAIC, the difference between each model and the best model), Weight (Akaike weights, aka model probabilities), and Evidence ratio (the amount of evidence for the best model relative to each model such that, for instance, 4 would mean that model has 4x less evidence supporting it than the best model).
}

\author{
Kevin Arbuckle
}

\examples{
\dontrun{
# Three models initially run in phytools using fitMk (see help file for that package to fit
# these models) and saved as objects named 'er', 'sym', and 'ard'.

# Model selection table using AICc
modSel.fitMk(er,sym,ard,tree=phy)

# Model selection table using AIC
modSel.fitMk(er,sym,ard,tree=phy,type="AIC")}
}