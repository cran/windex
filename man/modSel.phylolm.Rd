\name{modSel.phylolm}
\alias{modSel.phylolm}
\title{
Model selection table for phylogenetic (logistic) regressions fit in the phylolm and phyloglm functions in the package phylolm, based on information theoretical measures.
}
\description{
Creates a model selection table based on either AIC or AICc for phylogenetic (logistic) regressions fit using the phylolm() or phyloglm() functions in the phylolm package.
}
\usage{
modSel.phylolm(...,tree=NULL,type="AICc",method=c("phylolm","logistic"))
}
\arguments{
  \item{\dots}{
A set of phylogenetic (logistic) regressions fit in the phylolm and phyloglm functions in the package phylolm, which you want to compare.
}
  \item{tree}{
Either an object of class phylo used to fit the models being compared, or a numerical value giving the number of species in the tree used to fit the models being compared. Only necessary when type="AICc".
}
  \item{type}{
Type of information theoretical measure you want to use (AICc or AIC are allowed), defaults to AICc.
}
  \item{method}{
Whether the models being compared are standard phylogenetic regression (fit with phylolm function) or phylogenetic logistic regression (fit with phyloglm function). Defaults to phylolm but issues a warning if you haven't specified the method.
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
# Three models initially run in phylolm using phylolm or phyloglm
# (see help file for that package to fit these models) and saved
# as objects named 'mod1', 'mod2', and 'mod3'.

# Model selection table for phylogenetic regressions using AICc
modSel.phylolm(mod1,mod2,mod3,tree=phy,method="phylolm")

# Model selection table for phylogenetic regressions using AIC
modSel.phylolm(mod1,mod2,mod3,type="AIC",tree=phy,method="phylolm")

# Model selection table for phylogenetic logistic regressions using AICc
modSel.phylolm(mod1,mod2,mod3,tree=phy,method="logistic")}
}