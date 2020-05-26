\name{getPCAs}
\alias{getPCAs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Principal component analysis of samples from ExpressionSet or matrix
}

\description{
A wrapper function of the stats::prcomp that takes an ExpressionSet or a matrix, calculates principal comonents, and merges the results with phenotype/clinical data (from ExpressionSet object).
}

\usage{
getPCAs(eset, maxpc = 3)
}

\arguments{
\item{eset}{expressionSet or matrix of the epitope data, where samples are columns, and features are rows}
\item{maxpc}{number of principal components in the output}
}

\value{
The function returns a list object with a data.frame, which consists of pricincipal components and phenotypic data (if eset instead of matrix was provided), and varex - proportion of variance explained by each component.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Maria Suprun,
Randall J. Ellis,
Mayte Suárez-Fariñas
}

\examples{
data(Egg)
pca.db <- getPCAs(as.matrix(bbea$Median), maxpc = 3)
head(pca.db$db)
pca.db$varex
}
