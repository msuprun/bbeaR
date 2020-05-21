\name{mgsub}
\alias{mgsub}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiple string substitution based on a specific pattern.
}
\description{
From a pattern and replacement string of the same length, 
}
\usage{
mgsub(pattern, replacement)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pattern}{
     pattern to be replaced}
  \item{replacement}{
     string that is inserted to substitute the pattern}
}


\author{
Maria Suprun
Randall J. Ellis
Mayte Suárez-Fariñas
}

\examples{
mgsub(c("A","z","bc"),c("a","Z","BC"),"Azbc")
}

