\name{pvcaBatchAssess.bbea}
\alias{pvcaBatchAssess.bbea}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Principal Variance Component Analysis of samples from expressionset or matrix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pvcaBatchAssess.bbea(eset, batch.factors, threshold, include.inter=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eset}{
     expressionSet of peptide data
  \item{batch.factors}
  	 batch metadata
  \item{threshold}
     pre-defined threshold for principal components containing an amount of variation 
  \item{include.inter}
     interactions to include
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Maria Suprun
Randall J. Ellis
Mayte Suárez-Fariñas
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
