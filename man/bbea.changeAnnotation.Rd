\name{bbea.changeAnnotation}
\alias{bbea.changeAnnotation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert analytes to peptides in the bbeaR object created after importing the Luminex output file
}
\description{
Replace analyte numbers with peptide names using an annotation file
}
\usage{
changeAnnotation(bbea.obj, annotation, newNameCol="Peptide", AnalyteCol="Analyte")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bbea.obj}{
     bbeaR dataset
  \item{annotation}{
  	 annotation mapping between analytes and peptides
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
