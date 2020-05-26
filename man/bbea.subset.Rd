\name{bbea.subset}
\alias{bbea.subset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Subset bbea object
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bbea.subset(bbea.obj, statement)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bbea.obj}{bbea data object}
  \item{statement}{statement criterion to subset data by}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{L1 }{Median, mean, and counts of fluorescence}
  \item{L2 }{Phenotype data}
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
# keeping samples with avergae analyte count of at least 25
data(Milk)
bbea.sub <- bbea.subset(bbea, statement = (bbea$pData$CountMean > 25))
}

