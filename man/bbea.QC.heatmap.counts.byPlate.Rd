\name{bbea.QC.heatmap.counts.byPlate}
\alias{bbea.QC.heatmap.counts.byPlate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quality control heatmap of epitope counts by individual plate
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bbea.QC.heatmap.counts.byPlate(bbea.obj, getlog2=FALSE, filename='...')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bbea.obj}{
     bbeaR data object
  \item{getlog2}
     boolean to calculate the log2 expression of each epitope
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
bbea.QC.heatmap.counts
}
\examples{
data(Milk)
bbea.QC.heatmap.counts.byPlate(bbea, getlog2=FALSE,
                               filename="QC.CountsHeatmap",
                               plateVar="Plate", ann=NULL, he=7, wi=9)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
