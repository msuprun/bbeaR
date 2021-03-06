\name{bbea.QC.Samples}
\alias{bbea.QC.Samples}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quality control of samples by visualization of average counts
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bbea.QC.Samples(bbea.obj, filename='...')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bbea.obj}{
     bbeaR data object
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{pavg}{Average counts of all samples}
\item{pmin}{Minimum counts of all samples}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Maria Suprun,
Randall J. Ellis,
Mayte Suárez-Fariñas
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
bbea.QC.heatmap.counts.byPlate
}
\examples{
data(Egg)
p<-bbea.QC.Samples(bbeaEgg,
               filename = "QC.",
               plateVar = 'Plate',
               gt = 25)
p$pmin
}

