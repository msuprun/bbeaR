\name{bbea.QC.heatmap.counts}
\alias{bbea.QC.heatmap.counts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quality control heatmap of epitope counts
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bbea.QC.heatmap.counts(bbea.obj, getlog2=FALSE, filename='...')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bbea.obj}{bbeaR data object}
  \item{getlog2}{boolean to calculate the log2 expression of each epitope}
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
Maria Suprun,
Randall J. Ellis,
Mayte Suárez-Fariñas
}

\seealso{
bbea.QC.heatmap.counts.byPlate
}
\examples{
data(Egg)
bbea.QC.heatmap.counts(bbeaEgg,
                       getlog2 = FALSE,
                       filename = "QC.CountsHeatmap.pdf",
                       plateVar = "Plate", ann = NULL)

}

