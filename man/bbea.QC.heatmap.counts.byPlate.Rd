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

\arguments{
  \item{bbea.obj}{bbeaR data object}
  \item{getlog2}{boolean to calculate the log2 expression of each epitope}
}

\author{
Maria Suprun
Randall J. Ellis
Mayte Suárez-Fariñas
}

\seealso{
bbea.QC.heatmap.counts
}
\examples{
data(Milk)
bbea.QC.heatmap.counts.byPlate(bbea, getlog2=FALSE,
                               filename="QC.CountsHeatmap",
                               plateVar="Plate", ann=NULL, he=7, wi=9)
}

