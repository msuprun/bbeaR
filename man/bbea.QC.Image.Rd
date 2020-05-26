\name{bbea.QC.Image}
\alias{bbea.QC.Image}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting plate layout with sample labels.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bbea.QC.Image(db, plate.design.db=NULL, 
ht=4, wd=6, txtsize=2, hjust=0.5, 
filename="PlateImage")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{db}{experiment outputs}
  \item{plate.design.db}{plate layout}
  \item{ht}{image height}
  \item{wd}{image width}
  \item{txtsize}{label size}
  \item{hjust}{label position}
  \item{filename}{directory and filename to save pdf}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
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
}
\examples{
data(Egg)
l <- create.plate.db(direction = "horizontal")
bbea.QC.Image(db=bbeaEgg$pData,
filename = "QC.Image.",
plate.design.db=l$plate.design.db)
}

