\name{bbea.QC.Image}
\alias{bbea.QC.Image}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quality control of plates visualized over dates of experiments in a single image
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bbea.QC.Image(db, ht=4, wd=6, filename="...")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{db}{experiment outputs}
  \item{ht}{image height}
  \item{wd}{image width}
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
bbea.QC.Image(bbeaEgg$pData,
              filename = "QC.Image.",
              plate.layout = plate.design.db)
}

