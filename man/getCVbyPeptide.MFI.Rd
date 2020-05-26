\name{getCVbyPeptide.MFI}
\alias{getCVbyPeptide.MFI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the coefficient of variation for each peptide's mean fluorescence intensity
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getCVbyPeptide.MFI(bbea.obj, UR="PTID, Plate")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bbea.obj}{bbea data object}
  \item{UR}{unit of replication (e.g., patient, visit)}
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
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(Egg)
cv.db <- getCVbyPeptide.MFI(bbeaEgg, UR = c("PTID","Plate"))
head(cv.db)
}
