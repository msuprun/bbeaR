\name{MFI2nMFI}
\alias{MFI2nMFI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normalize mean flourescence intensity (MFI) values
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MFI2nMFI(bbea.obj, offset=0.5, rmNeg=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bbea.obj}{
     bbea data object
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
bbeaN <- MFI2nMFI(bbeaEgg, offset = 0.5, rmNeg = TRUE) 
}

