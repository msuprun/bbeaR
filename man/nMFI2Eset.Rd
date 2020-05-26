\name{nMFI2Eset}
\alias{nMFI2Eset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert normalized mean fluorescence intensity values object to expression set
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nMFI2Eset(nMFI.object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nMFI.obj}{
     Normalized mean fluorescence intensity values object
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
data(Milk)
bbeaN <- MFI2nMFI(bbea, offset = 0.5, rmNeg = TRUE) 
eset <- nMFI2Eset(nMFI.object = bbeaN)
}

