\name{Image.Plate}
\alias{Image.Plate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
For each plate in the object (binding, eset, or LuEpit), generate PDF with values for all plates
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Image.Plate(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{bbea data object or ExpressionSet}
  \item{direction}{was the plate read "horizontally" or "vertically" on the Luminex reader}
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
# normalize MFI
bbeaN <- MFI2nMFI(bbea, offset = 0.5,rmNeg = TRUE)
# plot plate layout
Image.Plate(bbeaN)
}

