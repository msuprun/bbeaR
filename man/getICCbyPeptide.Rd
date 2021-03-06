\name{getICCbyPeptide}
\alias{getICCbyPeptide}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the Intraclass Correlation Coefficient for each peptide
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getICCbyPeptide(eset, UR="PTID")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eset}{expressionSet of peptides}
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

# convert MFI to normlaized MFI
bbeaN <- MFI2nMFI(bbeaEgg, offset = 0.5, rmNeg = TRUE)

# chnage to the ExpressionSet object
eset <- nMFI2Eset(nMFI.object = bbeaN)

# get ICC of the replicates 
icc.db <- getICCbyPeptide(eset, UR=c("PTID","Plate"))
head(icc.db)
}
