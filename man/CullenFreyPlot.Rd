\name{CullenFreyPlot}
\alias{CullenFreyPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a Cullen-Frey plot of median fluorescence intensity values
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CullenFreyPlot(mat, filename="./CullenFrey.", 
boot=100, qqPlot=FALSE, scalePeptide=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
     matrix of median fluorescence intensity values
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
CullenFreyPlot(as.matrix(bbeaEgg$Median), filename = "QC.CullenFrey.MFI")
}

