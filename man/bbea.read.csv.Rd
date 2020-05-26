\name{bbea.read.csv}
\alias{bbea.read.csv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read Luminex data from original output file
}
\description{
From a CSV file of the Luminex output, create a list containing the same output, along with Median, NetMFI, and Count matrices with rows as analytes (epitopes) and columns as samples. The Median are the Median Flourescence Intensities (MFIs) and the NetMFI are the Medians normalized to background. Median and NetMFI will have exactly same values if normalization was not selected during the assay run.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bbea.read.csv(fname="...")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fname}{
  Filepath to CSV
%%     ~~Describe \code{x} here~~
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
Maria Suprun
Randall J. Ellis
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
## Do not run
## bbea <- bbea.read.csv(fname = "../inst/extdata/BBEA_OVM_58p_IgE_patients.csv") 
}