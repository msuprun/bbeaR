\name{read.BBEA.csv}
\alias{read.BBEA.csv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read Luminex data from original output file
}
\description{
From a CSV file of the Luminex output, create a list containing the same output, along with Median, NetMFI, and Count matrices with rows as analytes (epitopes) and columns as samples. The Median are the Median Flourescence Intensities (MFIs) and the NetMFI are the Medians normalized to background. Median and NetMFI will have exactly same values if normalization was not selected during the assay run.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.BBEA.csv(fname="...")
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
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
bbea <- read.BBEA.csv(fname="../Data/BBEA_Egg-OVM-IgE/BBEA_OVM_58p_IgE_patients_20190625_165733.csv")
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
