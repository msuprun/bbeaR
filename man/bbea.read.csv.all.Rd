\name{bbea.read.csv.all}
\alias{bbea.read.csv.all}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read multiple Luminex CSV files
}
\description{
From a list of Luminex output CSV files, read and merge data
}
\usage{
bbea.read.csv.all(filenames)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filenames}{
     List of CSV filenames from Luminex xPONENT outputs
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
## file_names <- dir(path = "../inst/extdata/", pattern=".csv", all.files = FALSE, full.names = TRUE)
## bbea <- bbea.read.csv.all(file_names[grepl("MOIT_IgE",file_names)]) 
}

