\name{getEnzymeCuts}
\alias{getEnzymeCuts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add sites of enzymatic cuts (by trypsin, pepsin, and chymotrypsin)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getEnzymeCuts(sequence,topo = FALSE, topodb = NULL, rmsignal=T, signalend=24)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sequence}{amino acid sequence}
  \item{topo}{boolean for a dataframe generated with getTopologyPlotDB()}
  \item{topodb}{name of the getTopologyPlotDB() dataframe}
  \item{rmsignal}{boolean for the removal of the signal peptide chain}
  \item{signalend}{final amino acid residue of the signal chain}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The functions returns a data.frame with amino acid positiob (AA.Number) and indicator variables if that position could be enzymatically cut by Pepsin (P), Trypsin (T), or Chymotrypsin (Ch). "Enzyme" column is a cancatenation of P,T,and Ch columns. 
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
aa.seq<-"MAMAGVFVLFSFVLCGFLPDAAFGAEVDCSRFPNATDKEGKDVLVCNKDLRPICGTDGVTYTNDCLLCAYSIEFGTNISKEHDGECKETVPMNCSSYANTTSEDGKVMVLCNRAFNPVCGTDGVTYDNECLLCAHKVEQGASVDKRHDGGCRKELAAVSVDCSEYPKPDCTAEDRPLCGSDNKTYGNKCNFCNAVVESNGTLTLSHFGKC"
getEnzymeCuts(aa.seq, topo = FALSE, rmsignal = TRUE, signalend = 24)
}

