\name{pvca.plot}
\alias{pvca.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a Principal Variance Components Analysis (PVCA) plot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pvca.plot(pvcaObj, cex.percentage = 1, fname = NULL, ht = 4, wd = 5, title = fname,ylim=c(0,1), order=labels)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pvcaObj}{PVCA data}
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
data(Milk)
bbeaN <- MFI2nMFI(bbea, offset = 0.5, rmNeg = TRUE) 
eset <- nMFI2Eset(nMFI.object = bbeaN)
pvca.obj<-pvcaBatchAssess.bbea(eset,
                               threshold=0.8,
                               batch.factors=c('Plate','Treatment','Visit',"SUBJECT.ID"),  
                               include.inter='Treatment:Visit') 
pvca.plot(pvca.obj, fname='PVCA.Plate.Covariates', ht=4, wd=5.5,
          order=c('Plate','Treatment','Visit','Treatment x Visit',"SUBJECT.ID",'resid'))

}

