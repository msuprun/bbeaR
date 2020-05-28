\name{binarizeMFIbySD}
\alias{binarizeMFIbySD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve binary indicator of peptide signal presence based on the number of standard deviations above background mean fluorescence intensity
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
binarizeMFIbySD(bbea.object,buffer.name="Buffer",plateVar="PlateNum",
         UR=c('PTID','Visit'),nSD=3,logMFI=TRUE,offset=0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bbea.object}{bbea data object}
  \item{buffer.name}{buffer to use for reference signal}
  \item{plateVar}{column indicating batch/plate}
  \item{UR}{unit of replication}
  \item{nSD}{number of standard deviations to use as the cutoff before indicating a positive signal presence}
  \item{logMFI}{boolean for taking the log transform of the mean fluorescence intensity}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
expressionSet
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
# adding patient data
bbeaEgg$pData <- mutate(bbeaEgg$pData, 
                     PTID = sapply(strsplit(Sample, "-",fixed=TRUE), head, 1),
                     SampleType = ifelse(grepl("Buff", Sample),"Buffer",
                                         ifelse(grepl("PP|NP", Sample),
                                                sapply(strsplit(Sample, "-",fixed=TRUE),head, 1), "Patient")))
PDm <- merge(bbeaEgg$pData, PDegg, by = "PTID", all.x = TRUE)
rownames(PDm) <- PDm$File
bbeaEgg$pData <- PDm[colnames(bbeaEgg$Median),]
binary.db <- binarizeMFIbySD(bbea.object = bbeaEgg,
                             buffer.name = "Buffer",
                             plateVar = "Plate",
                             UR = 'PTID', 
                             nSD = 3) 
sampleNames(binary.db) <- sapply(strsplit(sampleNames(binary.db), "_"),tail, 1)
# can remove background (Buffer) wells
binary.db <- binary.db[, !grepl("Buffer", sampleNames(binary.db))]
}

