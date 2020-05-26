\name{create.plate.db}
\alias{create.plate.db}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create plate layout
}
\description{
Create a 96-well plate layout in a specific direction, with 8 rows and 12 columns.
}
\usage{
create.plate.db(direction="horizontal")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{direction}{
Orientation of 96-well plate (8 rows, 12 columns), either horizontal or vertical.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{plat.design.db }{dataframe of plates}
\item{plat.design }{design of plates}
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

\examples{
plate <- create.plate.db(direction="horizontal")
plate$plate.design
}

