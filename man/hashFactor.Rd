\name{hashFactor}
\alias{hashFactor}
\title{Create a hashFactor}
\usage{
  hashFactor(x = character(), levels, codes)
}
\arguments{
  \item{x}{The values to be encoded into a factor}

  \item{levels}{The labels associated with each level of
  the factor}

  \item{codes}{the integer codes associated with each level
  of the factor}
}
\description{
  Creates a hashFactor object, which operates in much the
  same way as a factor, but doesn't require a continuous
  sequence of integers for the levels.
}
\examples{
fac <- hashFactor(sample(LETTERS[1:5], 10, replace=TRUE),
	levels=LETTERS[1:5], codes=c(1,5,8,2,6))
d <- data.frame(i=11:20, f=fac)
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

