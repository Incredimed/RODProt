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
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

