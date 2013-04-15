\name{read_data_package}
\alias{read_data_package}
\title{Read in a data package.}
\usage{
  read_data_package(content, base, getter)
}
\arguments{
  \item{content}{The Data Package to parse. This can either
  be a URL, a path to a local file, or a character string
  containing the actual JSON describing the data package.
  See
  \url{http://www.dataprotocols.org/en/latest/data-packages.html}.}

  \item{base}{The base URL or directory for all references
  to data. This will be ignored if the \code{content}
  provided is either a URL or a local file. Only if
  \code{content} is a character string will this be used.}

  \item{getter}{The \link{Getter} to use when retrieving
  the specified data. By default, the getter will be
  inferred based on the structure of the \code{content}
  parameter. Alternatively, you can explicitly set one
  here, or even provide a custom getter.}
}
\description{
  Read in a data package.
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

