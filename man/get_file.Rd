\name{get_file}
\alias{get_file}
\title{Download Data Package File}
\usage{
  get_file(dataPkg, file)
}
\arguments{
  \item{dataPkg}{The Data Package (as loaded from
  \code{\link{read_data_package}}) to use.}

  \item{file}{A character string specifying the names of
  the file to retrieve from the Data Package. At the time
  of development, it was unclear exactly how a file would
  be indexed (see
  \url{https://github.com/dataprotocols/dataprotocols/issues/32}),
  thus the package supports a variety of indexing
  mechanisms for files. The order of precedence is as
  follows: \enumerate{ \item Keys from the \code{files}
  hash - If the \code{files} value was provided as a JSON
  hash instead of an array, the required name prefacing
  each file will be used as that file's canonical name.
  \item The \code{id} field within each file will be
  checked for any exact matches. \item The \code{name}
  field within each file will be checked for any exact
  matches. \item The \code{url} and \code{path} fields
  within each file will be checked for any exact matches.
  \item The \code{url} and \code{path} fields within each
  file will be checked for partial matches on the extracted
  \code{<file>} portion of a \code{/<path>/<file>}
  reference. The path is extracted using
  \code{\link{dirname}} and ignored to see if the remainder
  of the string matches (with the possible exclusion of a
  prefacing slash). }}
}
\description{
  Download a file referenced by a Data Package.
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

