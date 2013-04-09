\name{get_file}
\alias{get_file}
\title{Download Data Package File}
\usage{
  get_file(dataPkg, file, cache = TRUE)
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

  \item{cache}{Whether or not to cache the file once
  retrieved. If \code{TRUE}, the function will check this
  Data Package to see if the file has already been cached.
  If it has, it will simply return the file from the cache.
  If it has not, the function will retrieve the remote file
  then save a copy in this package's cache. Note that this
  will involve storing an extra copy of the data, doubling
  memory usage. If \code{FALSE}, the function will retrieve
  the file remotely, but not store the result in the cache
  when returning. The third option is to set cache to
  \code{"flush"}. When this occurs, the function will
  retrieve the file remotely, ignoring any pre-existing
  local copy, and store the result in the cache for future
  use.}
}
\description{
  Download a file referenced by a Data Package.
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

