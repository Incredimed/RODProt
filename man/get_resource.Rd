\name{get_resource}
\alias{get_resource}
\title{Download Data Package Resource}
\usage{
  get_resource(dataPkg, resource, cache = TRUE, ...)
}
\arguments{
  \item{dataPkg}{The Data Package (as loaded from
  \code{\link{read_data_package}}) to use.}

  \item{resource}{A character string specifying the names
  of the resource to retrieve from the Data Package. At the
  time of development, it was unclear exactly how a
  resource would be indexed (see
  \url{https://github.com/dataprotocols/dataprotocols/issues/32}),
  thus the package supports a variety of indexing
  mechanisms for resources. The order of precedence is as
  follows: \enumerate{ \item Keys from the \code{resources}
  hash - If the \code{resources} value was provided as a
  JSON hash instead of an array, the required name
  prefacing each resource will be used as that resource's
  canonical name. \item The \code{id} field within each
  resource will be checked for any exact matches. \item The
  \code{name} field within each resource will be checked
  for any exact matches. \item The \code{url} and
  \code{path} fields within each resource will be checked
  for any exact matches. \item The \code{url} and
  \code{path} fields within each resource will be checked
  for partial matches on the extracted \code{<resource>}
  portion of a \code{/<path>/<resource>} reference. The
  path is extracted using \code{\link{dirname}} and ignored
  to see if the remainder of the string matches (with the
  possible exclusion of a prefacing slash). }}

  \item{cache}{Whether or not to cache the resource once
  retrieved. If \code{TRUE}, (default) the function will
  check this Data Package to see if the resource has
  already been cached. If it has, it will simply return the
  resource from the cache. If it has not, the function will
  retrieve the remote resource then save a copy in this
  package's cache. Note that this will involve storing an
  extra copy of the data, doubling memory usage. If
  \code{FALSE}, the function will retrieve the resource
  remotely, but not store the result in the cache when
  returning. The third option is to set cache to
  \code{"flush"}. When this occurs, the function will
  retrieve the resource remotely, ignoring any pre-existing
  local copy, and store the result in the cache for future
  use.}

  \item{...}{Arguments to be passed on to
  \link{read_json_table}.}
}
\description{
  Download a resource referenced by a Data Package.
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

