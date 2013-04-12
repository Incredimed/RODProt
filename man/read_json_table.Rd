\name{read_json_table}
\alias{read_json_table}
\title{A function to read in a JSON Table Schema}
\usage{
  read_json_table(content, schema, overlook.types = FALSE,
    factorize.foreign.keys = TRUE, ...)
}
\arguments{
  \item{content}{The content to convert. This can either be
  a String of JSON, a local filename, or a URL. You can
  provide the schema and data in a single message (with
  keys named \code{fields} and \code{data}, respectively),
  or you can use the \code{schema} paramter to specify the
  schema and provide only the data here (with no prefacing
  \code{data:\{} attribute).}

  \item{schema}{Optionally, you can provide a separate
  schema for the message contained in the \code{content}
  parameter. This can be either the JSON itself, a local
  file reference, or a URL.}

  \item{overlook.types}{If TRUE, any unrecognized or
  non-supported type will just be treated as a character
  vector. Otherwise, the function will terminate upon
  encountering a non-supported type. The currently
  supported types are: (\code{boolean}, \code{string},
  \code{integer}, and \code{number}).}

  \item{factorize.foreign.keys}{When \code{TRUE} (default),
  any column leveraging the (non-standardized, at the time
  of writing -- see
  \url{https://github.com/dataprotocols/dataprotocols/issues/23})
  foreign-key functionality of JSON Table Schemas will be
  handled as a fator, mapping the underlying key to the
  presented form. Currently, this only works with foreign
  keys that map integers to strings.}

  \item{...}{Additional arugments to be passed to
  \code{incorporate-foreign-keys} (unexported, but publicly
  documented in this package).}
}
\description{
  A function to read in a JSON Table Schema
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

