\name{write_json_table}
\alias{write_json_table}
\title{Write a table in the JSON Table Schema format.}
\usage{
  write_json_table(data, connection = stdout(),
    named = FALSE)
}
\arguments{
  \item{data}{The data.frame to convert to JSON Table
  Schema format.}

  \item{connection}{The connection to which to write, or a
  character string naming the file to which the function
  will write.}

  \item{named}{if \code{TRUE}, will use named hashes in the
  table. If \code{FALSE} (default), it will just create a
  mixed-type array.}
}
\description{
  Write a table in the JSON Table Schema format.
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

