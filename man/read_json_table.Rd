\name{read_json_table}
\alias{read_json_table}
\title{A function to read in a JSON Table Schema}
\usage{
  read_json_table(content, schema)
}
\arguments{
  \item{content}{The content to convert. This can either be
  a String of JSON, a local filename, or a URL. You can
  provide the schema and data in a single message (with the
  schema as the first JSON element, and the data as the
  second), or you can use the \code{schema} paramter to
  specify the schema and provide only the data here.}

  \item{schema}{Optionally, you can provide a separate
  schema for the message contained in the \code{content}
  parameter.}
}
\description{
  A function to read in a JSON Table Schema
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

