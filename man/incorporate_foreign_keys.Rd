\name{incorporate_foreign_keys}
\alias{incorporate_foreign_keys}
\title{Factorize foreign keys}
\usage{
  incorporate_foreign_keys(table, schema,
    name.columns = "name")
}
\arguments{
  \item{table}{The JSON Table as read in from
  \link{read_json_table}.}

  \item{schema}{The JSON Table Schema associated with this
  table.}

  \item{name.columns}{the name of the column to use as the
  levels of the factor. If the referenced table has two of
  fewer columns, this value will be ignored. See Details
  for more information.}
}
\description{
  Identify the columns in the table which have been labeled
  as foreign keys and map them to their proper label based
  on the underlying level.
}
\details{
  This function aims to create factors from the columns
  identified as having a foreign key in the provided
  schema. In order to do this in R, up to two columns will
  be involved: the ID columns (as specified in the
  \code{foreignkey} attribute in the schema), and the names
  or levels to be associated with these values. In the
  current implementation, these levels will be converted to
  character vectors.

  The behavior is as follows: if there is only one column
  in the referenced file and that column matches the
  \code{ID} given for the foreignkey, that column will be
  used as both the IDs and the levels of the factor --
  meaning that any values defined in this external table
  will map to themselves, and any other values not in the
  table will map to \code{NA}. If the table has two
  columns, one of which matches the \code{ID} value of the
  foreignkey, then the \code{ID} column will be used for
  the IDs, and the second column, regardless of name, will
  be used as the levels for the factor. If there are more
  than two columns, then the column to be used as the name
  must be specified using the \code{name.column} parameter.
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

