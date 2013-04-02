#' Write a table in the JSON Table Schema format.
#' 
#' @param data The data.frame to convert to JSON Table Schema format.
#' @param connection The connection to which to write, or a character
#' string naming the file to which the function will write.
#' @importFrom rjson toJSON
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @export
write_json_table <- function(data, connection=stdout(), named=FALSE){
  if (is.character(connection)) {
    connection <- file(connection, "w")
    on.exit(close(connection))
  }
  
  schema <- list()
  for (i in 1:ncol(data)){
    type <- switch(class(data[[i]]),
                   "integer" = "integer",
                   "numeric" = "number",
                   "character" = "string",
                   "logical" = "boolean"
    )
    
    schema[[i]] <- list(id=colnames(data)[i],
                        type=type)
  }
  
  #hack together JSON manually so we can use the write.table function to handle 
  # when an element should be quoted. Since we can't have a mixed-type array in R,
  # there's not a way to blend the types without it getting convered to a list by
  # toJSON().
  writeLines('{"fields":', connection, sep="")
  writeLines(toJSON(schema), connection, sep="")
  writeLines(',"data":[', connection, sep="")  
  writeLines("[", connection, sep="")
  write.table(data[1:(nrow(data)-1),], file=connection, col.names=FALSE, row.names=FALSE, eol="],[", sep=",")
  write.table(data[nrow(data),], file=connection, col.names=FALSE, row.names=FALSE, eol="]", sep=",")
  writeLines("]}\n", connection, sep="")
}