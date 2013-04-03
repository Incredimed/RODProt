#' Write a table in the JSON Table Schema format.
#' 
#' @param data The data.frame to convert to JSON Table Schema format.
#' @param connection The connection to which to write, or a character
#' string naming the file to which the function will write.
#' @param named if \code{TRUE}, will use named hashes in the table.
#' If \code{FALSE} (default), it will just create a mixed-type array.
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
  
  #for some reason, getting some leading whitespace that needs to be trimmed
  if (!named){
  	dataList <- lapply(apply(data, 1, function(x){list(unname(sub("^\\s+", "", x)))}), "[[", 1)
  } else{
  	dataList <- lapply(apply(data, 1, function(x){list(sub("^\\s+", "", x))}), "[[", 1)
  }
    
  writeLines(toJSON(list(fields=schema, data=dataList)), connection)
}