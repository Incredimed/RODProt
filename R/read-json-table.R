#' A function to read in a JSON Table Schema
#' 
#' @param content The content to convert. This can either be a String of JSON,
#' a local filename, or a URL. You can provide the schema and data in a 
#' single message (with keys named \code{fields} and \code{data}, respectively), 
#' or you can use the \code{schema} paramter to specify the schema and provide 
#' only the data here (with no prefacing \code{data:\{} attribute).
#' @param schema Optionally, you can provide a separate schema for the 
#' message contained in the \code{content} parameter. This can be either the JSON
#' itself, a local file reference, or a URL.
#' @param overlook.types If TRUE, any unrecognized or non-supported type will 
#' just be treated as a character vector. Otherwise, the function will terminate
#' upon encountering a non-supported type. The currently supported types are: 
#' (\code{boolean}, \code{string}, \code{integer}, and \code{number}).
#' @param factorize.foreign.keys When \code{TRUE} (default), any column leveraging
#' the (non-standardized, at the time of writing -- see 
#' \url{https://github.com/dataprotocols/dataprotocols/issues/23}) foreign-key 
#' functionality of JSON Table Schemas will be handled as a fator, mapping the
#' underlying key to the presented form. Currently, this only works with foreign
#' keys that map integers to strings.
#' @param ... Additional arugments to be passed to \code{incorporate-foreign-keys}
#' (unexported, but publicly documented in this package).
#' @importFrom rjson fromJSON
#' @importFrom httr GET
#' @importFrom httr content
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @export
read_json_table <- function(content, 
														schema, 
														overlook.types=FALSE, 
														factorize.foreign.keys=TRUE, ...){	
	if (missing(content)){
		content <- list()
	}
	
	if (class(content) == "list"){
		#assume it's already a parsed JSON file.
		#TODO: test
		json <- content
	}	else{
		if (file.exists(content)){					
			#Assume it's a local file and parse accordingly.
			json <- fromJSON(file=content)
		} else if (tolower(substr(content,0, 4)) == "http"){
		  json <- fromJSON(content(GET(content)))
    }else{
			json <- fromJSON(content)
		}	
	}
	
	if (missing(schema)){
		schema <- json$fields
		data <- json$data
	} else{		
		if (class(schema) == "list"){
			#already parsed
		} else if (file.exists(schema)){
			#Assume it's a local file and parse accordingly.
			schema <- fromJSON(file=schema)
		} else if (tolower(substr(schema,0, 4)) == "http"){
		  schema <- fromJSON(content(GET(schema)))
    } else{
			schema <- fromJSON(schema)
		}	
		
		#allow (encourage) schema to nest fields element in its JSON
		if (!is.null(schema$fields)){
			schema <- schema$fields
		}
				
		data <- json		
	}
	
	#allow (discourage) data to nest field under name of 'data'.
	if (!is.null(names(data))){
		if (names(data) == "data"){
			data <- data$data
		} else{
			stop("data parameter should not have named elements.")
		}
	}
	
	#stop if named elements in schema. Sign of malformed JSON.
	if (!is.null(names(schema))){
		stop("JSON Table Schema should not have named elements in its fields.")		
	}
	
	
	
	#to ultimately be converted into the data.frame to return
	table <- list()
	
	if (!is.null(schema)){
		#map of IDs to column indices
		idMap <- numeric(length=length(schema))	
	} else{
		#calculate IDs dynamically.
		nullNames <- unlist(lapply(data, function(x){is.null(names(x))}))
		if (sum(nullNames) > 0 && !all(nullNames)){
			stop("You can't mix the mixed-array format with the named list format in a schema-less JSON table. We don't know which column is which!")
		}
		if (!all(nullNames)){
			idMap <- unique(unlist(lapply(data, names)))
		} else{
			idMap <- paste("C", 1:length(data[[1]]), sep="")
		}
	}
	
	type <- character(length=length(idMap))
	
	#setup data.frame
	for (i in 1:length(idMap)){
		if(!is.null(schema)){
			#if we have a schema, parse it.
			thisSchema <- schema[[i]]
				
			if (!is.null(thisSchema$type)){
				thisType <- switch(thisSchema$type,
	 							 				   "integer" = "integer",
												   "number" = "numeric",
												   "string" = "character",
													 "boolean" = "logical"
				)			
				if (is.null(thisType)){
					if (!overlook.types){
						#non-defined class, stop
						stop(paste("The class specified ('", thisSchema$type, "') is not supported. Set ",
											 "'overlook.types' to TRUE to ignore this error.", sep=""))
					} else{
						type[i] <- "character"
					}
					
				} else{
					type[i] <- thisType
				}
			} else{
				type[i] <- "character"
			}		
		}
		else{
			#no schema was provided, need to build ourselves from the idMap
			thisSchema <- list(id=idMap[i])
			type[i] <- "character"
		}
		
		col <- get(type[i])(length=length(data))
		
		thisCol <- list()
		#TODO: do something with column labels. Perhaps extend the data.frame class (preface 
		# our objects with some new class name which prints using labels, but actually indexes
		# using IDs.
		thisCol[[thisSchema$id]] <- col
						
		table <- c(table, thisCol)
		idMap[i] <- thisSchema$id
	}
	
	table <- as.data.frame(table, stringsAsFactors=FALSE)
	
	#separate data into named and mixed-array type rows
	namedInd <- sapply(data, class) == "list" & !sapply(data, function(x){is.null(names(x))})
	mixed <- data[!namedInd]
	named <- data[namedInd]
	
	#process mixed-aray columns which have no names
	mixLen <- length(mixed)	
	if (mixLen > 0){		
		colCounts <- unique(sapply(mixed, length))
		if (length(colCounts) > 1){		
			stop("All mixed-array rows must have the same number of columns")
		}		
		if (colCounts != length(idMap)){
			stop("Number of columns in data doesn't match the number of columns specified in the schema.")
		}
				
		mixed <- unlist(mixed)
		for (i in 1:ncol(table)){			
			table[!namedInd,i] <- get(paste("as",type[i],sep="."))(mixed[(0:(mixLen-1))*ncol(table)+i])			
		}	
	}
	
	#process named elements
	#Thanks to Josh O'Brien and the others in the discussion at:
	# http://stackoverflow.com/questions/15753091/
	if (length(named) > 0){		
		nonSchemad <- !sapply(lapply(named, names), function(x){all(x %in% idMap)})
		
		if (sum(nonSchemad) > 0){
			warning(paste("One of the following named elements were not specified in the schema and will",
							"be ignored: ", (paste(unlist(lapply(named, names)[nonSchemad]),collapse=",")), sep=""))
		}
		
		mat <- t(vapply(named, 
									FUN = function(X) as.character(unlist(X)[idMap]), 
									FUN.VALUE = character(length(idMap))))	
		mat <- as.data.frame(mat, stringsAsFactors=FALSE)
		for (i in 1:ncol(mat)){
			mat[,i] <- get(paste("as", type[i], sep="."))(mat[,i])
		}
		
		table[namedInd,] <- mat
	}
	
	if (factorize.foreign.keys){				
		table <- incorporate_foreign_keys(table, schema, ...)
	}
	table
}