#' A function to read in a JSON Table Schema
#' 
#' @param content The content to convert. This can either be a String of JSON,
#' a local filename, or a URL. You can provide the schema and data in a 
#' single message (with the schema as the first JSON element, and the data
#' as the second), or you can use the \code{schema} paramter to specify the
#' schema and provide only the data here.
#' @param schema Optionally, you can provide a separate schema for the 
#' message contained in the \code{content} parameter.
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @export
read_json_table <- function(content, schema){	
	if (class(content) == "list"){
		#assume it's already a parsed JSON file.
		#TODO: test
		json <- content
	}	else{
		if (file.exists(content) || 
					tolower(substr(content,0, 7)) == "http://"){
			#Assume it's a local file and parse accordingly.
			json <- fromJSON(file=content)
		} else{
			json <- fromJSON(content)
		}	
	}
	
	schema <- json[[1]]
	data <- json[[2]]
	
	#to ultimately be converted into the data.frame to return
	table <- list()
	
	#map of IDs to column indices
	idMap <- numeric(length=length(schema))
	
	type <- character(length=length(schema))
	
	#setup data.frame
	for (i in 1:length(schema)){
		thisSchema <- schema[[i]]
				
		if (!is.null(thisSchema$type)){
			type[i] <- switch(thisSchema$type,
										 "integer" = "integer",
										 "number" = "numeric",
										 "string" = "character"
			)
			if (is.null(col)){
				#non-defined class, use character
				type[i] <- "character"
			}
		} else{
			type[i] <- "character"
		}		
						
		col <- get(type[i])(length=length(data))
		
		thisCol <- list()
		if (!is.null(thisSchema$label)){
			thisCol[[thisSchema$label]] <- col
		} else{
			thisCol[[thisSchema$id]] <- col
		}
				
		table <- c(table, thisCol)
		idMap[i] <- thisSchema$id
	}
	table <- as.data.frame(table, stringsAsFactors=FALSE)
	
	#separate data into named and mixed-array type rows
	namedInd <- sapply(data, class) == "list" & !sapply(data, function(x){is.null(names(x))})
	mixed <- data[!namedInd]
	named <- data[namedInd]
	
	#process mixed-aray columns which have no names
	if (length(unique(sapply(mixed, length))) > 1){		
		stop("All mixed-array rows must have the same number of columns")
	}
	
	mixLen <- length(mixed)
	mixed <- unlist(mixed)
	if (mixLen > 0){
		for (i in 1:ncol(table)){			
			table[1:mixLen,i] <- get(paste("as",type[i],sep="."))(mixed[(0:(mixLen-1))*ncol(table)+i])			
		}	
	}
	
	#process named elements
	#match(,idMap)
	
	
	table
}