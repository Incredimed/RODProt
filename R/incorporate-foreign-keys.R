#' Factorize foreign keys
#' 
#' Identify the columns in the table which have been labeled as 
#' foreign keys and map them to their proper label based on the
#' underlying level.
#' @param table The JSON Table as read in from \link{read_json_table}.
#' @param schema The JSON Table Schema associated with this table.
#' @param name.column the name of the column to use as the levels of the factor. If the referenced
#' table has two of fewer columns, this value will be ignored. See Details for more information. 
#' @details This function aims to create factors from the columns identified as having a foreign
#' key in the provided schema. In order to do this in R, up to two columns will be involved: the
#' ID columns (as specified in the \code{foreignkey} attribute in the schema), and the names or
#' levels to be associated with these values. In the current implementation, these levels will be
#' converted to character vectors.
#' 
#' The behavior is as follows: if there is only one column in the referenced file and that column
#' matches the \code{ID} given for the foreignkey, that column will be used as both the IDs and 
#' the levels of the factor -- meaning that any values defined in this external table will map
#' to themselves, and any other values not in the table will map to \code{NA}. If the table has two
#' columns, one of which matches the \code{ID} value of the foreignkey, then the \code{ID} column
#' will be used for the IDs, and the second column, regardless of name, will be used as the levels
#' for the factor. If there are more than two columns, then the column to be used as the name must
#' be specified using the \code{name.column} parameter.
#' @importFrom rjson fromJSON
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
incorporate_foreign_keys <- function(table, schema, name.column="name"){	
	if (is.character(schema)){
		schema <- fromJSON(schema)
	}
	
	fks <- lapply(schema, "[[" , "foreignkey")

	factors <- lapply(fks, function(x){
		if (is.null(x)){
			return(NULL)
		}
		dataPkg <- read_data_package(x$pkg)		
		file <- get_file(dataPkg, x$file)
		
		if (!x$id %in% colnames(file)){
			stop("The ID specified in the foreignkey was not found in the file.")
		}
		
		#FIXME: duplicated levels in factors (even NAs) will throw an error eventually.
		
		if (ncol(file) == 1){
			#Only one column, map all values to themselves and any other values to NA.
			fac <- factor()
			levs <- rep(NA_character_, length=max(file[,1]))
			levs[as.integer(file[,1])] <- as.character(file[,1])			
			attr(fac, "levels") <- levs
			
			return(fac)
		}
		if(ncol(file) == 2){
			# Two columns, use the ID as the index and the other column as the levels.
			fac <- factor()
			levs <- rep(NA_character_, length=max(file[[x$id]]))
			levs[as.integer(file[[x$id]])] <- file[[which(colnames(file) != x$id)]]
			attr(fac, "levels") <- levs
						
			return(fac)
		}
		if (ncol(file) > 2){			
			browser()
			
			if (! name.column %in% colnames(file)){
				stop("The name.column value was not found in the file. See the documentation and Details section for how to correct this.")
			}
			fac <- factor()			
			levs <- rep(NA_character_, length=max(file[[x$id]]))
			levs[as.integer(file[[x$id]])] <- file[[name.column]]
			attr(fac, "levels") <- levs			
			
			return(fac)
		}		
	})
	names(factors) <- lapply(schema, "[[", "id")
	
	for (s in schema){
		if (!is.null(s$foreignkey)){						
			class(table[[s$id]]) <- "factor"
			#levels() <- strips out NA's. Have to hack to get this to work.
			attr(table[[s$id]], "levels") <- levels(factors[[s$id]])
		}
	}
	
	table
}