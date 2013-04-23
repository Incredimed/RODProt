#' Convert data type names
#' 
#' Convert the type names used in JSON Table Schema to their R equivalents.
#' @param type The JSON Table Schema type name.
#' @return The R type name.
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @export
cast_type <- function(type){	
	as.character(sapply(c(type), cast_single))	
}

cast_single <- function(type){	
	switch(type,
				 "integer" = "integer",
				 "number" = "numeric",
				 "string" = "character",
				 "boolean" = "logical"
	)	
}
