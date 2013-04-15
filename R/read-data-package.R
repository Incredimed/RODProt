#' Read in a data package.
#' 
#' @param content The Data Package to parse. This can either be a URL,
#' a path to a local file, or a character string containing the actual
#' JSON describing the data package. See \url{http://www.dataprotocols.org/en/latest/data-packages.html}.
#' @param base The base URL or directory for all references to data.
#' This will be ignored if the \code{content} provided is either a URL
#' or a local file. Only if \code{content} is a character string will
#' this be used.
#' @param getter The \link{Getter} to use when retrieving the specified data. By
#' default, the getter will be inferred based on the structure of the 
#' \code{content} parameter. Alternatively, you can explicitly set one here, or 
#' even provide a custom getter.
#' @importFrom rjson fromJSON
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom digest digest
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @export
read_data_package <- function(content, base, getter){
	raw <- FALSE
	if (missing(getter)){
		if (file.exists(content)){
			getter <- LocalGetter$new()			
		} else if (tolower(substr(content,0, 4)) == "http"){
			getter <- HTTPGetter$new()			
		} else{
	  	#must be raw JSON
	  	getter <- RawGetter$new()
	  	raw <- TRUE
	  }
	} 
	
  json <- fromJSON(getter$get(content))
	
	#if (getter$className == "RawGetter"){ TODO: Figure out how to compare classes
	if (raw){
		if (missing(base)){
			warning("You didn't specify a base directory and provided the package content directly, so we won't know how to find any files referenced within this Data Package. Consider setting the 'base' variable or providing a file/URL reference instead of the JSON itself.")
			base <- NULL
		} else{
			base <- base
		}		
	} else{
		base <- dirname(content)		
	}
	
	json$base <- base
  class(json) <- c("dataPackage", class(json))
	
	json$hash <- digest(json, algo="sha256")
	
  json
}