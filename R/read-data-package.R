#' Read in a data package.
#' 
#' @param content The Data Package to parse. This can either be a URL,
#' a path to a local file, or a character string containing the actual
#' JSON describing the data package. See \url{http://www.dataprotocols.org/en/latest/data-packages.html}.
#' @param base The base URL or directory for all references to data.
#' This will be ignored if the \code{content} provided is either a URL
#' or a local file. Only if \code{content} is a character string will
#' this be used.
#' @importFrom rjson fromJSON
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom digest digest
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @export
read_data_package <- function(content, base){
	if (file.exists(content)){
		#open local file
		json <- fromJSON(file=content)
		base <- dirname(content)
	} else if (tolower(substr(content,0, 4)) == "http"){
		#download remote file
		json <- fromJSON(content(GET(content)))
		base <- dirname(content)
		json$url <- content
	} else{
  	#must be raw JSON
  	json <- fromJSON(content)
  	if (missing(base)){
  		warning("You didn't specify a base directory and provided the package content directly, so we won't know how to find any files referenced within this Data Package. Consider setting the 'base' variable or providing a file/URL reference instead of the JSON itself.")
  		base <- NULL
  	}  	
  }
  
	json$base <- base
  class(json) <- c("dataPackage", class(json))
	
	json$hash <- digest(json, algo="sha256")
	
  json
}