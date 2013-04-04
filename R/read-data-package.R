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
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
#' @export
read_data_package <- function(content, base){
	if (file.exists(content)){
		#open local file
		json <- fromJSON(file=content)
	} else if (tolower(substr(content,0, 4)) == "http"){
		#download remote file
		json <- content(GET(content))
	} else{
  	#must be raw JSON
  	json <- fromJSON(content)
  }
  
  class(json) <- c("dataPackage", class(json))
  json
}