#' HTTP-based getter
#' 
#' Getter which uses the \code{httr} package to retrieve objects.
#' @importFrom httr GET
#' @export
#' @exportClass HTTPGetter
#' @aliases HTTPGetter-class
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
HTTPGetter <- setRefClass("HTTPGetter",
	methods = list(
	 	initialize = function(...){
	 		initFields(...)
	 	},
	 	get = function(uri){
			 content(GET(uri), as="text")
	 	}
	)
)
