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
			 
	 		if (nchar(uri) > 1000){
	 			warning(paste("Some server+client combinations don't support URLs over ",
	 										"1,000 characters. The request you made contains ", 
	 										nchar(uri), " characters. If this request fails, try ",
	 										"breaking this request into multiple smaller requests.",
	 										sep =""))
	 		}
	 		
	 		content(GET(uri), as="text")
	 	}
	)
)
