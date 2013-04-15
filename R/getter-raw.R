#' Raw/Passthrough Getter
#' 
#' Getter which passes the input text specified in as output
#' @importFrom httr GET
#' @export
#' @exportClass RawGetter
#' @aliases RawGetter-class
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
RawGetter <- setRefClass("RawGetter",
	methods = list(
	 	initialize = function(...){
	 		initFields(...)
	 	},
	 	get = function(uri){
			 uri
	 	}
	)
)
