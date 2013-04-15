#' Local file getter
#' 
#' Getter which accesses local files
#' @export
#' @exportClass LocalGetter
#' @aliases LocalGetter-class
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
LocalGetter <- setRefClass("LocalGetter",
	methods = list(
	 	initialize = function(...){
	 		initFields(...)
	 	},
	 	get = function(uri){
			con <- file(uri, "r")
			lines <- paste(readLines(con), collapse="\n")
			close(con)
			lines
	 	}
	)
)
