#' Generic class to retrieve objects
#' 
#' Define a getter class to specify how objects should be retrieved.
#' @alises Getter-class
#' @exportClass Getter
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
Getter <- setRefClass("Getter", 
	methods = list(
		initialize = function(...){
			initFields(...)
		},
		get = function(uri){
			
		}
	)
)