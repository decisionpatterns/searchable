#' searchable 
#' 
#' Make objects more searchable by allowing matching to names based on case
#' (in)sensitivity, regular expressions, etc. 
#' 
#' @references 
#'   \url{http://stackoverflow.com/questions/5671719/case-insensitive-search-of-a-list-in-r} \cr
#'   
#'   
#' @examples
#'   v <- c( a=1, B=2, c=3 )
#'   l <- list( a=1, B=2, c=3 )
#'   
#'   # MAKE 
#'   searchable( x, ignore.case )
#'   x[[ "b" ]]     # 2 
#'   
#' @docType pacakge
#' @name searchable-package

NULL
