#' searchable 
#' 
#' Make R objects more searchable by allowing search of names based on case
#' (in)sensitivity, regular expressions, fixed patterns, exact matches, etc. 
#' The package facilitates making and searching dictionaries, 
#' 
#' @references 
#'   \url{http://stackoverflow.com/questions/5671719/case-insensitive-search-of-a-list-in-r} \cr
#'   
#' @seealso
#'   \code{\link{searchable}}
#'   
#' @examples
#'   
#'   # Vectors 
#'     v <- c( ay=1, bee=2, cee=3 )
#'     sv <- searchable( v, modifiers = list(ignore.case) )
#'     
#'     sv$BEE 
#'     
#'     sv[[ "b" ]]                # 2 
#'     sv[[ "B" ]]                # 2 
#'     sv[[ ignore.case('BEE') ]] # 2
#'     
#'     sv[[ fixed('b') ]]         # 2 with warning
#'     
#'     
#'     sv[ 'bee' ]                       
#'     sv[ 'ee' ]                 #  
#'     sv[ perl('[A|B]') ]        # 1,2  
#'      
#'   # LISTS 
#'     l <- list( a=1, B=2, c=3 )
#'     sl <- searchable(l)
#
#'   
#' @docType package
#' @name searchable-package
#' @include Class-searchable.R

NULL
