#' searchable 
#' 
#' The 'searchable' package provides flexibile methods of accessing objects with 
#' names using case (in)sensitivity, regular or fixed expressions, and boundary
#' matching. It applies the most common textual searches to allow the 
#' construction of sophisticated dictionaries and thesauruses. 
#' 
#' @references 
#'   \url{http://stackoverflow.com/questions/5671719/case-insensitive-search-of-a-list-in-r} \cr
#'   \url{http://stackoverflow.com/questions/27085620/which-command-in-r-with-case-insensitive} \cr
#'   \url{http://stackoverflow.com/questions/21450925/r-grep-usage-finding-the-averages} \cr
#'   
#' @seealso
#'   \code{\link{searchable}} \cr
#'   \url{http://cran.r-project.org/web/packages/qdap}
#'   
#' @examples
#'   
#'   # ATOMIC VECTORS 
#'     v <- c( ay=1, bee=2, cee=3 )
#'     sv <- searchable( v, case_insensitive = TRUE )
#'     
#'     sv$BEE 
#'     
#'     sv[[ "b" ]]                # 2 
#'     sv[[ "B" ]]                # 2 
#'     sv[[ ignore.case('BEE') ]] # 2
#'     
#'     sv[[ fixed('b') ]]         # 2 
#'     
#'     
#'     sv[ 'bee' ]                       
#'     sv[ 'ee' ]                 #  
#'     sv[ regex('[A|B]') ]        # 1,2  
#'      
#'   # RECURSIVE LISTS 
#'   
#'     l <- list( ay=c(1), bee=c(2), cee=3 )
#'     sl <- searchable(l, ignore.case )
#'
#'     sl$BEE                     # 1 2   
#'     
#'     sl[[ "b" ]]                # 1 2 
#'     sl[[ "B" ]]                # 1 2 
#'     sl[[ ignore.case('BEE') ]] # 1 2
#'     
#'     sl[[ fixed('b') ]]         # 1 2 
#'     
#'     sl[ 'bee' ]                       
#'     sl[ 'ee' ]                 # 1  1 2 
#'     sl[ regex('[a|b]') ]        # 1  1 2  
#'    
#'    
#' @docType package
#' @name searchable-package
#' @import magrittr methods stringi

NULL
