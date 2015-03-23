#' Invert a structure by swapping keys and values
#' 
#' @param x object to invert
#' 
#' @details 
#' Inverts named vectors 
#' 
#' @return 
#'   A character vector in which the names are the former valeus.
#' 
#' @examples 
#'  v <- 1:26
#'  names(v) <- letters 
#' 
#'  invert(v)
#'   
#'  l <- as.list(v)
#' 
#' @note
#'   - currently applies to atomic vectors only
#'   - apply to list (recursive structures), data.frames, matrices and arrays 
#'   - invert might be an ambiguous name ... call it swap_kv? kvswap? swapKV? swapNV?
#'   
#' @export

setGeneric( "invert", function(x) standardGeneric( "invert" ) )

setMethod( "invert", "vector", 
  function(x) {
    
    if( is.null( names(x) ) ) stop( "vector does not have names.")
    
    v <- names(x)
    names(v) <- as.character(x)
    
    return(v)
    
  }
)


# SEARCHABLE
  setMethod( "invert", "searchable", 
  
    function(x) { 
      x@.Data <- invert( x@.Data )
      x
    }
    
  )

# LISTS
  setMethod( "invert", "list", 
    function(x) {
      stop( "Inverting lists is not supported yet")
    }  
  )
