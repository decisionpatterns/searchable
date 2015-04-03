#' detect 
#'
#' which elements matching the search pattern 
#'
#' @param str seachable target
#' @param pattern method for searching 
#'
#' @return logical; vector indicating which elements match
#'
#' @seealso 
#'   .matches
#'   
#' @note 
#'   - may not export this function
#' @export 

detect  <- function(target, pattern) { 

  type <- if( ! pattern %>% is('pattern') ) "standard" else pattern@type
  
  switch( type
    , regex    = stri_detect_regex( str, pattern@.Data, opts_regex = stri_opts_regex( pattern@options ) )      
    , fixed    = stri_detect_fixed( str, pattern@.Data, opts_fixed = stri_opts_fixed( pattern@options ) )    
    , coll     = stri_detect_coll(  str, pattern@.Data, opts_coll  = stri_opts_coll(  pattern@options ) )
    , standard = stri_detect_standard( str, pattern@.Data, opts_standard = stri_opts_std stri_opts_standard stop('A pattern should have been specified by now.')
    , stop( "Unknown search type : ", type )
  )

}

