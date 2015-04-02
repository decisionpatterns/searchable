# List of active modifiers, exact/default is not a match modifier, it resets ther others
# .match.modifiers <- c( 'ignore.case', 'fixed', 'perl' ) #, 'exact', 'default', 'partial' #
.match.modifiers <- c( 'ignore.case', 'fixed', 'perl', 'exact', 'default', 'partial', 'case.sensitive', 'case.insensitive', 'use.case' )

.partials <- c('full','partials' ) 


# "x" %>% ignore.case %>% .is.modified
# "x" %>% regex %>% .is.modified 
# "x" %>% ignore.case %>% regex %>% .is.modified 

# use pattern@type == 'standard' instead
# .is.modified <- function(object) {
#   
#   ! is.null( attr(object,'type') ) &&
#   object %>% type %in% c('fixed','regex','coll')         ||
#     
#   object %>% attributes %>% is.null %>% magrittr::not &&
#   object %>% attributes %>% names %in% c('case.insensitive', 'boundary', 'reverse.lookup')  
#     
# }

# .are.modified <- function(...) {
#   
#   li <- list(...)
#   li %>% sapply( .is.modified ) %>% all
#     
# }

# .is.unmodified <- function(object) ! .is.modified(object)
# 
# .are.unmodified <- function(...) {
#   
#   li <- list(...)
#   li %>% sapply( .is.unmodified ) %>% all
#     
# }


# Apply modifiers 
#  
# Copy Attributes from object to pattern if pattern is unmodified and the 
# target is modified
# 
# example 
#  .collect.modifiers( "n"  %>% ignore.case, "x" ) %>% str

# .collect.modifiers <- function(target,pattern) { 
# 
#   if( .is.modified(target)  && ! .is.modified(pattern) )
#     attributes(pattern) <- attributes(target)
#   
#   return(pattern)  
# 
# }
# 



# # Turn OFF attribute by toggling attribute to NULL if TRUE  
# .off <- function( object, which ) { 
# 
#   for( wh in which ) { 
#     
#     attr <- object  %>% attr(wh)
#     if( ! attr  %>% is.null && attr  %>% is.logical && attr )
#       attr(object,wh) <- NULL
#       
#   }
#   
#   return(object)
# }
# 
# 
# # Toggle ON attribute 
# .on <- function(object, which ) { 
#   
#   for( wh in which ) { 
#     attr <- object  %>% attr(wh)
#     if( ! attr  %>% is.null && ! attr  %>% is.logical ) warning( wh, " is exists and is not logical") 
#     attr(object, wh) <- TRUE
#   }
#   return(object)
# }
# 
# 
# 
# .is.on <- function(object, which)
#   ! object %>% attr(which) %>% is.null && object %>% attr(which)
#   
