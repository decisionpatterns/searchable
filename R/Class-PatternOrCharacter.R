#' @include Class-Pattern.R

NULL 

# Class: PatternOrCharacter 
#
# The PatternOrCharacter class is necessary in order to allow a search to accept
# either a Pattern Or Character. See ?extract.

  setClassUnion( 'PatternOrCharacter', c('Pattern','character'))
