#' Class: Searchables
#'
#' Inherits from list and vector
#'
#' @name Searchables
#' @exportClass Searchables
#' @rdname Searchables

  setClassUnion( 'Searchables', c('list', 'vector') )
