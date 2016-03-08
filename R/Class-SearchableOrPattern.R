
#' @include Class-Searchable.R 
#' @include Class-Pattern.R

NULL 

# Class: SearchableOrPattern
#  
# This is a virtual class used internally to specify an object as either 
# Searchable or Pattern. This allows 
#
# @name SearchableOrPattern 
# 
# @include Class-Searchable.R 
# @include Class-Pattern.R


setClassUnion( 'SearchableOrPattern', c('Searchable', 'Pattern') )
