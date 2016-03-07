# searchable

[![CRAN](http://www.r-pkg.org/badges/version/searchable)](https://cran.rstudio.com/web/packages/searchable/index.html) 
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html) 
[![Downloads](http://cranlogs.r-pkg.org/badges/lettercase?color=brightgreen)](http://www.r-pkg.org/pkg/searchable)
 
 
Have you ever needed more flexibility in retrieving list and vector elements than exact name matches? Have you wanted to search ignoring case or subset the elements using a regular expression? Have you wanted this to be the default behavior? The `searchable` package is your answer.  

The *searchable* package provides flexibile methods for searching and subsetting objects by matching names using case (in)sensitivity, regular expressions, fixed expressions or other user defined patterns.  This is accomplished by overloading the standard `[` operator to allow the `string[r|i]` style match modifiers. These can be applied to the target list|vector|objects. When no match modifiers are used, the default is standard R behavior. So this is a drop-in replacement.  

`searchable` was designed to be extensible. Developers can use `searchable` to build thier own flexible, high performance dictionary and thesaurus data structures or their own match modifiers


## Features

Features of this package are:

* Easy to make name look-ups searchable -- simply by declaring objects searchable.
* Drop-in functionality -- the `[` operator is overload but defaults to base-R functionality. 
* stringr/i*-style match modifiers for matching by case (in)sensitivity, regular expressions or fixed expression. 
* Match modification applied to pattern and/or target
** Define default behavior for target (dictionary)
** Overridable, per-search change of search behavior


## Installation

Stable Version:

    install.packages('searchable')
    library(devtools)
    
    
Development Version: 

    library(devtools)
    install_github( "decisionpatterns/searchable" )
    

## Examples

```R
  library(searchable)
  library(magrittr)
 
  # ATOMIC VECTORS: 
    v <- c( a=1, b=2, B=3, c=4, c2=5 )
    sv <- searchable(v)
    
  # EXTRACT:
    sv$a
     
    sv[['a']]
    sv[[ ignore.case('A') ]]
    
    sv[ ignore.case('b') ]     
    sv[ perl('c') ]
    sv[ fixed('c') ]
           
                                      
  # REPLACEMENT: 
    sv$a               <- "first" 
    sv[['a']]          <- "1st"  
    sv[[ perl('c.') ]] <- "third"
    
    sv[ perl('c.?') ]   <- "3rd"
  
  
  # MODIFIERS TO SEARCH TARGET/OBJECT
    sv <- searchable(v, ignore.case )         
    sv$A
    sv['b']
    sv['B']
  
  
  # RECURSIVE LISTS:
    l <- list( a=1, b=2, c=3 )
    sl <- searchable(l)                
    sl[["b"]]
    sl[[ ignore.case("B") ]] 
    
  # USE WITH MAGRITTR   

    sl[ "B"  %>% ignore.case ]
    "b" %>% sl[[.]]
    "B" %>% ignore.case %>% sl[[ . ]]
```
