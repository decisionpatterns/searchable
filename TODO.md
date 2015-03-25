TODO:
 
# IMMEDIATE 

 - Supporting multiple patterns for [ and [<- 
 
   Currently these operators support only one pattern, unlike there base 
   counterparts. Thus this is an error:
   
     sv[ c('ay','bee','dee') ]
 
   In order to be more like base R, it might be better to support multiple 
   patterns. In general, the problems is that the pattern can match more than 
   one elements. This would mean that either, 
     - the returned object would be a different length than the patterns. 
     - Or, a list would be returned. Breaking the rule that [ is a slice 
       operator
     
   The alternative something ugly like:      
     c('ay','bee','dee')  %>% sapply( function(x) sv[x] )  %>% unlist( use.names = FALSE )
     
     - Use a return-modifier that ensures a single return value.
     
 
 - Support data.frames 
   
   This should be straight-foward and implement a method of 
   seachable-ANY-character that dispatches to data.frame-ANY-character after 
   appropriate resolutions.

 - Support data.tables
   
    This might be tricky as j is interpreted within x. There might not be a 
    good way to do this unless with = FALSE.   
 
 - What is the behavior of $, $<- ?
 
   - Option 1: Not overridden, behaves as base R. This is rationale because $
     does not generally interpret the argument.
     
   - Option 2: As [[, and interprets 
     There is no way to directly add to the structure if the 
     Since it is object$name there can be no modifiers applied to name.    
   
   - $<- ?
     If there is no
 
 
 - Can there be perl and ignore.case together? OR fixed and ignore.case? 
 
 - Refactor to 
   - match.modifiers: 
       x fixed, partial 
       x perl (regex)
       x ignore.case 
       x exact

   - search.modifiers: 
       - reverse.lookup - look in values  
       - recursive (unlist)

   - multiple matches
       - any 
       - all
       - first, last, second, third, nth(string,n)        
       
   - return.modifeirs: 
       - key, value
       x searchable (returns a searchable object): pipe result to searchable

   - setting modifiers:
       - uniqueness
       - count 

 - Better modifier collision in extract.R/.collect.modifiers 
    - ? match modifiers should be resolved without necessarily affecting search etc.  
    - It's possible to apply the modifiers differently to searchable vs nt searchable
      objects
    
    Any modifiers to search pattern override the target. 
    
    
# LONG TERM 
 - remove dependency on stringr in favor of stringi?
    
 - Determine if we need subclasses: 
   - searchable.atomic and searchable.recursive
     There are no classes for atomic and recursive(?)
   - searchable.vector, searchable.list, searchable.hash


# COMPLETED 
X Should be able to apply modifiers to target object:
  v %>% searchable %>% ignore.case  
  


# INTERFACE OPTIONS
-------------------------------------------------

## OPTION 1:

Follow closely stringr and use modifiers to change attributes, but have a lot of
slots.

PROs: Existing interface
CONs: many attributes, non-expandable, resolve at match time, object validity checking


## OPTION 2: 

Existing @modifiers list, overload fixed, perl, ignore,case (modifier) functions

PROs: easily extensible, write additional call-backs
CONs: conflict at load, though these are compatible with stringr

Rewrite fixed, perl and ignore.case to resolve match modification at call time.
Github( stringr#60 ).

- Can warn on incompatible modifiers, within the functions
- How to find incompatibles within list of modifiers? 
  - Digest?
  X Don't care...these are quick functions. 


## OPTION 3: 

Hybrid approach. 
- Refactor searchable with the appropriate slots.


## OPTION 3:

  Expand to match, search and return modifiers. Match follows closely stringr, searc

*This can be implemented later*
