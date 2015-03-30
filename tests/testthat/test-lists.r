library(testthat)
library(searchable)
library(magrittr)

l <- list( ay=1, bee=1:2, cee=1:3, aitch=1:8, aitch=-(1:8) )


context('list-default')
  sl <- searchable(l)

  # $
  sl$ee          %>% expect_null        # MISS 
  sl$ay          %>% expect_equal(1)    # SINGLE HIT
  sl$aitch       %>% expect_equal(1:8)  # FIRST OF TWO HITS  
  

  sl[['ee']]     %>% expect_null        # MISS 
  sl[['bee']]    %>% expect_equal(1:2)  # SINGLE HIT
  sl[['aitch']]  %>% expect_equal(1:8)  # FIRST OF TWO HITS
  

  sl['ee']       %T>% expect_is('list')  %>%  unlist %>% expect_null
  sl['cee']      %>% expect_equivalent( l[3] )# SINGLE HIT
  
  sl[ c('ay','bee','dee') ]  %>% expect_equivalent( l[ c('ay','bee','dee') ] ) 



context('list-fixed')
  sl <- searchable(l, fixed )

  # sl$MISS       %>% expect_null        # MISS: FAILS  
  sl$ay        %>% expect_equal(1)
               

  sl[['bee']]  %>% expect_equal(1:2)  
                   expect_error( sl[['ee']] )
  
  sl['cee']    %>% expect_equivalent( l[3] )
  sl['ee']     %>% expect_equivalent( l[2:3] )

