library(testthat)
library(searchable)
library(magrittr)

l <- list( ay=1, bee=1:2, cee=1:3 )


context('list:unmodified search')
  sl <- searchable(l)

  sl$ay        %>% expect_equal(1)
                   expect_null( sl$ee )
  
  sl[['bee']]  %>% expect_equal(1:2)  
                   expect_null( sl[['ee']] )
  
  sl['cee']    %>% expect_equivalent( l[3] )
  sl['ee']     %>% extract2(1)  %>% is.null %>% expect_true
  sl[ c('ay','bee','dee') ]  %>% expect_equivalent( sl[ c('ay','bee','dee') ] ) 


context('lists:fixed search')
  sl <- searchable(l, fixed )

  sl$ay        %>% expect_equal(1)
                   expect_error( sl$ee )
  
  sl[['bee']]  %>% expect_equal(1:2)  
                   expect_error( sl[['ee']] )
  
  sl['cee']    %>% expect_equivalent( l[3] )
  sl['ee']     %>% expect_equivalent( l[2:3] )

