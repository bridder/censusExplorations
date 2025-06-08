remapColumnNamesWithKeyValueTable <- function(tableToRemapNames,keyValueTable)
{
  require(tidyverse)
  require(magrittr)
  require(assertthat)
  require(assertr)
  
  assert_that(is_tibble(tableToRemapNames),msg = "tableToRemapNames is not a tibble.")
  assert_that(is_tibble(keyValueTable),msg = "tableToRemapNames is not a tibble.")
  assert_that(nrow(keyValueTable) >= 1L,msg = "keyValueTable has zero rows.")
  assert_that(keyValueTable %>% verify(has_only_names(c("key","value")),success_fun = success_logical),msg = "keyValueTable must have only the names 'key' and 'value'.")
  #assert_that(identical(x = keyValueTable %>% pull(key),y = tableToRemapNames %>% names()),msg = "The elements of 'key' must be an identical vector to the names of tableToRemapNames.")
  
  names(tableToRemapNames)[match(keyValueTable$key, names(tableToRemapNames))] <- keyValueTable$value
  return(tableToRemapNames)
}
