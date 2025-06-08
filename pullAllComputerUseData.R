library(tidyverse)
library(magrittr)
source("remapColumnNamesWithKeyValueTable.R")
dir.data <- "/home/haxor_elite/Desktop/censusExplorations/Data/computerTypesUsedACS_2017-2021_5Year/data/"

df.files <- list.files(dir.data,full.names = TRUE,recursive = TRUE) %>% as_tibble() %>% set_names("path.file") %>% 
  mutate(name.file = basename(path.file)) %>% 
  mutate(year = gsub(replacement = "",x = name.file,pattern = "^ACSST5Y") %>% str_sub(start = 1L,end = 4L) %>% as.integer()) %>%
  mutate(data = map(.x = path.file,.f = \(x) read_csv(file = x) %>% slice(-1L))) %>% 
  mutate(n.columns = map(.x = data,.f = \(x) ncol(x))) %>% unnest(n.columns) %>% 
  mutate(names.columns = map(.x = data,.f = \(x) names(x)))

df.md <- "/home/haxor_elite/Desktop/censusExplorations/Data/computerTypesUsedACS_2017-2021_5Year/metadata/ACSST5Y2021.S2801-Column-Metadata.csv" %>% 
  read_csv() %>% set_names(value = c("key","value"))

df.files %<>% mutate(data = map(.x = data,.f = \(x) remapColumnNamesWithKeyValueTable(tableToRemapNames = x,keyValueTable = df.md)))
df.files %<>% select(year,data) %>% unnest(data)

df.files %<>% select(-Geography) %>% separate_wider_delim(cols = `Geographic Area Name`,delim = ", ",names = c("county","state"))
df.files %>% mutate(across(.cols = matches("^Margin of Error"),.fns = \(x) as.double(x)))

#df.files %>% pivot_longer(cols = 4:128,names_to = "datafield",values_to = "value") %>% separate_wider_delim(cols = datafield,delim = "!!",names_sep = ".")
df.files %<>% pivot_longer(cols = 4:128,names_to = "datafield",values_to = "value") %>% separate_wider_delim(cols = datafield,delim = "!!",names_sep = ".",too_few = "align_start")