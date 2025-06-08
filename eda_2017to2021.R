library(tidyverse)
library(magrittr)

df.2018 <- read_csv("Data/computerTypesUsedACS_2017-2021_5Year/ACSST5Y2017.S2801-Data.csv",skip = 1L) %>% add_column(dataset.year = 2018L,yearSpan.ACS = 5L)
df.2019 <- read_csv("Data/computerTypesUsedACS_2017-2021_5Year/ACSST5Y2019.S2801-Data.csv",skip = 1L) %>% add_column(dataset.year = 2019L,yearSpan.ACS = 5L)
df.2020 <- read_csv("Data/computerTypesUsedACS_2017-2021_5Year/ACSST5Y2020.S2801-Data.csv",skip = 1L) %>% add_column(dataset.year = 2020L,yearSpan.ACS = 5L)
df.2021 <- read_csv("Data/computerTypesUsedACS_2017-2021_5Year/ACSST5Y2021.S2801-Data.csv",skip = 1L) %>% add_column(dataset.year = 2021L,yearSpan.ACS = 5L)

df <- bind_rows(df.2018,df.2019,df.2020,df.2021)

df %>% group_by(dataset.year) %>% summarize(numCounties = n_distinct(`Geographic Area Name`)) %>% print()

df %<>% select(-Geography)
df %<>% separate_wider_delim(cols = `Geographic Area Name`,delim = ",",names = c("county","state")) %>% mutate(state = trimws(state))
df %>% group_by(state,dataset.year) %>% summarize(numCounties = n_distinct(county)) %>% pivot_wider(names_from = dataset.year,values_from = numCounties,names_prefix = "nCounty.") %>% ungroup()

#Alaska is the one with a new county quantity.
df %>% group_by(state,dataset.year) %>% summarize(numCounties = n_distinct(county)) %>% summarize(gap = max(numCounties) - min(numCounties)) %>% arrange(desc(gap)) %>% print()
