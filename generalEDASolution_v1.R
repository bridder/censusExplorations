library(tidyverse)
library(magrittr)
select <- dplyr::select
df <- read_csv("Data/computerTypesUsedACS2022_5Year/ACSST5Y2022.S2801-Data.csv",skip = 1L)
df <- df[,1:length(df) - 1]
df %<>% select(-Geography)

df %<>% separate_wider_delim(cols = `Geographic Area Name`,delim = ",",names = c("county","state")) %>% mutate(state = trimws(state))
df %<>% rename_with(.fn = \(x) gsub(pattern = "!!",replacement = "_",x = x),.cols = everything())

df.Estimate <- df %>% select(state,county,matches("^Estimate_Total"))
