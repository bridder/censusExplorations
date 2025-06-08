library(tidyverse)
library(magrittr)
select <- dplyr::select
df <- read_csv("Data/computerTypesUsedACS2023_1Year/ACSST1Y2023.S2801-Data.csv",skip = 1L)
#df %>% skim %>% print
df <- df[,1:length(df) - 1]
df %<>% select(-Geography)
df %<>% separate_wider_delim(cols = `Geographic Area Name`,delim = ",",names = c("county","state")) %>% mutate(state = trimws(state))
df %<>% rename_with(.fn = \(x) gsub(pattern = "!!",replacement = ".",x = x),.cols = everything())
df %<>% rename_with(.fn = \(x) gsub(pattern = "^Estimate\\.Total\\.",replacement = "n.",x = x),.cols = everything())
df %<>% rename_with(.fn = \(x) gsub(pattern = "^Estimate\\.Percent\\.",replacement = "percent.",x = x),.cols = everything())

df %<>% rename_with(.fn = \(x) gsub(pattern = "^Margin of Error\\.Total\\.",replacement = "moeAbsolute.",x = x),.cols = everything())
df %<>% rename_with(.fn = \(x) gsub(pattern = "^Margin of Error\\.Percent\\.",replacement = "moePercent.",x = x),.cols = everything())
df %<>% rename_with(.fn = \(x) gsub(pattern = "Total households",replacement = "TotalHouseholds",x = x),.cols = everything())
df %<>% mutate(across(.cols = matches("^n\\."),.fns = as.integer))
df %<>% mutate(across(.cols = matches("^moe\\."),.fns = as.double))
df %<>% mutate(across(.cols = matches("^percent\\."),.fns = as.double))

df.n <- df[,1:50] %>% select(county,state,matches("^n\\."))
df.n %<>% rename_with(.fn = \(x) gsub(pattern = "\\.TotalHouseholds\\.",replacement = ".",x = x),.cols = everything())
df.n %<>% select(county,state,matches("\\.TYPES OF COMPUTER\\."))
df.n %<>% rename_with(.fn = \(x) gsub(pattern = "\\.Has one or more types of computing devices:",replacement = "",x = x),.cols = everything())
df.n %<>% rename_with(.fn = \(x) gsub(pattern = "TYPES OF COMPUTER",replacement = "",x = x),.cols = everything())
df.n %<>% rename_with(.fn = \(x) gsub(pattern = " with no other type of computing device",replacement = ".only",x = x),.cols = everything())
df.n %<>% rename_with(.fn = \(x) gsub(pattern = "Desktop or laptop",replacement = "DTLT",x = x),.cols = everything())
df.n %<>% rename_with(.fn = \(x) gsub(pattern = "\\.{2,}",replacement = ".",x = x),.cols = everything())

df.n %<>% rename_with(.fn = \(x) gsub(pattern = "DTLT\\.DTLT\\.",replacement = "DTLT.",x = x),.cols = everything())
df.n %<>% rename_with(.fn = \(x) gsub(pattern = "Smartphone\\.Smartphone\\.",replacement = "Smartphone.",x = x),.cols = everything())
df.n %<>% rename_with(.fn = \(x) gsub(pattern = "Tablet or other portable wireless computer\\.Tablet or other portable wireless computer\\.",replacement = "Tablet or other portable wireless computer.",x = x),.cols = everything())
df.n %<>% rename_with(.fn = \(x) gsub(pattern = "Other computer\\.Other computer\\.",replacement = "Other computer.",x = x),.cols = everything())
df.n %<>% rename_with(.fn = \(x) gsub(pattern = "Tablet or other portable wireless computer",replacement = "tablet",x = x),.cols = everything())
df.n %<>% rename_with(.fn = \(x) gsub(pattern = "Other computer",replacement = "other",x = x),.cols = everything())
df.n %<>% rename_with(.fn = \(x) tolower(x),.cols = everything())


df.n %<>% rename_with(.fn = \(x) gsub(pattern = "no computer",replacement = "ownsNoComputers",x = x),.cols = everything())

df.n %<>% relocate(matches("\\.only$"),.after = "state")
df.n %<>% rename("n.total" = "n.")
df.n %<>% relocate(n.total,.after = state)

df.n %>% pivot_longer(cols = c(n.total:n.ownsNoComputers),names_to = "unknownVariable",values_to = "n") %>% group_by(state,county) %>% mutate(p = n/sum(n)) %>% mutate(p = paste0(100*round(p,3L),"%")) %>% View

df.p.acrossCounties <- df.n %>% mutate(across(.cols = c(n.total:n.ownsNoComputers),.fns = \(x) x/sum(x))) %>% rename_with(.cols = c(n.total:n.ownsNoComputers),.fn = \(x) gsub(pattern = "^n",x = x,replacement = "p") %>% paste0(".acrossCounties"))
