library(tidyverse)
library(magrittr)
select <- dplyr::select
df <- read_csv("Data/computerTypesUsedACS2022_5Year/ACSST5Y2022.S2801-Data.csv",skip = 1L)
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
df %<>% mutate(across(.cols = matches("^moe"),.fns = as.double))
df %<>% mutate(across(.cols = matches("^percent\\."),.fns = as.double))
#df %<>% mutate(across(.cols = c(n.TotalHouseholds:last_col(),.fns = \(x) as.integer(x))))
df %<>% select(county,state,matches("^n\\.|^moeAbsolute"))

#df.n %<>% separate_wider_delim(cols = var,delim = ".",names =  c("varname","category","subcategory"))


df.n <- df %>% select(c(county,state,matches("^n"))) %>% pivot_longer(cols = matches("^n"),names_to = "var",values_to = "n")
#df.n %<>% mutate(var = gsub(pattern = "TYPES OF COMPUTER\\.Has one or more types of computing devices:",x = var,replacement = "ownsOneDevice"))
df.n %<>% mutate(var = gsub(pattern = "^n\\.",replacement = "",x = var)) %>% mutate(var = gsub(pattern = "^TotalHouseholds\\.",x = var,replacement = ""))
df.n %<>% mutate(varType = "n",.before = var)

df.moe <- df %>% select(c(county,state,matches("^moe"))) %>% pivot_longer(cols = matches("^moe"),names_to = "var",values_to = "moe")
df.moe %<>% mutate(var = gsub(pattern = "TYPES OF COMPUTER\\.Has one or more types of computing devices:",x = var,replacement = "ownsOneDevice"))

# df.n %<>% mutate(noSemiColon = grepl(pattern = ":.",x = var)) %>% mutate(var = ifelse(test = noSemiColon == FALSE,yes = paste0(var,".TotalPopulation"),no = var)) %>% select(-noSemiColon)
# df.n %<>% mutate(endsWithSemiColon = grepl(pattern = ":$",x = var)) %>% mutate(var = ifelse(test = endsWithSemiColon,yes = paste0(var,".At least one computing device"),no = var)) %>% select(-endsWithSemiColon)
#df.n %<>% mutate(var = gsub(pattern = "\\.TYPES OF COMPUTER\\.Has one or more types of computing devices:",replacement= "",x = var))
df %<>% full_join(x = df.n,y = df.moe,by = c("county","state","var"))

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
