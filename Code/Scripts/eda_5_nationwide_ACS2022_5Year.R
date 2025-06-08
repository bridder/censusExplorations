source("Code/Scripts/eda_4_nationwide_ACS2022_5Year_MOE.R")
source("Code/Scripts/eda_4_nationwide_ACS2022_5Year_N.R")
source("Code/Scripts/rnorm_positive.R")

df.n %<>% pivot_longer(cols = c(n.total:last_col()),names_to = "name",values_to = "value") %>% mutate(name = gsub(pattern = "^n\\.",replacement = "",x = name)) %>% rename("category" = "name")
df.moe %<>% pivot_longer(cols = c(moe.total:last_col()),names_to = "name",values_to = "value") %>% mutate(name = gsub(pattern = "^moe\\.",replacement = "",x = name)) %>% rename("category" = "name")
df.n %<>% add_column(variable = "n")
df.moe %<>% add_column(variable = "moe")

df <- bind_rows(df.n,df.moe)

df %<>% pivot_wider(names_from = variable,values_from = value) %>% mutate(n = as.integer(n))
