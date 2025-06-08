library(skimr)
library(dlookr)
library(magrittr)
library(tidyverse)
library(assertthat)
library(assertr)
library(readxl)

df.dict <- read_xlsx(path = "Data/VIUS_2021/vius-2021-puf-data-dictionary.xlsx",skip = 3L)
List1 <- df.dict %>% select(Field,Type) %>% distinct %>% mutate(col_type = case_when(Type == "CHAR" ~ "c",Type == "NUM" ~ "c")) %>% select(-Type)
column_types <- List1 %>% pull(col_type) %>% as.list() %>% set_names(List1 %>% pull(Field))

df <- read_csv("Data/VIUS_2021/vius_2021_puf.csv",col_types = column_types)

df.PL <- df %>% pivot_longer(cols = c(TABWEIGHT:last_col()),names_to = "Field",values_to = "value")
df.dict %<>% rename("value" = "Valid Values",
                    "description" = "...4") %>% 
  select(Field,value,description)

df.PL %<>% left_join(y = df.dict,by = c("Field","value"))
df <- df.PL %>% pivot_wider(names_from = Field,values_from = c(value,description))

df %>% select(value_TABWEIGHT,value_REGSTATE,description_REGSTATE,value_BUSRELATED,description_BUSRELATED) %>% 
  group_by(value_REGSTATE,description_REGSTATE,value_BUSRELATED,description_BUSRELATED) %>% 
  summarize(w = sum(as.double(value_TABWEIGHT))) %>%
  ungroup() %>% 
  group_by(value_REGSTATE,description_REGSTATE) %>% 
  mutate(p = w/sum(w)) %>%
  ungroup() %>% 
  select(-value_REGSTATE,-value_BUSRELATED,-w) %>% 
  pivot_wider(names_from = "description_BUSRELATED",
              values_from = "p") %>% 
  set_names("state","someCommercialActivity","personalUseOnly","vehicleNotInUse","notReported") %>% 
  arrange(desc(personalUseOnly)) %>% 
  pivot_longer(values_to = "f",cols = someCommercialActivity:notReported) %>% 
  mutate(state = as.factor(state),name = as.factor(name)) %>% 
  arrange(name) %>% 
  group_by(name) %>% 
  summarize(q2.5pct = quantile(x = f,probs = 0.025),
            q50.0pct = quantile(x = f,probs = 0.50),
            q95.0pct = quantile(x = f,probs = 0.95),
            q97.5pct = quantile(x = f,probs = 0.975),
  )
