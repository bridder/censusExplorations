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

#Brakes by state
df.brakeMechanismEDA <- df %>% group_by(value_REGSTATE,value_BRAKES,description_BRAKES) %>% 
  filter(value_BRAKES != "X") %>% 
  summarize(w = sum(value_TABWEIGHT %>% as.double)) %>% 
  ungroup() %>% 
  group_by(value_REGSTATE) %>% 
  mutate(f = w/sum(w)) %>% 
  ungroup() %>% 
  select(value_REGSTATE,description_BRAKES,f) %>% 
  pivot_wider(names_from = description_BRAKES,values_from = f) %>% 
  mutate(across(.cols = 2:last_col(),.fns = \(x) ifelse(test = is.na(x),yes = 0.0,no = x))) %>% 
  set_names("State","hydraulic.standard","hydraulic.powerAssist","air","other","notReported")

df.brakeMechanismEDA %>% mutate(across(.cols = 2:last_col(),.fns = \(x) paste0(100*round(x,digits = 4L),"%"))) %>% View

df.singleUnitBodyTypeEDA <- df %>% 
  select(value_TABWEIGHT,value_REGSTATE,value_BTYPE,description_BTYPE) %>% 
  filter(value_BTYPE != "X") %>% 
  group_by(value_BTYPE,description_BTYPE) %>% 
  summarize(w = sum(as.double(value_TABWEIGHT))) %>% 
  ungroup() %>% 
  mutate(f = w/sum(w)) %>% 
  arrange(desc(f)) %>% 
  select(-w,-value_BTYPE) %>% 
  set_names("bodyType.SingleUnitVehicle","fraction")

df.businessRelatedEDA <- df %>% group_by(value_REGSTATE,value_BUSRELATED,description_BUSRELATED) %>% 
  summarize(w = sum(value_TABWEIGHT %>% as.double))%>% group_by(value_REGSTATE)%>% mutate(f = w/sum(w))%>% select(-value_BUSRELATED,-w)%>% ungroup()%>% pivot_wider(names_from = description_BUSRELATED,values_from = f)%>% arrange(`Some commercial activity`)
df.brakeMechanismEDA



df.acquireYearEDA <- df %>% 
  group_by(value_REGSTATE,value_ACQUIREYEAR,description_ACQUIREYEAR) %>% 
  summarize(w = sum(value_TABWEIGHT %>% as.double)) %>% 
  group_by(value_REGSTATE) %>% 
  mutate(p = w/sum(w)) %>% 
  select(-w,-value_ACQUIREYEAR) %>% 
  pivot_wider(names_from = description_ACQUIREYEAR,values_from = p,names_prefix = "yearAcq.") %>% 
  ungroup() %>% 
  mutate(across(.cols = 2:last_col(),
                .fns = \(x) paste0(round(x,digits = 3L),"%")))


df.acquireYearMostPluralityYear <- df %>% filter(value_ACQUIREYEAR != "Z")%>% 
  group_by(value_REGSTATE,value_ACQUIREYEAR,description_ACQUIREYEAR) %>% 
  summarize(w = sum(value_TABWEIGHT %>% as.double)) %>% 
  group_by(value_REGSTATE)%>% mutate(p = w/sum(w))%>% slice_max(order_by = p,n = 1L)%>% select(-value_ACQUIREYEAR,-w)%>% ungroup()%>% arrange(desc(p)) %>% mutate(p = paste0(round(100*p,digits = 1L),"%"))%>% View



# 
# 
# fieldList <- df.dict %>% group_by(Field) %>% group_split()
# 
# fieldList[[1]] %>% distinct()
# 
# df %<>% select(ID,BTYPE,TABWEIGHT,AVGWEIGHT,CYLINDERS,DC_ADAPCRUISE,DC_LANEASST,DC_PLATOON,DC_VTVCOMM,FE_HYBDRIVENP,FE_HYBDRIVEPL,FE_LRRTIRES,FE_NOSECONE,FUELTYPE,FUELWHERE,HBTYPE,KINDOFBUS,MILESANNL,MILESLIFE,MODELYEAR,MPG,NUMGEARS,OD_AHIGHBEAM,OD_BACKUPCAM,OD_HUD,REGSTATE,ST_ABS,ST_AIRBAG,ST_CRUISE,ST_DRIVERCAM,ST_GPS,ST_GPSNAV,ST_INTERNET,ST_ROLLOVER,TOTLENGTH,TRANSMISSION)
# 
# join_VIUS <- function(.data,factorName)
# {
#   df.join <- df.dict %>% filter(Field == factorName) %>% select(`Valid Values`,`...4`) %>% set_names(factorName,paste0(factorName,".value"))
#   
#   return(.data %>% left_join(y = df.join,by = factorName) %>% relocate(paste0(factorName,".value"),.after = factorName))
# }
# 
# 
# df.dict %>% filter(Field == "BTYPE") %>% select(-Field,-Description,-Type,-Length,-`Applicable Vehicles`) %>% set_names("BTYPE","BTYPE.value")
# 
# left_join(x = df %>% select(ID,TABWEIGHT,BTYPE,CYLINDERS,AVGWEIGHT),
#           y = df.dict %>% filter(Field == "BTYPE") %>% select(-Field,-Description,-Type,-Length,-`Applicable Vehicles`) %>% set_names("BTYPE","BTYPE.value"),
#           by = "BTYPE") %>% 
#   filter(BTYPE != "X") %>% 
#   arrange(BTYPE.value) %>% filter(CYLINDERS != "Oth") %>% 
#   left_join(y = df.dict %>% filter(Field == "AVGWEIGHT") %>% select(`Valid Values`,`...4`) %>% set_names("AVGWEIGHT","AVGWEIGHT.value"),
#             by = "AVGWEIGHT") %>% 
#   filter(AVGWEIGHT.value != "Not applicable (see 'Applicable Vehicles')") %>% 
#   filter(BTYPE.value == "Armored") %>% 
#   separate_wider_delim(cols = AVGWEIGHT.value,delim = " ",names = c("AVGWEIGHT.low","c1","AVGWEIGHT.high","c2")) %>% select(-c1,-c2) %>% 
#   mutate(AVGWEIGHT.low = as.double(AVGWEIGHT.low %>% gsub(pattern = ",",replacement = "",x = .)),
#          AVGWEIGHT.high = as.double(AVGWEIGHT.high %>% gsub(pattern = ",",replacement = "",x = .))) %>% 
#   arrange(AVGWEIGHT.low)
