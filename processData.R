library(tidyverse)
library(magrittr)

df <- "/media/haxor_elite/ExtraDrive1/computerCensusData/UQTypeOwnership/inputSamples/6_9_2025/outputResultsSamples" %>% 
  list.files(recursive = TRUE,full.names = TRUE) %>% 
  lapply(FUN = read_rds) %>% 
  bind_rows()

df %<>% group_by(county,state) %>% mutate(groupID = cur_group_id()) %>% ungroup()

reconciledCounts <- df %>% filter(groupID == 3L) %>% pull(x.solution) %>% bind_rows


df.reconciledPivotLonger <- df %>% mutate(x.solution = map(.x = x.solution,
                               .f = \(x) as_tibble_row(x))) %>% 
  select(state,county,sampleIndex,x.solution) %>% 
  unnest(x.solution) %>% 
  pivot_longer(cols = c(n.0000:n.1111),names_to = "reconciledEstimate",values_to = "n")

keyTable <- read_csv("SupportData/computerOwnershipKeyTable.csv",col_types = "cc")

df.plot <- df.reconciledPivotLonger %>% filter(state == "Indiana",county == "White County")

df.plot %<>% left_join(y = keyTable,by = join_by("reconciledEstimate" == "value"))
df.plot %<>% select(-reconciledEstimate)

p1 <- ggplot(data = df.plot,mapping = aes(x = n,y = key))
p1 <- p1 + theme_bw()
p1 <- p1 + geom_boxplot()
p1 <- p1 + scale_x_log10() + annotation_logticks(sides = "b")
p1 <- p1 + ggtitle(df.plot %>% distinct(state,county) %>% unlist() %>% paste0(collapse = " - "))
print(p1)

df.reconciledPivotLonger %>% 
  mutate(ownsNone = reconciledEstimate == "n.0000") %>% 
  group_by(state,county,sampleIndex,ownsNone) %>% 
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = ownsNone,values_from = n,names_prefix = "n.") %>% 
  mutate(p.ownsNone = n.TRUE/(n.TRUE + n.FALSE)) %>% 
  select(state,county,sampleIndex,p.ownsNone)


df.reconciledPivotLonger %>% 
  group_by(state,sampleIndex,reconciledEstimate) %>% 
  summarize(n= sum(n)) %>% 
  mutate(ownsNone = reconciledEstimate == "n.0000") %>% 
  group_by(state,sampleIndex,ownsNone) %>% 
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = ownsNone,values_from = n,names_prefix = "n.") %>% 
  mutate(p.ownsNone = n.TRUE/(n.TRUE + n.FALSE)) %>% 
  select(state,sampleIndex,p.ownsNone)
