library(tidyverse)
library(magrittr)

df <- "/media/haxor_elite/ExtraDrive1/computerCensusData/UQTypeOwnership/inputSamples/6_9_2025/outputResultsSamples" %>% 
  list.files(recursive = TRUE,full.names = TRUE) %>% 
  lapply(FUN = read_rds) %>% 
  bind_rows()

df %<>% group_by(county,state) %>% mutate(groupID = cur_group_id()) %>% ungroup()


df %>% 
  select(county:n.ownsNoComputers) %>% 
  pivot_longer(cols = c(nhat.total:last_col()),
               names_to = "variable",
               values_to = "count") %>% 
  mutate()

df.plotresidual <- df %>% 
  select(county:n.ownsNoComputers) %>% 
  pivot_longer(cols = c(nhat.total:last_col()),
               names_to = "variable",values_to = "count") %>% 
  mutate(variable = gsub(pattern = "^n?\\.",x = variable,replacement = "n_")) %>% 
  mutate(variable = gsub(pattern = "^nhat?\\.",x = variable,replacement = "nhat_")) %>% 
  separate_wider_delim(cols = variable,delim = "_",names = c("variable","computingDeviceType")) %>% 
  pivot_wider(names_from = variable,values_from = count) %>% 
  mutate(residual = n - nhat)


df.plotresidual.filtered <- df.plotresidual %>% filter(state == "Alabama",county == "Jefferson County")


p1 <- ggplot(data = df.plotresidual.filtered,mapping = aes(x = residual,y = computingDeviceType))
p1 <- p1 + theme_bw()
p1 <- p1 + geom_boxplot()
#p1 <- p1 + scale_x_log10() + annotation_logticks(sides = "b")
p1 <- p1 + ggtitle(df.plotresidual.filtered %>% distinct(state,county) %>% unlist() %>% paste0(collapse = " - "))
print(p1)

df.plotresidual.filtered %>% 
  group_by(computingDeviceType) %>% 
  summarize(n.2.5percent = quantile(residual,probs = 0.025),
            n.50percent = median(residual),
            n.97.5percent = quantile(residual,probs = 0.975))

df.plotresidual.filtered %>% 
  group_by(computingDeviceType) %>% 
  summarize(nhat_2.5pct = quantile(nhat,probs = 0.025),
            nhat_50pct = median(nhat),
            nhat_97.5pct = quantile(nhat,probs = 0.975),
            n_2.5pct = quantile(n,probs = 0.025),
            n_50pct = median(n),
            n_97.5pct = quantile(n,probs = 0.975))


# df %>% 
#   select(county:n.ownsNoComputers) %>% 
#   pivot_longer(cols = c(nhat.total:last_col()),
#                names_to = "variable",values_to = "count") %>% 
#   filter(county == "Autauga County")
df.plot2 <- df.plotresidual.filtered %>% 
  select(-residual)%>% 
  pivot_longer(cols = c(nhat,n),values_to = "count",names_to = "type")


p1 <- ggplot(data = df.plot2,mapping = aes(x = count,y = computingDeviceType,fill = type))
p1 <- p1 + theme_bw()
p1 <- p1 + geom_boxplot(linewidth = 0.2)
p1 <- p1 + scale_x_log10() + annotation_logticks(sides = "b")
p1 <- p1 + ggtitle(df.plot2 %>% distinct(state,county) %>% unlist() %>% paste0(collapse = " - "))
print(p1)


df.plotresidual2d <- df.plotresidual %>% filter(computingDeviceType == "smartphone") %>% filter(county == "Jefferson County",state == "Alabama")
p1 <- ggplot(data = df.plotresidual2d,mapping = aes(x = n,y = nhat))
p1 <- p1 + theme_bw()
p1 <- p1 + ggtitle(df.plotresidual2d %>% distinct(state,county) %>% unlist() %>% paste0(collapse = " - "))

p1 <- p1 + geom_point(size = 0.3)
p1 <- p1 + theme(aspect.ratio = 1.0)
p1 <- p1 + geom_abline(slope = 1,intercept = 0,color = "red",linewidth = 3,alpha = 0.2)
print(p1)

