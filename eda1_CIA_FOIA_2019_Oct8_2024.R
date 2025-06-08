library(tidyverse)
library(magrittr)
library(lubridate)
df <- read_csv("Data/CIA_FOIA_2019/CIA_FY2019_FOIA_Annual_Report_Raw_Data.csv",col_types = list(`Request Number` = "c"))

df <- df[,colSums(is.na(df))<nrow(df)]

df %<>% select(-`Is Consultation`)
df %<>% select(-`Component/Office`)
df %<>% select(-`Exemption 3 Statutes`)
df %<>% select(-`Exemption(s) Applied`)
df %<>% select(-`Appeal Exemption(s) Applied`)
df %<>% select(-c(`Exp. Date Received`,`Exp. Date of Determination`,`Exp. Granted/Denied`,`Fee Date Received`,`Fee Date of Determination`,`Fee Granted/Denied`))
df %<>% select(-`Amount of Fees Collected`)

df %<>% rename("id.number" = "Request Number")
df %<>% rename("date.initialRequestReceived" = "Dates Initially Received")
df %<>% rename("date.initialRequestComplete" = "Completed")
df %<>% rename("initialRequest.type" = "Track")
df %<>% rename("initialRequest.outcome" = "Disposition")
df %<>% rename("initialRequest.outcomeReason" = "Disposition Reasons")
df %<>% rename("date.appealReceived" = "Appeal Date Received")
df %<>% rename("date.appealComplete" = "Date Closed")
df %<>% rename("appeal.outcome" = "Appeal Disposition")
df %<>% rename("appeal.outcomeReason" = "Appeal Reasons")

df %<>% mutate(across(.cols = matches("^date"),.fn = \(x) mdy(x)))

df %>% 
  drop_na(id.number,date.initialRequestReceived,date.initialRequestComplete) %>% 
  mutate(duration.initialRequest = date.initialRequestComplete - date.initialRequestReceived) %>% 
  select(id.number,date.initialRequestReceived,date.initialRequestComplete,duration.initialRequest,initialRequest.outcomeReason) %>% 
  mutate(duration.initialRequest = duration.initialRequest/dyears(1)) %>% 
  arrange(desc(duration.initialRequest)) %>% 
  mutate(months = (duration.initialRequest - floor(duration.initialRequest))*12) %>% mutate(duration.initialRequest.years = duration.initialRequest %>% floor) %>% mutate(duration.initialRequest.months = floor(months)) %>% 
  select(-duration.initialRequest,-months) %>% 
  mutate(across(.cols = c(duration.initialRequest.years,duration.initialRequest.months),.fns = \(x) as.integer(x))) %>% 
  drop_na(initialRequest.outcomeReason)
