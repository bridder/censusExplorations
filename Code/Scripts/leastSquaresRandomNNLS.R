library(tidyverse)
library(magrittr)
library(stringi)
library(pracma)
library(nnls)
set.seed(23L)
#Order for binary coding of cases is (desktop OR laptop) - (smartphone) - (tablet) - (other)
#For example, n.0000 is the case of owning none of those devices at all.
#n.1000 would be someone owns at least one (desktop OR laptop) and nothing else.
#n.0100 would be someone who owns at least 1 smartphone and nothing else.
#n.1110 would be someone who owns at least 1 (desktop OR laptop), at least 1 smartphone, at least 1 tablet, and zero devices in the "Other" category.

source("Code/Scripts/eda_5_nationwide_ACS2022_5Year.R")
#                     1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16
A <- matrix(data = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                     0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                     0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                     0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                     0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
                     0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                     0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L,
                     0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L,
                     0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L,
                     1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),ncol = 16L,byrow = TRUE)


nTrials <- 2L
outputFolder_outputResultsSamples <- "/media/haxor_elite/ExtraDrive1/computerCensusData/UQTypeOwnership/inputSamples/6_7_2025/outputResultsSamples/"
dir.create(path = outputFolder_outputResultsSamples,recursive = TRUE)
for(j in 1:nTrials)
{
  df.n <- df %>% 
    rowwise() %>%
    mutate(n.sample = rnorm_positive(mean = n,sd = moe)) %>% 
    ungroup() %>% 
    select(-n,-moe) %>% 
    rename("n" = "n.sample") %>% 
    pivot_wider(names_from = category,values_from = n,names_sep = ".") %>% 
    rename_with(.fn = \(x) paste0("n.",x),
                .cols = c(total:last_col()))
  
  N <- nrow(df.n)
  list.xSolution <- list()
  list.reconciledDf <- list()
  for(i in 1:N)
  {
    printString <- df.n %>% slice(i) %>% mutate(printString = paste0(county,", ",state)) %>% pull(printString)
    
    sprintf("i = %d, %s",i,printString) %>% print()
    b <- df.n %>% slice(i) %>% dplyr::select(c(n.total:n.ownsNoComputers)) %>% unlist() %>% as.matrix() %>% unname
    output.nnls <- nnls(A,b)

    x.solution <- output.nnls %>% extract2("x") %>% round() %>% as.integer()
    nameVector <- expand.grid(c(0,1),c(0,1),c(0,1),c(0,1)) %>% as_tibble() %>% mutate(nameVector = paste0(Var1,Var2,Var3,Var4)) %>% pull(nameVector) %>% stri_reverse() %>% paste0("n.",.)
    names(x.solution) <- nameVector
    reconciled.df <- df.n %>% slice(i)
    reconciled.df %<>% rename_with(.cols = c(n.total:n.ownsNoComputers),.fn = \(x) gsub(pattern = "^n",x = x,replacement = "nhat"))
    reconciled.df %<>% pivot_longer(cols = c(nhat.total:nhat.ownsNoComputers))
    reconciled.df %<>% mutate(value = (A %*% x.solution) %>% as.vector)
    reconciled.df %<>% add_column(sampleIndex = j) %>% relocate(sampleIndex,.after = state)
    reconciled.df %<>% pivot_wider(names_from = name,values_from = value)
    reconciled.df %<>% left_join(y = df.n,by = c("county","state"))
    reconciled.df %<>% add_column(solution.nnls = list(output.nnls))
    list.reconciledDf[[i]] <- reconciled.df
  }
  list.reconciledDf %<>% bind_rows()
  outputFileName <- sprintf("output_sampleIndex_%d.rds",j)
  write_rds(x = list.reconciledDf,file = paste0(outputFolder_outputResultsSamples,outputFileName))
}

#   
#   x1 <- df.n %>% bind_rows %>% pivot_longer(cols = c(n.total:n.ownsNoComputers)) %>% add_column(comparison = "originalData")
#   y1 <- list.reconciledDf %>% bind_rows %>% pivot_longer(cols = c(nhat.total:nhat.ownsNoComputers)) %>% mutate(name = gsub(pattern = "^nhat",replacement = "n",x = name)) %>% add_column(comparison = "reconciled")
#   
#   df.compare <- bind_rows(x1,y1)
#   
#   #left_join(x = df.n %>% slice(1L),y = reconciled.df,by = c("county","state")) %>% View
#   #plotDf <- list.xSolution  %>% bind_rows() %>% select(-convergenceFlag)  %>% filter(state != "Puerto Rico") %>% pivot_longer(cols = c(n.0000:n.1111)) %>% group_by(county,state) %>% mutate(p = value/sum(value)) %>% select(-value) %>% pivot_wider(names_from = name,values_from = p) %>% ungroup() %>% pivot_longer(cols = c(n.0000:n.1111))
#   
#   plotDf <- list.xSolution  %>% bind_rows() %>% filter(state != "Puerto Rico") %>% pivot_longer(cols = c(n.0000:n.1111)) %>% group_by(county,state) %>% mutate(p = value/sum(value)) %>% select(-value) %>% pivot_wider(names_from = name,values_from = p) %>% ungroup() %>% pivot_longer(cols = c(n.0000:n.1111))
#   
#   p1 <- ggplot(data = plotDf,mapping = aes(x = value,fill = name))
#   p1 <- p1 + theme_bw()
#   p1 <- p1 + geom_density(alpha = 0.3,linewidth = 0.2)
#   p1 <- p1 + scale_x_log10(limits = c(1e-6,1e-0),breaks = 10^seq(from = -6,to = 0,by = 1))
#   p1 <- p1 + annotation_logticks()
#   print(p1)
#   
#   plotDf2 <- list.xSolution  %>% bind_rows() %>% select(-convergenceFlag)  %>% filter(state != "Puerto Rico") %>% pivot_longer(cols = c(n.0000:n.1111)) %>% group_by(county,state) %>% mutate(p = value/sum(value)) %>% select(-value) %>% pivot_wider(names_from = name,values_from = p) %>% ungroup()
#   
#   p2 <- ggplot(data = plotDf2 %>% filter(n.1000 > 0,n.1110 > 0),mapping = aes(x = n.1000,y = n.1110))
#   p2 <- p2 + geom_point(size = 0.1) + theme_bw()
#   #p2 <- p2 + scale_x_log10(limits = c(10^-12,10^-0))
#   #p2 <- p2 + scale_y_log10(limits = c(10^-12,10^-0))
#   p2 <- p2 + scale_x_log10()
#   p2 <- p2 + scale_y_log10()
#   print(p2)
#   
#   
#   n.total.national <- list.xSolution  %>% bind_rows() %>% select(-convergenceFlag)  %>% filter(state != "Puerto Rico") %>% pivot_longer(cols = c(n.0000:n.1111)) %>% pull(value) %>% sum()
#   n.sp.national <- list.xSolution  %>% bind_rows() %>% select(-convergenceFlag)  %>% filter(state != "Puerto Rico") %>% mutate(n.sp = n.0100 + n.0101 + n.0110 + n.0111 + n.1100 + n.1101 + n.1110 + n.1111) %>% pull(n.sp) %>% sum()
#   p.sp.national <- n.sp.national/n.total.national
#   
#   
#   n.dtlt.national <- list.xSolution  %>% bind_rows() %>% select(-convergenceFlag)  %>% filter(state != "Puerto Rico") %>% mutate(x = n.1000 + n.1001 + n.1010 + n.1011 + n.1100 + n.1101 + n.1110 + n.1111) %>% pull(x) %>% sum()
#   p.dtlt.national <- n.dtlt.national/n.total.national
#   
#   n.anyDevice.national <- list.xSolution %>% 
#     bind_rows() %>% 
#     select(-convergenceFlag) %>% 
#     filter(state != "Puerto Rico") %>% 
#     mutate(x = n.0001 + n.0010 + n.0011 + n.0100 + n.0101 + n.0110 + n.0111 + n.1000 + n.1001 + n.1010 + n.1011 + n.1100 + n.1101 + n.1110 + n.1111) %>% 
#     pull(x) %>% 
#     sum()
#   
#   p.anyDevice.national <- n.anyDevice.national/n.total.national
#   
# }
