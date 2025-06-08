library(tidyverse)
library(magrittr)
library(stringi)
library(pracma)
#Order for binary coding of cases is (desktop OR laptop) - (smartphone) - (tablet) - (other)
#For example, n.0000 is the case of owning none of those devices at all.
#n.1000 would be someone owns at least one (desktop OR laptop) and nothing else.
#n.0100 would be someone who owns at least 1 smartphone and nothing else.
#n.1110 would be someone who owns at least 1 (desktop OR laptop), at least 1 smartphone, at least 1 tablet, and zero devices in the "Other" category.
#
#n.0000  1 owns no computing devices
#n.0001  2 owns some other kind of computing device only 
#n.0010  3 owns a tablet only
#n.0011  4
#n.0100  5 owns a smartphone only
#n.0101  6
#n.0110  7
#n.0111  8
#n.1000  9 owns a desktop or laptop only
#n.1001 10
#n.1010 11
#n.1011 12
#n.1100 13
#n.1101 14
#n.1110 15
#n.1111 16

#Equation row order is:
#n.total n.dtlt.only n.smartphone.only n.tablet.only n.other.only n.dtlt n.smartphone n.tablet n.other n.ownsNoComputers

source("Code/Scripts/eda_3_nationwide_ACS2022_5Year.R")
#                     1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16
A <- matrix(data = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, # n.total
                     0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, # n.dtlt.only
                     0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, # n.smartphone.only
                     0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, # n.tablet.only
                     0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, # n.other.only
                     0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, # n.dtlt
                     0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, # n.smartphone
                     0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L, # n.tablet
                     0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, # n.other
                     1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),# n.ownsNoComputers
            ncol = 16L,byrow = TRUE)

N <- nrow(df.n)
list.xSolution <- list()
list.reconciledDf <- list()

#initialGuessFractions <- c(0.040,0.000,0.003,0.000,0.133,0.000,0.033,0.000,0.000,0.009,0.000,0.000,0.182,0.000,0.600,0.000)
initialGuessFractions <- dim(A) %>% extract2(2L) %>% raise_to_power(-1) %>% rep(dim(A) %>% extract2(2L))

for(i in 1:N)
{
  printString <- df.n %>% slice(i) %>% mutate(printString = paste0(county,", ",state)) %>% pull(printString)
  
  sprintf("i = %d, %s",i,printString) %>% print()
  b <- df.n %>% slice(i) %>% dplyr::select(c(n.total:n.ownsNoComputers)) %>% unlist() %>% as.matrix() %>% unname
  
  #x0.calculated <- (df.n %>% slice(i) %>% pull(n.total))*initialGuessFractions
  
  #x.solution <- lsqnonneg(C = A,d = as.vector(b)) %>% extract2("x") %>% round() %>% as.integer()
  
  costFunction <- function(x)
  {
    n.total <- df.n %>% slice(i) %>% pull(n.total)
    n.apportioned <- n.total*x
    #print(n.apportioned)
    #print(x)
    print(A %*% n.apportioned - b)
    #print(sum(((A %*% n.apportioned - b)^2)))
    #print(A %*% n.apportioned)
    sum((A %*% n.apportioned - b)^2)

  }
  
  Aeq.sumToOne <- rep(1L,times = ncol(A)) %>% matrix(nrow = 1L)
  beq.sumToOne <- 1.0
  
  output1 <- fmincon(x0 = initialGuessFractions,
                     fn = costFunction,
                     lb = rep(0.0,16L),
                     ub = rep(1.0,16L),
                     Aeq = Aeq.sumToOne,
                     beq = beq.sumToOne)
  
  x.solution <- output1 %>% extract2("par") %>% round() %>% as.integer()
  
  nameVector <- expand.grid(c(0,1),c(0,1),c(0,1),c(0,1)) %>% as_tibble() %>% mutate(nameVector = paste0(Var1,Var2,Var3,Var4)) %>% pull(nameVector) %>% stri_reverse() %>% paste0("n.",.)
  names(x.solution) <- nameVector
  
  
  reconciled.df <- df.n %>% slice(i)
  reconciled.df %<>% rename_with(.cols = c(n.total:n.ownsNoComputers),.fn = \(x) gsub(pattern = "^n",x = x,replacement = "nhat"))
  reconciled.df %<>% pivot_longer(cols = c(nhat.total:nhat.ownsNoComputers))
  reconciled.df %<>% mutate(value = (A %*% x.solution) %>% as.vector)
  
  reconciled.df %<>% pivot_wider(names_from = name,values_from = value)
  
  list.reconciledDf[[i]] <- reconciled.df
  
  list.xSolution[[i]] <- x.solution %>% as_tibble_row() %>% bind_cols(df.n %>% slice(i) %>% dplyr::select(county,state),.) %>% add_column(convergenceFlag = output1 %>% extract2("convergence"))
  
  
  
  
}