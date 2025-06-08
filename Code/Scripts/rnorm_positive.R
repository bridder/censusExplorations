rnorm_positive <- function(mean = 0, sd = 1)
{
  x <- -1.0
  while(x < 0)
  {
    x <- rnorm(n = 1L,mean = mean,sd = sd)
  }
  return(x)
}