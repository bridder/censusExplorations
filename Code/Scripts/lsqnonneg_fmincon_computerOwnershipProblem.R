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


costFunction <- function(x)
{
  sum((A %*% x - b)^2)
}