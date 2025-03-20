library(tidyverse)

normalised_ichart <- function (x,y,m,sdmethod="average") {
  n <- x/y
  d1 <- c(NA, abs(diff(x/y)))
  d2 <- c(NA, sqrt(zoo::rollapply(1/y, 2, by = 1, sum)))
  s <- sqrt(pi/2)*d1/d2
  sbar <- ifelse (sdmethod=="average", 
          mean(s, na.rm=T),
          sqrt(2/pi)/qnorm(0.75)*median(s, na.rm=T))
  cl <-  sum(x)/sum(y)
  ucl <- cl+3*sbar*sqrt(1/y)
  lcl <- cl-3*sbar*sqrt(1/y)
  
  # return scaled values by factor m
  i<- 1:length(n)
  n <- m*n
  cl <- m*cl
  lcl <- m*lcl
  ucl <- m*ucl
  return(as_data_frame(cbind(i,x,y,n,cl,lcl,ucl)))
}


x <- c(77.58,77.19,73.69,69.17,67.16,67.58,77.96,74.99,78.09,81.74,70.82,68.61)
y <- c(1702,1919,1836,1846,1910,1779,2023,1987,1815,1998,1796,1987)


df <- normalised_ichart(x,y,m=100)


ggplot(df, aes(x=i,y=n)) + 
  geom_point() + 
  geom_line()+
  geom_line(aes(y=cl))+
  geom_line(aes(y=lcl))+
  geom_line(aes(y=ucl))

            
            
                

