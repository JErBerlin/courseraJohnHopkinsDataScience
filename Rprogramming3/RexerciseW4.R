# load lib
library(ggplot2)

## Generating Random Numbers From a Linear Model

set.seed(20)
x <- rnorm(100)
e <- rnorm(100,0,2)
y <- 0.5 + 2*x + e
# summary(y)
# plot(x,y)
lm1 <- data.frame(x=x,y=y)
ggplot(lm1, aes(x=x,y=y))+geom_point()

set.seed(10)
x <- rbinom(100,1,0.5)
e <- rnorm(100,0,2)
y <- 0.5+2+x+e
# plot(x,y)
lm2 <- data.frame(x=x,y=y)
ggplot(lm2, aes(x=x,y=y))+geom_point()

set.seed(10)
x <- rep(0:1, each=5)
e <- rnorm(10,0,20)
y <- 0.5 + 2*x + e
