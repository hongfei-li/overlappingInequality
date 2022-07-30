library(plotly)


################################################################################
## bivariate uniform scenarios (equality holds)
################################################################################

#input
a1 <- 0 
a2 <- 3 
b1 <- 0
b2 <- 2

#plot
x1 <- seq(a1-1, a2+1, length= 100)
x2 <- seq(b1-3, b2+3, length= 100)
z1 <- function(x1,x2){ z <- as.numeric(x1 > a1 & x1 < a2 & x2 > b1 & x2 < b2) / ((a2-a1) * (b2 - b1))}
f1 <- t(outer(x1,x2,z1))

color1 <- rep(0, length(c(f)))
dim(color1) <- dim(f)
color2 <- rep(1, length(f))
dim(color2) <- dim(f)

p <- plot_ly(colors = c('red', 'blue')) %>% 
  add_surface(x=x1,y=x2,z=f1, ## name = "P1",
              surfacecolor=color1,
              opacity = 0.8,
              cauto=F,
              cmax=1,
              cmin=0
  ) 

a1 <- 1 
a2 <- 5 
b1 <- 0
b2 <- 2
x1 <- seq(a1-1, a2+1, length= 100)
x2 <- seq(b1-3, b2+3, length= 100)
z2 <- function(x1,x2){ z <- as.numeric(x1 > a1 & x1 < a2 & x2 > b1 & x2 < b2) / ((a2-a1) * (b2 - b1))}
f2 <- t(outer(x1,x2,z1))

p <- p %>% 
  add_surface(x=x1,y=x2,z=f2, name = "Q1",
              surfacecolor=color2,
              opacity = 0.5,
              cauto=F,
              cmax=1,
              cmin=0)
p


################################################################################
## bivariate uniform scenarios (equality does not hold)
################################################################################

#input
a1 <- 0 
a2 <- 3 
b1 <- 0
b2 <- 4

#plot
x1 <- seq(a1-1, a2+1, length= 100)
x2 <- seq(b1-3, b2+3, length= 100)
z1 <- function(x1,x2){ z <- as.numeric(x1 > a1 & x1 < a2 & x2 > b1 & x2 < b2) / ((a2-a1) * (b2 - b1))}
f1 <- t(outer(x1,x2,z1))

color1 <- rep(0, length(c(f)))
dim(color1) <- dim(f)
color2 <- rep(1, length(f))
dim(color2) <- dim(f)

p <- plot_ly(colors = c('red', 'blue')) %>% 
  add_surface(x=x1,y=x2,z=f1, ## name = "P1",
              surfacecolor=color1,
              opacity = 0.7,
              cauto=F,
              cmax=1,
              cmin=0
  ) 

a1 <- 1 
a2 <- 5 
b1 <- 1
b2 <- 3
x1 <- seq(a1-1, a2+1, length= 100)
x2 <- seq(b1-3, b2+3, length= 100)
z2 <- function(x1,x2){ z <- as.numeric(x1 > a1 & x1 < a2 & x2 > b1 & x2 < b2) / ((a2-a1) * (b2 - b1))}
f2 <- t(outer(x1,x2,z1))

p <- p %>% 
  add_surface(x=x1,y=x2,z=f2, name = "Q1",
              surfacecolor=color2,
              opacity = 0.7,
              cauto=F,
              cmax=1,
              cmin=0)
p

################################################################################
## two bivariate normal scenarios (equality holds)
################################################################################

#input
mu1<-2 #mean of X_1
mu2<-3 #mean of X_2
sigma11<-2 #variance of X_1
sigma22<-1 #variance of X_2
sigma12<-0 #covariance of X_1 and X_2 

#plot
x1 <- seq(mu1-3, mu1+3, length= 100)
x2 <- seq(mu2-3, mu2+3, length= 100)
z <- function(x1,x2){ z <- exp(-(sigma22*(x1-mu1)^2+sigma11*(x2-mu2)^2-2*sigma12*(x1-mu1)*(x2-mu2))/(2*(sigma11*sigma22-sigma12^2)))/(2*pi*sqrt(sigma11*sigma22-sigma12^2)) }
f <- t(outer(x1,x2,z))

color1 <- rep(0, length(c(f)))
dim(color1) <- dim(f)
color2 <- rep(1, length(f))
dim(color2) <- dim(f)

p <- plot_ly(colors = c('red', 'blue')) %>% 
  add_surface(x=x1,y=x2,z=f, ## name = "P1",
              surfacecolor=color1,
              opacity = 0.8,
              cauto=F,
              cmax=1,
              cmin=0,
              contours = list(
                z = list(
                  show=TRUE,
                  usecolormap=TRUE,
                  highlightcolor="#ff0000",
                  project=list(z=TRUE)
                ))
  ) %>% 
  add_surface(x=x1+3,y=x2,z=f, name = "Q1",
              surfacecolor=color2,
              opacity = 0.5,
              cauto=F,
              cmax=1,
              cmin=0,
              contours = list(
                z = list(
                  show=TRUE,
                  usecolormap=TRUE,
                  highlightcolor="#0000ff",
                  project=list(z=TRUE)
                )))
p


# 
# 
# 
# t1 <- seq(-3, 3, 0.1); t2 <- seq(-3, 3, 0.1)
# 
# p1 <- matrix(nrow = length(t1), ncol = length(t2))
# p2 <- matrix(nrow = length(t1), ncol = length(t2))
# 
# p8a1 <- 1.2
# p8a2 <- 1
# p8d <- -1
# p8b1 <- 0.7
# p8b2 <- 0.6
# 
# for (i in 1:length(t2)) {
#   for (j in 1:length(t1)) {
#     p1[i, j] <- 1 / (1 + exp(-1.7 * (p8a1 * t1[j] + p8a2 * t2[i] + p8d)))
#     p2[i, j] <- (1 / (1 + exp(-1.7 * p8a1 * (t1[j]- p8b1)))) * 
#       (1 / (1 + exp(-1.7 * p8a2 * (t2[j]- p8b2))))
#   }
# }
# 
# df1 <- list(t1, t2, p1)
# df2 <- list(t1, t2, p2)
# 
# names(df1) <- c("t1", "t2", "p1")
# names(df2) <- c("t1", "t2", "p2")
# m <- list(l = 10, r = 10, b = 5, t = 0, pad = 3)
# 
# color <- rep(0, length(df1$p1))
# dim(color) <- dim(df1$p1)
# p <- plot_ly(colors = c('red', 'blue')) %>%
#   add_surface(x = df1$t1,
#               y = df1$t2,
#               z = df1$p1,
#               opacity = 0.8,
#               #surfacecolor=c('red')
#               surfacecolor=color,
#               cauto=F,
#               cmax=1,
#               cmin=0
#   )
# color2 <- rep(1, length(df2$p2))
# dim(color2) <- dim(df2$p2 )
# 
# p <-  add_surface(p,
#                   x = df2$t1,
#                   y = df2$t2,
#                   z = df2$p2,
#                   opacity = 1,
#                   surfacecolor=color2,
#                   cauto=F,
#                   cmax=1,
#                   cmin=0)
# p


################################################################################
## two bivariate normal scenarios (equality does not hold)
################################################################################

#input
mu1<-2 #mean of X_1
mu2<-3 #mean of X_2
sigma11<-2 #variance of X_1
sigma22<-1 #variance of X_2
sigma12<--1 #covariance of X_1 and X_2 

#plot
x1 <- seq(mu1-3, mu1+3, length= 100)
x2 <- seq(mu2-3, mu2+3, length= 100)
z <- function(x1,x2){ z <- exp(-(sigma22*(x1-mu1)^2+sigma11*(x2-mu2)^2-2*sigma12*(x1-mu1)*(x2-mu2))/(2*(sigma11*sigma22-sigma12^2)))/(2*pi*sqrt(sigma11*sigma22-sigma12^2)) }
f <- t(outer(x1,x2,z))

color1 <- rep(0, length(c(f)))
dim(color1) <- dim(f)
color2 <- rep(1, length(f))
dim(color2) <- dim(f)

p <- plot_ly(colors = c('red', 'blue')) %>% 
  add_surface(x=x1,y=x2,z=f, ## name = "P1",
              surfacecolor=color1,
              opacity = 0.8,
              cauto=F,
              cmax=1,
              cmin=0,
              contours = list(
                z = list(
                  show=TRUE,
                  usecolormap=TRUE,
                  highlightcolor="#ff0000",
                  project=list(z=TRUE)
                ))
  ) %>% 
  add_surface(x=x1+3,y=x2,z=f, name = "Q1",
              surfacecolor=color2,
              opacity = 0.5,
              cauto=F,
              cmax=1,
              cmin=0,
              contours = list(
                z = list(
                  show=TRUE,
                  usecolormap=TRUE,
                  highlightcolor="#0000ff",
                  project=list(z=TRUE)
                )))
p