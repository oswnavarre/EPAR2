library(tidyverse)
library(DescTools)
library(fGarch) 
library(jtools)
library(cowplot)


set.seed(20)
df <- data.frame(
  normal_distrib = round(rnorm(n = 10000, mean = 0, sd = 1),2),
  exp_distrib = rexp(n = 10000, rate = .1),
  gamma_distrib = round(rgamma(n = 10000, shape = 2, scale = 2),2),
  beta_distrib = round(rbeta(n = 10000, shape1 = 8, shape2 = 2),2)

)



mean.beta = mean(df$beta_distrib)
median.beta = median(df$beta_distrib)
mode.beta = Mode(df$beta_distrib)

g1 <- ggplot(data =df, aes(x=beta_distrib)) +
  geom_histogram(bins=20,fill="white",col="black") + 
  xlab("") + ylab("") +theme_void()

g2 <- ggplot(data =df, aes(x=beta_distrib)) +
  geom_boxplot() + 
  xlab("") + ylab("") +theme_void()

g3 <- ggplot(data =df, aes(x=beta_distrib)) +
  geom_density() + geom_vline(xintercept=mean.beta, col = "orange") +
  geom_vline(xintercept=median.beta,colour="red") +
  geom_vline(xintercept=mode.beta[1],colour="blue") + 
  xlab("") + ylab("") +theme_void()


g10 <- plot_grid(g3, g2, g1, nrow = 3)
g10
mean.gamma = mean(df$gamma_distrib)
median.gamma = median(df$gamma_distrib)
mode.gamma = Mode(df$gamma_distrib)

g4 <- ggplot(data =df, aes(x=gamma_distrib)) +
  geom_histogram(bins=20,fill="white",col="black") + 
  xlab("") + ylab("") +theme_void()

g5 <- ggplot(data =df, aes(x=gamma_distrib)) +
  geom_boxplot() + 
  xlab("") + ylab("") +theme_void()

g6 <- ggplot(data =df, aes(x=gamma_distrib)) +
  geom_density() + geom_vline(xintercept=mean.gamma, col = "orange") +
  geom_vline(xintercept=median.gamma,colour="red") +
  geom_vline(xintercept=mode.gamma[1],colour="blue") + theme_void()

g11 <- plot_grid(g6, g5, g4, nrow = 3)

ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, n = 10000, args = list(mean = 0, sd = 1), colour ="blue", size=1.20) +
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  theme_void()


dt <- rnorm(10000, mean = 0, sd=1)
dt <- as.data.frame(dt)
names(dt) <- c("Tiempo")
mean.t = mean(dt$Tiempo)
median.t = median(dt$Tiempo)

ggplot(dt, aes(x=Tiempo)) + 
  geom_histogram(bins=20,colour="black", fill="white") +
  ylab("")  + xlab("")  + theme_void()

ggplot(dt, aes(x=Tiempo)) + 
  geom_density() +
  geom_vline(xintercept=mean.t) +
  geom_vline(xintercept=median.t,col="red") +
  ylab("")  + xlab("")  + theme_void()

mean.normal = mean(df$normal_distrib)
median.normal = median(df$normal_distrib)
mode.normal = Mode(df$normal_distrib)


g7 <- ggplot(data =df, aes(x=normal_distrib)) +
  geom_histogram(bins=20,fill="white",col="black") + 
  xlab("") + ylab("") +theme_void()

g8 <- ggplot(data =df, aes(x=normal_distrib)) +
  geom_boxplot() + 
  xlab("") + ylab("") +theme_void()

g9 <- ggplot(data =df, aes(x=normal_distrib)) +
  geom_density() + geom_vline(xintercept=mean.normal, col = "orange") +
  geom_vline(xintercept=median.normal,colour="red")  + theme_void()

g12 <- plot_grid(g9, g8, g7, nrow = 3)

getwd()
ggsave("imagenes/izquierda.png",g10, width = 4, height = 6)
ggsave("imagenes/derecha.png",g11, width = 4, height = 6)
ggsave("imagenes/simetrico.png",g12, width = 4, height = 6)
