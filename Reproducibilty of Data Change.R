
#Reproducibility Data Challenge

library(RMKdiscrete)
library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)


mdat <- read.csv("~/Documents/BSD-QBio5/tutorials/reproducibility/data/mitchell_weevil_egg_data_1975.csv")
cdat <- read.csv("~/Documents/BSD-QBio5/tutorials/reproducibility/data/cole_arthropod_data_1946.csv")

head(mdat)
head(cdat)

## Spiders

#Determine and set the number of boards, count the spiders, and get mean of spiders per board
cdat$nspiders <- cdat$k_number_of_arthropods * cdat$C_count_of_boards_with_k_spiders
mean_of_spiders <- sum(cdat$nspiders) / sum(cdat$C_count_of_boards_with_k_spiders)
sum_of_boards <- sum(cdat$C_count_of_boards_with_k_spiders)

#Set up new dataframe that contains the Poisson Distribution numbers and determine probability from data alone
cdat$p_spider <- dpois(cdat$k_number_of_arthropods,lambda = mean_of_spiders)
cdat$p_normspider <- cdat$C_count_of_boards_with_k_spiders / sum(cdat$C_count_of_boards_with_k_spiders)


#dLGP step for setting curve for the additional graph requested
cdat$p_dLGP_spider <- dLGP(cdat$k_number_of_arthropods,theta = mean_of_spiders,lambda = 0,nc=NULL,log=FALSE)


#Plotting first graph with Poisson distribution and data
spider_plot <- ggplot(cdat, aes(x=cdat$k_number_of_arthropods, y=p_normspider)) +
  geom_point(size = 3) + xlab("") + ylab("") + 
  geom_line(data=cdat, aes(x=cdat$k_number_of_arthropods, y=p_spider), linetype='dashed', colour='green') +  
  geom_point(data=cdat, aes(x=cdat$k_number_of_arthropods, y=p_spider),colour='green', shape=0, size = 3)
spider_plot

#PLotting same graph but with dLGP data
spider_plot <- ggplot(cdat, aes(x=cdat$k_number_of_arthropods, y=p_normspider)) +
  geom_point(size = 3) + xlab("") + ylab("") +
  geom_line(data=cdat, aes(x=cdat$k_number_of_arthropods, y=p_dLGP_spider), linetype="dotted", colour='red') +  
  geom_point(data=cdat, aes(x=cdat$k_number_of_arthropods, y=p_dLGP_spider),colour='red', shape=0, size = 3)
spider_plot

## Sowbugs

#Determine and set the number of boards, count the sowbugs, and get mean of sowbugs per board
cdat$nsowbugs <- cdat$k_number_of_arthropods * cdat$C_count_of_boards_with_k_sowbugs
mean_of_sowbugs <- (sum(cdat$nsowbugs) / sum(cdat$C_count_of_boards_with_k_sowbugs))*(1-.53214)
sum_of_boards <- sum(cdat$C_count_of_boards_with_k_sowbugs)

#Set up new dataframe that contains the Poisson Distribution numbers and determine probability from data alone
cdat$p_sowbugs <- dpois(cdat$k_number_of_arthropods,lambda = mean_of_sowbugs)
cdat$p_normsowbugs <- cdat$C_count_of_boards_with_k_sowbugs / sum(cdat$C_count_of_boards_with_k_sowbugs)


#dLGP step for setting curve for the additional graph requested
cdat$p_dLGP_sowbugs <- dLGP(cdat$k_number_of_arthropods,theta = mean_of_sowbugs,lambda = .53214,nc=NULL,log=FALSE)


#Plotting first graph with Poisson distribution and data
sowbugs_plot <- ggplot(cdat, aes(x=cdat$k_number_of_arthropods, y=p_normsowbugs)) +
  geom_point(size = 3) + xlab("") + ylab("") + 
  geom_line(data=cdat, aes(x=cdat$k_number_of_arthropods, y=p_sowbugs), linetype='dashed', colour='green') +  
  geom_point(data=cdat, aes(x=cdat$k_number_of_arthropods, y=p_sowbugs),colour='green', shape=0, size = 3)
sowbugs_plot


#PLotting same graph but with dLGP data
sowbug_plot <- ggplot(cdat, aes(x=cdat$k_number_of_arthropods, y=p_normsowbugs)) +
  geom_point(size = 3) + xlab("") + ylab("") +
  geom_line(data=cdat, aes(x=cdat$k_number_of_arthropods, y=p_dLGP_sowbugs), linetype="dotted", colour='red') +  
  geom_point(data=cdat, aes(x=cdat$k_number_of_arthropods, y=p_dLGP_sowbugs),colour='red', shape=0, size = 3)
sowbug_plot

library(ggplot2)
library(RMKdiscrete)
library(cowplot)
spider_mean <- sum(cdat$C_count_of_boards_with_k_spiders * cdat$k_number_of_arthropods)/sum(cdat$C_count_of_boards_with_k_spiders)
sowbug_mean <- sum(cdat$C_count_of_boards_with_k_sowbugs * cdat$k_number_of_arthropods)/sum(cdat$C_count_of_boards_with_k_sowbugs)
egg_mean <- sum(mdat$C_count_of_beans_with_k_eggs * mdat$k_number_of_eggs)/sum(mdat$C_count_of_beans_with_k_eggs)

spider_p <- dpois(cdat$k_number_of_arthropods,spider_mean)
sowbug_p <- dpois(cdat$k_number_of_arthropods,sowbug_mean)
egg_p <- dpois(mdat$k_number_of_eggs, egg_mean)

dat$C_count_of_boards_with_k_spiders <- as.numeric(cdat$C_count_of_boards_with_k_spiders)
dat$C_count_of_boards_with_k_sowbugs <- as.numeric(cdat$C_count_of_boards_with_k_sowbugs)
dat2$k_number_of_eggs <- as.numeric(mdat$k_number_of_eggs)

plot_spider <- ggplot(data = cdat + spider_p, aes(x = cdat$k_number_of_arthropods, y = spider_p)) + geom_smooth() + geom_point() + labs(x = "Spider Counts", y = "Probability")
plot_sowbug <- ggplot(data = cdat + sowbug_p, aes(x = cdat$k_number_of_arthropods, y = sowbug_p)) + geom_smooth() + geom_point() + labs(x = "Sowbug Counts", y = "Probability")
plot_egg <- ggplot(data = mdat + egg_p, aes(x = mdat$k_number_of_eggs, y = egg_p)) + geom_smooth() + geom_point() + labs(x = "Egg Counts", y = "Probability")

spider_LGP <- dLGP(cdat$k_number_of_arthropods, theta = spider_mean, lambda = 0)
sowbug_LGP <- dLGP(cdat$k_number_of_arthropods, theta = (sowbug_mean*(1-0.53214)), lambda = 0.53214)
egg_LGP <- dLGP (mdat$k_number_of_eggs, theta = (egg_mean*(1-lambda2)), lambda = lambda2)

plot_spider_origin <-ggplot(data = cdat, aes(x = cdat$k_number_of_arthropods, y = cdat$C_count_of_boards_with_k_spiders)) + geom_smooth() + geom_point() + labs(x = "Spider Counts", y = "Number of Boards")
plot_sowbug_origin <-ggplot(data = cdat, aes(x = cdat$k_number_of_arthropods, y = cdat$C_count_of_boards_with_k_sowbugs)) + geom_smooth() + geom_point() + labs(x = "Sowbug Counts", y = "Number of Boards")
plot_egg_origin <-ggplot(data = mdat, aes(x = mdat$k_number_of_eggs, y = mdat$C_count_of_beans_with_k_eggs)) + geom_smooth() + geom_point() + labs(x = "Egg Counts", y = "Number of Spots")

plotSpider <- plot_grid(plot_spider,plot_spider_origin)
plotSowbug <- plot_grid(plot_sowbug,plot_sowbug_origin)
plotEgg <- plot_grid(plot_egg,plot_egg_origin)

lambda2 = 0.92314
plot_sowbug_LGP <- ggplot(data = cdat + sowbug_p + sowbug_LGP) + geom_point(aes(x = cdat$k_number_of_arthropods, y = sowbug_p)) + geom_point(aes(x = cdat$k_number_of_arthropods, y = sowbug_LGP)) +labs(title="Probability Distributions for Sowbug Counts", x = "Sowbug Counts", y = "Probability") + theme(plot.title = element_text(hjust = 0.5)) + geom_smooth(aes(x = cdat$k_number_of_arthropods, y = sowbug_p, color="Poisson")) + geom_smooth(aes(x = cdat$k_number_of_arthropods, y = sowbug_LGP,color = "LGP"))
plot_egg_LGP <- ggplot(data = mdat + egg_p + egg_LGP) + geom_point(aes(x = mdat$k_number_of_eggs, y = egg_p)) + geom_point(aes(x = mdat$k_number_of_eggs, y = egg_LGP)) +labs(title="Probability Distributions for Egg Counts", x = "Egg Counts", y = "Probability") + theme(plot.title = element_text(hjust = 0.5)) + geom_smooth(aes(x = mdat$k_number_of_eggs, y = egg_p, color="Poisson")) + geom_smooth(aes(x = mdat$k_number_of_eggs, y = egg_LGP, color = "LGP"))
plot_spider_LGP <- ggplot(data = cdat + spider_p + spider_LGP) + geom_point(aes(x = cdat$k_number_of_arthropods, y = spider_p)) + geom_point(aes(x = cdat$k_number_of_arthropods, y = spider_LGP)) +labs(title="Probability Distributions for Spider Counts", x = "Sowbug Counts", y = "Probability") + theme(plot.title = element_text(hjust = 0.5)) + geom_smooth(aes(x = cdat$k_number_of_arthropods, y = spider_p, color="Poisson")) + geom_smooth(aes(x = cdat$k_number_of_arthropods, y = spider_LGP, color ="LGP"))

