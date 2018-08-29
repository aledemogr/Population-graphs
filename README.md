# gender educational differentials in Africa (from Lopus and Frye paper http://journals.sagepub.com/doi/pdf/10.1177/2378023118795956)

rm(list = ls())

library(tidyverse)
library(png)
library(grid)
library(gridExtra)

###create a mock dataset
rtnorm <- function(n, mean, sd, a = -Inf, b = Inf){
  qnorm(runif(n, pnorm(a, mean, sd), pnorm(b, mean, sd)), mean, sd)
}

edu.diff <- rtnorm(n=100, mean=10, sd=10, a=0, b=30)

edu.cohort <- rtnorm(n=100, mean=50, sd=60, a=0, b=100)

df <- bind_cols(edu.diff=edu.diff, edu.cohort=edu.cohort)

area <- c('East', 'Central', 'West')
df$area <- sample(area, nrow(df), T, prob = c(.3,.3,.3))
cohort <- c(seq(1940, 2000, by=1))
df$birthcohort <- sample(cohort, nrow(df), T, prob = rep(1.6, 61))

df$females <- -1*(edu.diff)
df$males <- edu.diff
df$ordinal <- seq(1,nrow(df), by=1)
df1 <- df[,-1] %>% gather(edu.diff, value, c(4,5))

###
### Top chart part
###
edu.breaks <- paste0(seq(0, 100, by=20), '%')

#img1 <- readPNG("img1.png") #read in the map
#g1<- rasterGrob(img1, interpolate=TRUE)

colpal <- c("#E69F00", "#56B4E9", 'red')

y1label <- 'Derivation from educational gender parity \n(% male or female - % both sexes with any formal education)'
y2label <- 'birth cohort \n(minimum birth year)'
xlabel <- 'Proportion cohort with any formal education (both sexes)'
xnote <- 'gender \nparity'
segnote <- 'maximum possible gender disparity'


top <- ggplot(data=df1, aes(edu.cohort, value, col=area, group=ordinal, shape=edu.diff))+
  geom_point(col='black')+
  geom_hline(yintercept=0) +
  geom_linerange(aes(df1$edu.cohort, ymax=df1$value, ymin=0), show.legend = F)+
  scale_color_manual(values=colpal)+
  geom_segment(aes(x = 0, y = 0, xend = 50, yend = -50), data = df1, colour = "lightgrey", size=.2)+
  geom_segment(aes(x = 0, y = 0, xend = 50, yend = 50), data = df1, colour = "lightgrey", size=.2)+
  geom_segment(aes(x = 50, y = 50, xend = 100, yend = 0), data = df1, colour = "lightgrey", size=.2)+
  geom_segment(aes(x = 50, y = -50, xend = 100, yend = 0), data = df1, colour = "lightgrey", size=.2)+
 #annotation_custom(g1,xmin=90, xmax=106,ymin=30, ymax=55)+ #this in the map clipping
  annotate("text", x = 100:110, y = 0, label = xnote, size=2, color='darkgrey')+
  annotate("text", x = 35, y = 37, label = segnote, size=2.2, angle=22, color='darkgrey')+
  theme_bw()+
  theme(panel.grid.minor.y = element_line(color='lightgrey', size=0.1),
        panel.grid.major.y = element_line(color='lightgrey', size=0.1),
        panel.grid.major.x = element_line(color='lightgrey', size=0.1),
        panel.border = element_rect(fill = NA, color='white'),
        axis.line = element_line(colour = 'black', size = .2),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text=element_text(size=8),
        legend.position = c(.79, .99), legend.background = element_rect('white'), 
        legend.text=element_text(size=8), legend.title=element_blank(),
       legend.justification = c(0, 1) ,
       plot.margin = margin(0, 0.5, 0, 1.3, "cm"))+
  labs(x='', y=y1label, shape='')+
  ylim(-50, 50)+
  xlim(0, 110)+
  scale_x_continuous(breaks=seq(0, 100, by=20), lim=c(0,100))

###
### Bottom part
###
bottom <-
ggplot(df, aes(edu.cohort, birthcoho, col=area))+
  geom_point()+ theme_bw()+
  scale_color_manual(values=colpal)+
  theme(panel.grid.minor.y = element_line(color='lightgrey', size=0.1),
        panel.grid.major.y = element_line(color='lightgrey', size=0.1),
        panel.grid.major.x = element_line(color='lightgrey', size=0.1),
        panel.border = element_rect(fill = NA, color='white'),
        axis.line = element_line(colour = 'black', size = .1),
        text=element_text(size=8),
        legend.position="none",
        plot.margin = margin(-.09, 0.52, 1, 1.12, "cm"))+ #top right bottom left
  labs(x=xlabel, y=y2label, shape='')+
  scale_x_continuous(breaks=seq(0, 100, by=20), lim=c(0,100))+
  scale_y_reverse(breaks=seq(1940, 2000, by=20), lim=c(2005,1935))
   
 #grid.arrange(top, bottom, heights=c(...), widths=c(...), padding=0)


