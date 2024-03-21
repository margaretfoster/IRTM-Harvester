### Visualize specific thetas for all countries:

rm(list=ls())

loadPkg=function(toLoad){
  for(lib in toLoad){
    if(!(lib %in% installed.packages()[,1])){ 
      install.packages(lib, repos='http://cran.rstudio.com/') }
    suppressMessages( library(lib, character.only=TRUE) )
  }
}
packs <- c("ggplot2", "RColorBrewer",
           "latex2exp", "reshape2", "ggridges",
           "rlang", "stargazer", 
           "xtable")

loadPkg(packs)

thetasPath  <-"./Results/"

load(paste0(thetasPath, "all-Thetas.Rds"))

all_df$country <- as.factor(all_df$country)

length(levels(all_df$country)) ## 32 countries

## Table of data per year:
sink("R4t6data.txt")
xtable(table(all_df$round, all_df$int.year),
       caption = "Survey N, by year")
sink()


ggt1 <- ggplot(all_df) + 
  geom_density_ridges(aes(x=Theta1,
                          y=country), 
                          # fill=country), ## commented out for B/W
                      color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'black')+
  ggtitle('Posterior Distribution of Countrywide Mean Theta 1', 
  subtitle=paste0(" Round 4 (2008-2009), Round 5 (2011-2013), Round 6 (2014-2015)")) + 
  xlab(TeX('$\\theta_1$')) + 
  ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(all_df$country)))+
  theme_bw() + theme(legend.position = 'none') +
  facet_grid(~round)

ggt1

ggsave(ggt1, file="allThetasVisualizedBW.png",
       width=12,
       height=6,
       units='in')

## Subsets:
## Urban vs Rural:

all_df[which(all_df$urbrur==1), "urbrur"] <- "urban"
all_df[which(all_df$urbrur==2), "urbrur"] <- "rural"


ggt2 <- ggplot(all_df) + 
  geom_density_ridges(aes(x=Theta1,
                          y=country, 
                          fill=country), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Countrywide Mean Theta 1', 
          subtitle=paste0(" Round 4 (2008-2009), Round 5 (2011-2013), Round 6 (2014-2015)")) + 
  xlab(TeX('$\\theta_1$')) + 
  ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(all_df$country)))+
  theme_bw() + theme(legend.position = 'none') +
  facet_grid(~round * urbrur) ## mae


ggt2

ggsave(ggt2, 
     file="allThetasVisualized_UrbanRuralDivide.png",
     width=12,
     height=6,
     units='in')

print("Plotted All Thetas on Single Plot")