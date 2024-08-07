rm(list=ls())

## Customize for your machine:
setwd("/Users/Promachos/Dropbox (Personal)/IRTM-Harvester/code")

#### Load packages:
loadPkg=function(toLoad){
  for(lib in toLoad){
    if(!(lib %in% installed.packages()[,1])){ 
      install.packages(lib, repos='http://cran.rstudio.com/') }
    suppressMessages( library(lib, character.only=TRUE) )
  }
}

library(groundhog)

packs <- c("tidyverse", "ggmap",
           "ggplot2", "maps",
           #"rgdal",
           "viridis",
           'pscl', "MASS", "boot", ## for Zero-inflated negative binomial
           "lmtest", "sandwich", "broom") ## for clustered standard errors

groundhog.library(packs, "2023-09-30")


## Load previous-processed data:

dataPath <- "./Data/"

load(paste0(dataPath, "acled_thetas.RData"))
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Modeling:
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vars <- c("num_events", ## dvs
          "T1.quant.25","T2.quant.75", "T3.quant.75",
          "T1.mean","T2.mean","T3.mean", "T1.var",
          "urbrur", "grid")


### Make the regression/correlation across country
## then plot the point estimate and coef plot across 
## countries. Make the point that the benefit of the IRT-M model
## in quantifying surveys about  is the
## ability to precisely estimate inside countries
## Theta 1 can be compared across locations 
## theta 2 /theta3 meanings are more contextual

## First, simple, model:
## for each interaction of interest:
## interaction ~ Theta 1.
## Expectation: Theta 1 negatively correlated with violent events 
## (Eg: lower Theta 1, higher number of violent events)

## type of event ~ Theta 1 
## Expectation: Theta 1 negatively correlated with protests
## (Eg: lower Theta 1, higher number of protests)

## "Demonstrations" is the coding for events that involved rioters, protesters
## vs other entities
demonstrations <- acled_thetas_Lag %>%
  filter(event_grouping== "non_org_non_state") %>%
  filter(!is.na(T1.quant.25)) ## protests/riots in same year as AB interviews

## "Violence" is the coding for events with groups, millitias:

violence <- acled_thetas_Lag %>%
  filter(event_grouping== "org_non_state") %>%
  filter(!is.na(T1.quant.25)) ## protests/riots in same year as AB interviews



## Country-level correlations
## All country GIDs, all rounds:
## Correlation:
## Pearson (default) is not the appropriate correlation test, because it requires a 
## linear relationship (of two normally-distributed variables). As the overall plot 
## suggest, the relationship is U-shaped.

d_cors <- data.frame()
for(u in unique(demonstrations$country)){
 c <- round(cor(x=demonstrations[which(demonstrations$country==u),]$num_events,
                y=demonstrations[which(demonstrations$country==u),]$T1.mean,
                method = c("kendall")), 4)
 d_cors <- rbind(d_cors, 
                cbind(c, u))
}  

colnames(d_cors) <- c("Correlation", "Country")
d_cors$Correlation <- as.numeric(d_cors$Correlation)
d_cors$Country <- as.factor(d_cors$Country)

## in number of observations:
num_dem <- as.data.frame(table(demonstrations$country))

d_cors <- merge(d_cors,
             num_dem, 
             by.x=c("Country"),
             by.y=c("Var1"))

d_cors_plot <- ggplot(d_cors,
                      aes(y= Correlation, 
                          x=Country,
                      fill=Correlation)) +
  geom_bar(stat="identity", colour="black", show.legend = FALSE) +
  scale_fill_gradient2(low="lightgray", high="darkgray", midpoint=0) +
  labs(x="\nCountry", y="Correlation Theta 1 and Demonstrations\n") +
  theme_bw() +
  geom_text(aes(label = Freq), vjust = -1)+
  theme(axis.text.x=element_text(angle=45, vjust=0.5))+
  geom_hline(yintercept=0, linetype='dotted', col = 'gray')+
   theme(legend.position="bottom")

d_cors_plot

ggsave(d_cors_plot,
       width = 8, height = 6,
       file="correlations_demonstrations_meanT1.png")

## By Round:
## First filter out the rows with no data:
d_cors_2 <- data.frame()
for(u in unique(demonstrations$country)){
  for(r in unique(demonstrations$int.round)){
    c <- round(cor(x=demonstrations[which(demonstrations$country==u &
                                            demonstrations$int.round==r),]$num_events,
                   y=demonstrations[which(demonstrations$country==u &
                                            demonstrations$int.round==r),]$T1.mean,
                   method = c("kendall")), 4)
  d_cors_2 <- rbind(d_cors_2, 
                  cbind(c, u, r))
  }  
}

colnames(d_cors_2) <- c("Correlation", "Country", "Round")
d_cors_2$Correlation <- as.numeric(d_cors_2$Correlation)
d_cors_2$Country <- as.factor(d_cors_2$Country)
d_cors_2$Round <- paste0("Round ", d_cors_2$Round)


### Plot, wrapped by round:
d_cors_p2 <- ggplot(d_cors_2,
                    aes(y= Correlation, 
                        x=Country,
                        fill=Correlation)) +
  geom_bar(stat="identity", colour="black", show.legend = FALSE) +
  scale_fill_gradient2(low="lightgray", high="darkgray", midpoint=0) +
  labs(x="\nCountry", y="Correlation Theta 1 and Demonstrations\n") +
  #ggtitle("Correlations Between Average Theta 1 and ACLED Demonstrations")+
  theme_bw() +
  ##theme(axis.text.x=element_text(angle=45, vjust=0.5)) +
  facet_wrap(~Round, nrow=3)+
  ##geom_text(aes(label = Freq), vjust = -1)+
  geom_hline(yintercept=0, linetype='dotted', col = 'gray')+
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))+
  theme(legend.position="bottom")

d_cors_p2

ggsave(d_cors_p2,
       width = 8, height = 6,
       file="MeanTheta1_demonstrations_cors_byr.png")


#####################
### Correlations between Theta 1 and interaction types
#####################

#### edit to wrap for each of the types of interactions

## Create a threshold for # of events:
event_freqs <- as.data.frame(table(acled_thetas_Lag$interaction)) %>%
  arrange(desc(Freq))
## extract the interaction codes for the 16 types with over 500 instances in the data:
tomodel <- event_freqs[which(event_freqs$Freq >=500), "Var1"]
tomodel <- as.numeric(as.character(tomodel)) ## awkward but a factor edgecase

v_cors <- data.frame()
for(inter in tomodel){
   
  vdat <- acled_thetas_Lag %>%
  filter(interaction == inter) %>%
  filter(!is.na(T1.quant.25)) ## Violence in same year as AB Interviews

  ## Then iterate through the countries:

  for(u in unique(violence$country)){
    c <- round(cor(x=violence[which(violence$country==u),]$num_events,
                 y=violence[which(violence$country==u),]$T1.mean,
                 method = c("kendall")), 4)
    v_cors <- rbind(v_cors, 
                  cbind(c, u, inter)) 
  }  
}

colnames(v_cors) <- c("Correlation", "Country", "EventType")
v_cors$Correlation <- as.numeric(v_cors$Correlation)
v_cors$Country <- as.factor(v_cors$Country)
v_cors$EventType <- as.factor(v_cors$EventType)


##
## By AB-Round:
v_cors_2 <- data.frame()

for(int in tomodel){
  vdat <- acled_thetas_Lag %>%
    filter(interaction == int) %>%
    filter(!is.na(T1.quant.25)) ## Violence in same year as AB Interviews
  
for(u in unique(vdat$country)){
  for(r in unique(vdat$int.round)){
    c <- round(cor(x=vdat[which(vdat$country==u &
                                  vdat$int.round==r),]$num_events,
                   y=vdat[which(vdat$country==u &
                                  vdat$int.round==r),]$T1.mean,
                   method = c("kendall")), 4)
  v_cors_2 <- rbind(v_cors_2, 
                  cbind(c, u, r, int))
  } 
}
}

colnames(v_cors_2) <- c("Correlation", "Country", "Round", "EventType" )
v_cors_2$Correlation <- as.numeric(v_cors_2$Correlation)
v_cors_2$Country <- as.factor(v_cors_2$Country)
v_cors_2$Round <- paste0("Round ", v_cors_2$Round)
v_cors_2$EventType <- as.factor(v_cors_2$EventType)


## Overview  plot of violence correlations
### Plot, wrapped by round:
v_cors_rounds <- ggplot(v_cors_2,
                        aes(y= Correlation, 
                            x=Country,
                            fill=Correlation)) +
  geom_bar(stat="identity", colour="black", show.legend = FALSE) +
  geom_text(aes(label=EventType), size = 2, position = position_stack(vjust = 0.5))+
  scale_fill_gradient2(low="lightgray", high="darkgray", midpoint=0) +
  labs(x="\nCountry", y="Correlation Theta 1 and Organized Violence\n") +
  #ggtitle("Correlations Between Average Theta 1 and ACLED Organized Violence")+
  theme_bw() +
  ##theme(axis.text.x=element_text(angle=45, vjust=0.5)) +
  facet_wrap(~Round, nrow=3)+
  ##geom_text(aes(label = Freq), vjust = -1)+
  geom_hline(yintercept=0, linetype='dotted', col = 'gray')+
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))+
  theme(legend.position="bottom")

v_cors_rounds

ggsave(v_cors_rounds, file="MeanTheta1_Correlations_OrganizedViolence.png")


### Data is too big to put in a single plot:
## Events 60 [protests], 37 [militia vs civilians], 13 [military vs militia],
## 15 [military vs rioters], 12 [military vs rebels] are the highest

event_focus = 60
v_cors_p3 <- ggplot(v_cors_2[which(v_cors_2$EventType==event_focus),],
                      aes(y= Correlation, 
                          x= Country,
                          fill=Correlation)) +
  geom_bar(stat="identity", colour="black", show.legend = FALSE) +
  scale_fill_gradient2(high="lightgray", low="darkgray", midpoint=0) +
  labs(x="\nCountry", 
       y=paste0("Kendall Correlation Theta 1 and ACLED Event ", event_focus, "\n")) +
  ggtitle(paste0("Correlations Theta 1 and ACLED Event ", event_focus, "\n"))+
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, vjust=0.5)) +
  facet_wrap(~Round, nrow=3)+
  geom_hline(yintercept=0, linetype='dotted', col = 'gray')

v_cors_p3




### Heatmap for specific types of events
## (y-axis; though this should be an interactive dashboard)
v_cors_p2 <- ggplot(v_cors_2,
                    aes(y= EventType, 
                        x= Country,
                        fill=Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low="white", high="darkgray", midpoint=0) +
  labs(x="\nCountry", 
      y=paste0("Kendall Correlation Theta 1 and ACLED Events\n")) +
  ggtitle("Correlations Theta 1 and ACLED Events")+
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, vjust=0.5)) +
  facet_wrap(~Round, nrow=3, scales="free")+
  theme(legend.position="bottom")

v_cors_p2
#ggsave(v_cors_p2, file="MeanTheta1_Correlations_Different_Events.png")


## Interactive:
## commented out b/c prepping for manuscript
#library(plotly)
#ggplotly(v_cors_p2, tooltip="text")



### Correlation between violence and Theta 1 lower quartile:

v_var <- data.frame()

for(u in unique(violence$country)){
  for(r in unique(violence$int.round)){
    c <- round(cor(x=violence[which(violence$country==u &
                                      violence$int.round==r),]$num_events,
                   y=violence[which(violence$country==u &
                                      violence$int.round==r),]$T1.mean,
                   method = c("kendall")), 4)
    v_var <- rbind(v_var, 
                      cbind(c, u, r))
  }
}  

colnames(v_var) <- c("Correlation", "Country", "Round")
v_var$Correlation <- as.numeric(v_var$Correlation)
v_var$Country <- as.factor(v_var$Country)

v_var <- ggplot(v_var,
                    aes(y= Correlation, 
                        x=Country,
                        fill=Correlation)) +
   geom_bar(stat="identity", colour="black", show.legend = FALSE) +
  scale_fill_gradient2(low="gray", high="darkgreen", midpoint=0) +
  labs(x="\nCountry", y="Correlation Variance Theta 1 and Political Violence\n") +
  ggtitle("Correlations Between Theta 1 Variance and ACLED Political Violence")+
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, vjust=0.5)) +
  facet_wrap(~Round, nrow=3)+
  geom_hline(yintercept=0, linetype='dotted', col = 'gray')

v_var

### Diving deeper:

## Violence: 
violence[which(violence$country=="Senegal"),]

## All countries, all rounds aggregated:

## Mean
cor.test(x=violence$T1.mean, 
         y=violence$num_events, 
         method=c("kendall") ##non-parametric
)

## Lower quartile of Theta 1
cor.test(x=violence$T1.quant.25, 
         y=violence$num_events, 
         method=c("kendall") ##non-parametric
)


## Variance in Theta and violence
cor.test(x=violence$T1.var, 
         y=violence$num_events, 
         method=c("kendall") ##non-parametric
)


### Now countries with above X observations:
## observations are grid-years with thetas:

country_freq <- as.data.frame(table(violence$country))
summary(country_freq$Freq) ## 1-297; median 23.5; mean 45.37

cf2 <- as.character(country_freq[which(country_freq$Freq > 50),]$Var1)
length(cf2) ##10 countries with threshold set at 50

vhigh <- violence[which(violence$country %in% cf2),] ## 996 [so, 73% of the overall data]

cor.test(x=vhigh$T1.mean, 
         y=vhigh$num_events, 
         method=c("kendall") ##basically the same as for the whole data
)


## Urban = 1; rural = 2
cor.test(x=vhigh[which(vhigh$urbrur==1),]$T1.mean, 
         y=vhigh[which(vhigh$urbrur==1),]$num_events, 
         method=c("kendall") ##non-parametric
)

cor.test(x=vhigh[which(vhigh$urbrur==2),]$T1.mean, 
         y=vhigh[which(vhigh$urbrur==2),]$num_events, 
         method=c("kendall") ##non-parametric
)


##
cf3 <- as.character(country_freq[which(country_freq$Freq > 100),]$Var1)
length(cf3) ##4 countries when threshold set to more than 100 grid-years

vhigh2 <- violence[which(violence$country %in% cf3),] ## 612 obs

cor.test(x=vhigh2$T1.mean, 
         y=vhigh2$num_events, 
         method=c("kendall") ##basically the same as for the whole data
)


## Correlation test in only countries with "a lot" (more than 50)
cor.test(x=violence[which(violence$urbrur==1),]$T1.mean, 
         y=violence[which(violence$urbrur==1),]$num_events, 
         method=c("kendall") ##non-parametric
)


## Urban = 1; rural = 2
cor.test(x=demonstrations[which(demonstrations$urbrur==1),]$T1.mean, 
         y=demonstrations[which(demonstrations$urbrur==1),]$num_events, 
         method=c("kendall") ##non-parametric
)

## Kendall for Demonstrations
cor.test(x=demonstrations$T1.quant.25, 
         y=demonstrations$num_events, 
         method=c("kendall") ##non-parametric
)





## Urban = 1; rural = 2
cor.test(x=violence[which(violence$urbrur==1),]$T1.mean, 
         y=violence[which(violence$urbrur==1),]$num_events, 
         method=c("kendall") ##non-parametric
         )

cor.test(x=violence[which(violence$urbrur==2),]$T1.mean, 
         y=violence[which(violence$urbrur==2),]$num_events, 
         method=c("kendall"))

cor.test(x=demonstrations[which(demonstrations$urbrur==1),]$T1.mean, 
         y=demonstrations[which(demonstrations$urbrur==1),]$num_events)

cor.test(x=demonstrations[which(demonstrations$urbrur==2),]$T1.mean, 
         y=demonstrations[which(demonstrations$urbrur==2),]$num_events)
## Different observations?

table(violence$country)

## "All the correlations" 
## slice and dice in all the ways
## "stop when the document is unreadable" ; we'll talk next time
## pull out Lambda 
## contact liz about what the shibboleth question is
## Send the visualization plot to Liz
## "We have enough layers" means that it might hit

### With error bars?




## Run for each of the different data sets (PRIO, GID, SCAD)
## Run for different thresholds (countries with more than 10 entries; more than 10, etc)
## run for just the most populated countries (Uganda; Nigeria; South Africa; Kenya)
##

## TODO: dig into the violence and demonstrations correlations
## note: outliers strongly influence pearsons correlation;
## 1) find outliers; 2) check Kendall

plot(hist(violence$num_events))

###

## Violence:
gg <- ggplot(violence[which(violence$urbrur==1 |
                              violence$urbrur==2),], 
             aes(x = T1.mean, 
                 y = num_events))+
  geom_point()+
  labs(xlab = "Theta One Mean", 
       ylab = "Violence")+
  theme_bw() + 
  facet_wrap(~urbrur)

gg

## ToDo:
## ~ simple quadratic regression; so we can say effect we're finding it consistent with the literature.
violence$meansq <- violence$T1.mean^2

plot(density(violence$meansq))

summary(lm(formula = violence$num_events ~ violence$T1.mean+ violence$meansq))

summary(lm(formula = violence$num_events ~ violence$T1.mean))

## Demonstrations:
## Observe that this seems to fit well
## with the extant theory-- in the middle; but trending displeased
gg <- ggplot(demonstrations, 
             aes(x = T1.mean, 
                 y = num_events))+
  geom_point()+
  ##scale_colour_manual(name = 'Urban/Rural',
    ##                  values = c('gray','green')) +
  labs(xlab = "Theta One Mean", 
       ylab = "Demonstrations")+
  theme_bw()

gg


       
