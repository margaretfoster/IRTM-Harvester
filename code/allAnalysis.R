rm(list=ls())

## Customize for your maachine:
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



dataPath <- "./Data/"
thetasPath  <-"./Results/"

## loads Grids and Theta summaries
load(paste0(thetasPath, "gridyear-Thetas-All.Rds"))

## helper to convert long-lat to PRIO grid system:
source("coordsToPrioGrid.R") 

## Data:

acled <-  read.csv(paste0(dataPath, "acled_africa.csv"), sep=";") ## 334,188 -- has field for PRIO grid
##ged <- read.csv(paste0(dataPath, "GEDEvent_v22_1.csv")) ### 293,634; uses PRIO-Grid
##scad <- read.csv(paste0(dataPath, "SCAD2018Africa_Final.csv")) ## 17,644; needs PRIO-post-processing

## Merge in Thetas by PRIO grid:
grids <- read.csv(paste0(dataPath, "priogrid_v1_01.csv"), sep=";") ## PRIO Grids

## Acled:
## Largest dataset, but actors are combined, which creates more information about activities
## but harder to pair with a theory of non-government mobilization

## mapping:

## Interaction codes indicating
## State only: 
## 10 (sole military), 11 (military vs military), 17 (military vs civilians)

state <- c(10, 11, 17)

## Codes with military-organized rebel/militias
## 12 (military vs rebels); 13 (military vs political militia); 14 (military vs communal militia)
## 20 (sole rebel action); 22 (rebels vs rebels); 23 (rebels vs political militia); 24 (rebels vs communal militia)
## 27 (rebel vs civilians); 28 (rebel vs others); 30 (sole political militia); 33 (political militia vs political militia);
# #34 (political militia vs communal militia);  37 (political militia vs civilians); 38 political militia vs others);
## 40 (sole communal militia action); 44 (communal militia vs communal militia)
## 47 (communal militia vs civilians); 48 (communal militia vs other)

org_non_state <-  c(12, 13, 14,
                    20, 22, 23, 24, 27, 28, 
                    30, 33, 34, 37, 38, 
                    40, 44, 47, 48)

## Codes with non-government operations:

## protests/riots
## 15 (military vs rioters); 16 (military vs protesters); 25 (rebels vs rioters); 26 (rebels vs protesters)
## 35 (political militia vs rioters); 36 (political militia vs protesters); 45 (communal militia vs rioters);
## 46 communal militia vs protesters
## 50 (sole rioter action); 55 (rioters vs rioters); 56 (rioters vs protesters); 57 (rioters vs civilians)'
## 58 (rioters vs others); 60 (sole protester action); 66 (protester vs protesters); 68 (protester vs other)

non_org_non_state <- c(15, 16, 
                       25, 26, 
                       35, 36, 
                       45, 46,
                       50, 55, 56, 57, 58, 
                       60, 66, 68)

## Add "Political Violence; Demonstrations" section to "Political violence'"

acled[which(acled$disorder_type=="Political violence; Demonstrations"),
      c("disorder_type")] <- "Political violence"

### Summarize events to the grid-interaction level:
acled_grids <- label_grid(acled) %>% ## attaches events to a PRIO grid
  dplyr::group_by(interaction, year, grid) %>% ## separate model for each "interaction" event
  dplyr::summarise(num_events=n()) 

## Remove the unneeded data:
rm(acled)
rm(grids)
rm(grid)


## Create an overarching grouping for interactions:
acled_grids$event_grouping <- "other"
acled_grids[which(acled_grids$interaction %in% state),"event_grouping"] <- "state"
acled_grids[which(acled_grids$interaction %in% org_non_state),"event_grouping"] <- "org_non_state"
acled_grids[which(acled_grids$interaction %in% non_org_non_state),"event_grouping"] <- "non_org_non_state"

table(acled_grids$event_grouping)

## merge in theta
acled_thetas <-merge(x=acled_grids, 
                     y=gridyear_Thetas,
                     by.x=c("grid", "year"), 
                     by.y= c("grid", "int.year"),
                     all=TRUE)

## Cells with thetas and no violence:

## Cells with no recorded violent or non-violent events
acled_thetas[is.na(acled_thetas$disorder_type), "disorder_type"]  <- "None" ## Type of event
acled_thetas[is.na(acled_thetas$num_events), "num_events"]  <- 0 ## number of events

## Interview rounds:
acled_thetas$int.round <- NA
acled_thetas[which(acled_thetas$year==2008), "int.round"] <- 4
acled_thetas[which(acled_thetas$year==2009), "int.round"] <- 4
acled_thetas[which(acled_thetas$year==2012), "int.round"] <- 5
acled_thetas[which(acled_thetas$year==2013), "int.round"] <- 5
acled_thetas[which(acled_thetas$year==2014), "int.round"] <- 6
acled_thetas[which(acled_thetas$year==2015), "int.round"] <- 6

## Crop to years around the interviews:
acled_thetas <- acled_thetas[which(acled_thetas$year > 2007 &
                                     acled_thetas$year < 2016),]

table(acled_thetas$year)

## Add in 0s for grid-years without violence or interviews:
acled_thetas_Lag= expand.grid(
  grid = sort(unique(acled_thetas$grid)), ## group for index
  int.round = seq(from = 4, ## Expand the dataframe so that there is a entry for each grid-round
                  to = 6)) %>% ## 
  ## merge the scaffold into the data: 
  left_join(acled_thetas, by = c("grid", "int.round")) %>%  ## Add location, event, and theta estimates
  arrange(grid, int.round) %>%
  group_by(grid)

## Add in 0s for interaction codes in grid with no events:
acled_thetas[is.na(acled_thetas$interaction), c("interaction")] <- 0
     
## 
acled_thetas_Lag <- acled_thetas_Lag %>%
  group_by(grid) %>% ## group by location 
  mutate( ## Create lags for previous value
    T1.q25.prevround = lag(T1.quant.25, order_by = int.round),
    T1.mean.prevround = lag(T1.mean,  order_by = int.round),
    T1.var.prevround = lag(T1.var,  order_by = int.round),
    T1.q75.prevround = lag(T1.quant.75, order_by = int.round),
    T2.q25.prevround = lag(T2.quant.25, order_by = int.round),
    T2.mean.prevround = lag(T2.mean, order_by = int.round),
    T2.q75.prevround = lag(T2.quant.75, order_by = int.round),
    T3.q25.prevround = lag(T3.quant.25, order_by = int.round),
    T3.mean.prevround = lag(T3.mean, order_by = int.round),
    T3.q75.prevround = lag(T3.quant.75, order_by = int.round),
  ) %>%  ## Also fill in location  metadata:
    fill(c(urbrur, country),
       .direction =c("updown"))

### Check the data for one grid:
## unit is year-interaction
## with number of events for each of the different 
## interaction codes
print(acled_thetas[which(acled_thetas$grid==80317),])

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

demonstrations <- acled_thetas_Lag %>%
  filter(event_grouping== "non_org_non_state") %>%
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

## Plot violence correlations
  v_cors_plot <- ggplot(v_cors,
                      aes(y= Correlation, 
                          x=EventType,
                          fill=Correlation)) +
   geom_bar(stat="identity", colour="black", show.legend = FALSE) +
  scale_fill_gradient2(low="lightgray", high="darkgray"", midpoint=0) +
  labs(x="\n ACLED Event Code",
       y="Correlation Theta 1 and Frequent Events in ACLED\n") +
    ggtitle("Correlations Between Average Theta 1 and ACLED Event Types")+
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, vjust=0.5))+
  facet_wrap(~Country,scales = "free")

v_cors_plot

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

### Data is too big to put in a single plot:
## Events 60 [protests], 37 [militia vs civilians], 13 [military vs militia],
## 15 [military vs rioters], 12 [military vs rebels] are the highest

event_focus = 60
v_cors_p3 <- ggplot(v_cors_2[which(v_cors_2$EventType==event_focus),],
                      aes(y= Correlation, 
                          x= Country,
                          fill=Correlation)) +
  geom_bar(stat="identity", colour="black", show.legend = FALSE) +
  scale_fill_gradient2(high="lightgray", low="darkgray"", midpoint=0) +
  labs(x="\nCountry", 
       y=paste0("Kendall Correlation Theta 1 and ACLED Event ", event_focus, "\n")) +
  ggtitle(paste0("Correlations Theta 1 and ACLED Event ", event_focus, "\n"))+
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, vjust=0.5)) +
  facet_wrap(~Round, nrow=3)+
  geom_hline(yintercept=0, linetype='dotted', col = 'gray')

v_cors_p3

library(plotly)

v_cors_p2 <- ggplot(v_cors_2,
                    aes(y= EventType, 
                        x= Country,
                        fill=Correlation)) +
  geom_tile() +
  scale_fill_gradient2(high="lightgray", low="darkgray"", midpoint=0) +
  labs(x="\nCountry", 
       y=paste0("Kendall Correlation Theta 1 and ACLED Events\n")) +
  ggtitle("Correlations Theta 1 and ACLED Events")+
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, vjust=0.5)) +
  facet_wrap(~Round, nrow=3, scales="free")+
  theme(legend.position="bottom")

v_cors_p2

ggplotly(v_cors_p2, tooltip="text")


##ggsave(v_cors_p2, file="MeanTheta1_violence_cors_byr.png")



ggsave(v_cors_p2, file="MeanTheta1_violence_cors_byr.png")

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


       
