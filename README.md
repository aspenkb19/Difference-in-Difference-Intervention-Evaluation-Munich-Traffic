# Difference-in-Difference-Intervention-Evaluation-Munich-Traffic

## Background

City planners implemented an intervention to reduce road traffic injuries in the Massmanpark area of Munich.The intervention was the construction of a bypass road, which opened at the beginning of 2011, which would move much of the automobile traffic away from the surrounding residential areas.

Use difference-in-differences (DiD) design to assess whether this intervention led to a reduction in road traffic injuries. This will include further analyses with the aim of strengthening the overall causal argument.

```{r, message=FALSE, warning=FALSE}

library(dplyr)
library(ggplot2)

didtraffic <- read.csv("https://www.sg.tum.de/fileadmin/tuspfsp/php/csv/assignment3_did.csv")


didtraffic <- mutate(didtraffic, year = as.factor(year),
                 site = as.factor(site),
                 inj = as.numeric(inj),
                 tick = as.numeric(tick))

didtraffic[,1] <- NULL



head(didtraffic)
```
The dataset comprises the following variables:

* inj: number of injuries that occurred during a specific observation point
* month: an indicator variable (1:24) representing the month in which an observation occurred
* year: a factor variable indicating whether the observation is from the pre or post-intervention period (2010 and 2011, respectively)
* site: an indicator variable representing whether an observation is from the intervention site ('mas'- Massmanpark), or one of the two control sites ('hoh'- Hohenzollernplatz, 'pri'- Prinzregentenplatz)
* ticket: number of speeding tickets given out during a specific observation point

## Task 1

Provide a graphical portrayal of this impact evaluation.Hohenzollernplatz is the main control site. 

### My Solution for Task 1: 

```{r, message=FALSE, warning=FALSE}
didtraffic_filter <- filter(didtraffic, site != "pri")

ggplot(didtraffic_filter, aes(x = year, y = inj, color = site)) +
  stat_summary(fun=mean, geom="point", size=12) +
  theme_classic()

```
This graph illustrates that the control and intervention sites started at similar injury rates, but after the
implementation of the intervention in 2011, injuries decreased significantly in the intervention group.

## Task 2:

Using basic linear regression, calculate the DiD estimator assessing whether the bypass road led to a reduction in traffic injuries - for this main analysis, Hohenzollernplatz is the control site.

Also calculate the DiD estimator manually to confirm that the regression is correct.

### My Solution for Task 2:

```{r, message=FALSE, warning=FALSE}
#First difference: intervention outcome vs baseline
#Second difference: control outcome vs baseline
#Third difference: first difference - second difference

#DiD estimator

hoh_baseline <- mean(didtraffic_filter$inj[didtraffic_filter$year == '2010' & 
                                         didtraffic_filter$site == 'hoh'])
hoh_follow <- mean(didtraffic_filter$inj[didtraffic_filter$year == '2011' & 
                                       didtraffic_filter$site == 'hoh'])
mas_baseline <- mean(didtraffic_filter$inj[didtraffic_filter$year == '2010' &
                                       didtraffic_filter$site == 'mas'])
mas_follow <- mean(didtraffic_filter$inj[didtraffic_filter$year == '2011' &
                                     didtraffic_filter$site == 'mas'])

hoh_baseline
hoh_follow
mas_baseline
mas_follow

diff1 <- mas_follow - mas_baseline
diff2 <- hoh_follow - hoh_baseline
diff3 <- diff1 - diff2

# Linear regression

did_lm <- lm(inj ~ site + year + site*year, data = didtraffic_filter)
summary(did_lm)

```
In relation to Hohenzollernplatz, the number of injuries at Massamanpark decreased by 105.95, according to both the DiD estimator and linear regression. 

## Task 3

Provide a graphical portrayal which demonstrates whether these trends were parallel in the control and intervention sites.

### My Solution for Task 3:

```{r, message=FALSE, warning=FALSE}
ggplot(didtraffic_filter, aes(x = month, y = inj, color = site)) +
  stat_summary(fun=mean, geom="point", size=12) +
  theme_classic()

```
In the pre-intervention period, the outome trends between Massaman Park and Hohenzollernplatz were parallell, then began to diverge at the time of the intervention, with injury rates decreasing much faster at Massamanpaprk. 

## Task 4

Use the additional control site to conduct a placebo analysis (using basic linear regression), and comment on what this placebo analysis means for the strength of the causal argument.

### My Solution for Task 4:

```{r, message=FALSE, warning=FALSE}
did_lm_month <- lm(inj ~ site + month + site*month, data = didtraffic_filter)
summary(did_lm_month)

```
##Task 5

Conduct a placebo analysis using the number of speeding tickets as a placebo outcome (using basic linear regression). 

##My Solution for Task 5:
    
```{r, message=FALSE, warning=FALSE}
did_lm_tick <- lm(tick ~ site + month + site*month, data = didtraffic_filter)
summary(did_lm_tick)
```
### Task 6

Discuss the impact of the bypass road on road traffic injuries in the Massmanpark area of Munich, and the extent to which the additional analyses strengthen the causal argument.

#### My Solution for Task 6:

The bypass road had the effect of both decreasing injuries by 7.2 per month, in comparison to the control site,
and also decreased the number of speeding tickets by 4.5 per month. The additional analyses atrengthen the causal argument by showing the effect per month on average in the intervention site. 

