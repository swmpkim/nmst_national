dat<-read.csv(here::here("data", "compiled", "slopesAndExpl_bySite.csv"))
names(dat)
head(dat)
attach(dat)

library(nlme)
library(MuMIn)
library(car)

# Start with VIF analysis
#variance Inflation factor analysis # https://www.r-bloggers.com/2023/12/exploring-variance-inflation-factor-vif-in-r-a-practical-guide/


################# Option 1 - Remove 15 sites with missing data (Condition and SET)
dat2<-dat[-c(6,17,23,24,25,26,27,28,46,47,48,49,50,52,53), ] # 15 sites removed due to SET data absence!!

mod.lme <- lme(EIR_slope~Geomorphology+Salinity_category+SLR_last19yrs+Tidal_Range+NERRs_Landscape_resiliency_condition_sum_quantile+SET_minus_SLR_19yrs+proportion_low+NERR_Region, random =~1|Reserve, data=dat2, method="REML") #use REML when looking at final model, use ML when comparing
vif_values<-vif(mod.lme)
vif_values
#Results: NERR_Region GVIF 107.3
#Action: remove NERR_Region 

mod.lme <- lme(EIR_slope~Geomorphology+Salinity_category+SLR_last19yrs+Tidal_Range+
                   NERRs_Landscape_resiliency_condition_sum_quantile+
                   SET_minus_SLR_19yrs+proportion_low, 
               random =~1|Reserve, data=dat2, method="REML") #use REML when looking at final model, use ML when comparing
vif_values<-vif(mod.lme)
vif_values
#Results: GVIF all less than 5
# Action: Run model

# Kim additions: check residuals
resids <- residuals(mod.lme, type = "pearson")
hist(resids)
# there's that one outlier but otherwise looks good
# residuals by each predictor
# multiple graphs on a page
par(mfrow = c(2, 4))
preds <- c("Geomorphology", "Salinity_category", "SLR_last19yrs", "Tidal_Range", 
           "NERRs_Landscape_resiliency_condition_sum_quantile", 
           "SET_minus_SLR_19yrs", "proportion_low")
for(i in seq_along(preds)){
    preds_toplo <- dat2[[preds[i]]]
    if(class(preds_toplo) %in% c("numeric", "integer", "double")){
        plot(resids ~ preds_toplo, xlab = preds[i])
        abline(h = 0)
    } else {
        boxplot(resids ~ preds_toplo, xlab = preds[i])
    }
}
# back to normal
par(mfrow = c(1, 1))

# I don't really like that I'm seeing patterns in most of these residuals by predictors
# a bit of a cone for SLR_last 19 years
# big variation in landscape resiliency condition
# and that big outlier


# another Kim addition: center and standardize predictors before modeling and dredging
# divide by 2 sds, based on Gelman et al. recommendations
# puts numeric variables on a scale with mean 0 and sd 0.5
library(arm)
library(dplyr)
dat2<-dat[-c(6,17,23,24,25,26,27,28,46,47,48,49,50,52,53), ]
dat3 <- dat2 %>% 
    mutate(across(Total.unvegetated_slope:last_col(),
                  arm::rescale))
# will re-run models below their runs that Alice set up, using this data frame instead



#full model
mod.lme <- lme(EIR_slope~Geomorphology+Salinity_category+SLR_last19yrs+Tidal_Range+NERRs_Landscape_resiliency_condition_sum_quantile+SET_minus_SLR_19yrs+proportion_low
               +Salinity_category:SLR_last19yrs+Tidal_Range:SLR_last19yrs+Geomorphology:SET_minus_SLR_19yrs, random =~1|Reserve, data=dat2, method="ML") #use REML when looking at final model, use ML when comparing
mod.lme.scaled <- lme(EIR_slope~Geomorphology+Salinity_category+SLR_last19yrs+Tidal_Range+NERRs_Landscape_resiliency_condition_sum_quantile+SET_minus_SLR_19yrs+proportion_low
                      +Salinity_category:SLR_last19yrs+Tidal_Range:SLR_last19yrs+Geomorphology:SET_minus_SLR_19yrs, random =~1|Reserve, data=dat3, method="ML") #use REML when looking at final model, use ML when comparing

# check out residuals etc.
performance::check_model(mod.lme.scaled)
# hm, have a couple high vifs
vif_values<-vif(mod.lme.scaled)
vif_values
# Tidal range and its interaction with SLR
# remove tidal range??
# first check residuals
resids <- residuals(mod.lme, type = "pearson")
hist(resids)
# there's that one outlier but otherwise looks good
# residuals by each predictor
# multiple graphs on a page
par(mfrow = c(2, 4))
preds <- c("Geomorphology", "Salinity_category", "SLR_last19yrs", "Tidal_Range", 
           "NERRs_Landscape_resiliency_condition_sum_quantile", 
           "SET_minus_SLR_19yrs", "proportion_low")
for(i in seq_along(preds)){
    preds_toplo <- dat3[[preds[i]]]
    if(class(preds_toplo) == "numeric"){
        plot(resids ~ preds_toplo, xlab = preds[i])
    } else {
        boxplot(resids ~ preds_toplo, xlab = preds[i])
    }
}
# back to normal
par(mfrow = c(1, 1))
# generally better but I still don't like the SET_minus_SLR one - bit of a downward slope
# also a little bit of funneling still with SLR itself
# and that one category of landscape resiliency.


#dredge full model to show all subset models and compare with AIC
dd <- dredge(mod.lme)
dd.scaled <- dredge(mod.lme.scaled)

# Kim additions: check out the model set
# 260 candidate models
# how many have delta < 2: 2
sum(dd$delta < 2)
# delta < 4: 9
sum(dd$delta < 4)
# delta < 6: 24
sum(dd$delta < 6)
# 95% confidence set
dd2 <- dd
dd2$cumuwt = cumsum(dd$weight)

as.data.frame(dd2) |> 
    mutate(rownumber = 1:nrow(dd2)) |> 
    select(rownumber, delta, weight, cumuwt) |> 
    filter(cumuwt >= 0.95) |> 
    head()
# 75 models

# look at variable weights in 95% confidence set
sw(dd[1:74, ])


#show AIC results and ranking for all models with delta AIC <2
subset(dd, delta < 2)

best<-lme(EIR_slope~Salinity_category+SLR_last19yrs, random =~1|Reserve, data=dat2, method="REML") #use REML when looking at final model, use ML when comparing
summary(best)

library(ggplot2)

ggplot(dat2,aes(x=as.factor(Salinity_category),y=EIR_slope))+
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Salinity")

plot(dat2$SLR_last19yrs,dat2$EIR_slope)


########################################################### Option 2 remove variable, keep data


option2 <- lme(EIR_slope~Geomorphology+Salinity_category+SLR_last19yrs+Tidal_Range+proportion_low+NERR_Region, random =~1|Reserve, data=dat, method="REML") #use REML when looking at final model, use ML when comparing
vif_values<-vif(option2)
vif_values

# Results: Region large GVIF (79)
#Action: remove region 

option2 <- lme(EIR_slope~Geomorphology+Salinity_category+SLR_last19yrs+Tidal_Range+proportion_low, random =~1|Reserve, data=dat, method="REML") #use REML when looking at final model, use ML when comparing
vif_values<-vif(option2)
vif_values

#Results: Tidal Range is large GVIF (7.9) 
#Action: Remove Tidal Range
option2 <- lme(EIR_slope~Geomorphology+Salinity_category+SLR_last19yrs+proportion_low, random =~1|Reserve, data=dat, method="REML") #use REML when looking at final model, use ML when comparing
vif_values<-vif(option2)
vif_values
#Results: all GVIF <5
#Action: un model with interaction terms

#starting model
option2 <- lme(EIR_slope~Geomorphology+Salinity_category+SLR_last19yrs+proportion_low
               +Salinity_category:SLR_last19yrs, random =~1|Reserve, data=dat, method="ML") #use REML when looking at final model, use ML when comparing

dd <- dredge(option2)
subset(dd, delta < 2)

#Results: 2 models withing delta AIC <2
#Simplest<- lme(EIR_slope~Salinity_category+SLR_last19yrs, random =~1|Reserve, data=dat, method="ML")
#comparable<- lme(EIR_slope~Geomorphology+Salinity_category+SLR_last19yrs, random =~1|Reserve, data=dat, method="ML")

best<- lme(EIR_slope~Salinity_category+SLR_last19yrs, random =~1|Reserve, data=dat, method="REML")
summary(best)
library(ggplot2)

ggplot(dat,aes(x=as.factor(Salinity_category),y=EIR_slope))+
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Salinity")

aggregate(SLR_last19yrs ~ Salinity_category, data, function(x) min(x))
aggregate(SLR_last19yrs ~ Salinity_category, data, function(x) max(x))

#xlim not working
pdf(file="O:/SSNERR/Science/Projects_Active/Sentinel Sites/synthesis/NAMASTE_MarshSynthesis_Peter/Analysis Files/Namaste_data/LME_results_scatterplot.pdf",height=6,width=8) #CHANGE name of folder within Edited and CHANGE file name "name_plots.pdf"
plot(SLR_last19yrs,EIR_slope,xlab="Rate of SLR over 19 years", ylab="Ecotone Encroacher Ratio",main="All sites included",pch=16)
abline(-0.03133476,0.00171935,lty=2,col="grey",xlim=c(1.41,9.11)) #Fresh
abline((-0.03133476+0.01685420),0.00171935,lty=2,col="green",xlim=c(-7.87,10.98)) #Oligohaline
abline((-0.03133476+0.04128165),0.00171935,lty=2,col="blue",xlim=c(-7.87,10.98)) #Mesohaline
abline((-0.03133476+0.03300885),0.00171935,lty=2,col="black",xlim=c(1.41,10.98)) #Polyhaline
legend("topleft",c("Fresh","Oligohaline","Mesohaline","Polyhaline"),xpd = TRUE,cex=1.2, pch=c(16,16,16,16),lty=2, col=c("grey","green","blue","black"),merge=TRUE)
dev.off()


### Option 1 a - all veg cover response, remove data, keep variables

dat2<-dat[-c(6,17,23,24,25,26,27,28,46,47,48,49,50,52,53), ] # 15 sites removed due to SET data absence!!
mod.lme <- lme(Total.live.veg_slope~Geomorphology+Salinity_category+SLR_last19yrs+Tidal_Range+NERRs_Landscape_resiliency_condition_sum_quantile+SET_minus_SLR_19yrs+proportion_low
               +Salinity_category:SLR_last19yrs+Tidal_Range:SLR_last19yrs+Geomorphology:SET_minus_SLR_19yrs, random =~1|Reserve, data=dat2, method="ML") #use REML when looking at final model, use ML when comparing
dd <- dredge(mod.lme)
subset(dd, delta < 2)
# 8 models with delta AIV <2

# option 2 a - all veg cover response, keep data, remove variables
option2a <- lme(Total.live.veg_slope~Geomorphology+Salinity_category+SLR_last19yrs+proportion_low
               +Salinity_category:SLR_last19yrs, random =~1|Reserve, data=dat, method="ML") #use REML when looking at final model, use ML when comparing

dd <- dredge(option2a)
subset(dd, delta < 2)

#results: two models with delta AIC<2
best<- lme(Total.live.veg_slope~Salinity_category+proportion_low
                , random =~1|Reserve, data=dat, method="REML") #use REML when looking at final model, use ML when comparing
summary(best)
library(ggplot2)

ggplot(dat,aes(x=as.factor(Salinity_category),y=Total.live.veg_slope))+
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Salinity")

plot(proportion_low,Total.live.veg_slope)

