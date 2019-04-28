#######################Analysis of ROST Begging Data
#######################AUTHOR: KAYLA DAVIS

#set working directory
#location on local computer where data files saved
setwd("G://My Drive//R Working Directory//ROST//Beg") 

#load libraries
library(lme4)
library(DHARMa)
library(lubridate)
library(chron)
library(dplyr)
library(tidyr)
library(glmmADMB)
library(MuMIn)
library(ggplot2)

#function to make time a decimal time so we can standardize more easily
decimateTime=function(time) {
  time=as.numeric(unlist(strsplit(time, ":")))
  time = time[1]*60+time[2]/60
  return(time)
}

#load data
#parent-offspring association analysis
parent<-read.table("HY_Parent_Begs.csv",header = TRUE,sep = ",",as.is=TRUE)

#begging events data
Focals<-read.table("ROST_BegData.csv",header = TRUE,sep = ",",as.is=TRUE)

#banding date data
band<-read.table("ROST_BegBandingData.csv",header = TRUE,sep = ",",as.is=TRUE)
##############################Parent-offspring association analysis############################################

#logistic regression to determine probability of begging at parent vs non-parent
#this analysis uses only begging observations where both parents are known
lr<-glmer(parent$Parent~1+(1|parent$HY), data=parent, family=binomial)
summary(lr)
confint(lr)

#model diagnostics
plot(lr)
plot(lr, HY~resid(.))


#calculate probability and standard error/confidence intervals
predict<-predict(lr,type="response")
mean(predict)
sd(predict)
se<-sd(predict)/sqrt(length(predict))
se*1.96
mean(predict)-se*1.96
mean(predict)+se*1.96



############################Begging Events Analysis##########################################################

#data formatting
#convert time and date
Focals$DATE <- as.Date(Focals$DATE, format="%m/%d/%Y")

#convert time to decimal time so we can scale it later
times=Focals$TIME
Focals$dec.time<-sapply(times,decimateTime)

#convert time back to real time and save for later
Focals$TIME <- strptime(Focals$TIME, format="%H:%M", tz = '')
Focals$TIME <- times(format(Focals$TIME,"%H:%M:%S"))

time_real <- Focals$TIME
summary(time_real)
sd(time_real)

#take a look at the data
head(Focals)
summary(Focals$DayNum)

#name variables
beg<-Focals$BEG #dependent variable for begging (beg=1, other=0)
day<-scale(Focals$DayNum) #Day number of season, 0-61 for 2014 and 0-59 for 2015
time<-scale(Focals$dec.time) #sample time in

#time<-scale(Focals$TIME)
hour<-scale(Focals$HOUR) #hour of sample time in

#make random effects factor variables
pfr<-factor(Focals$PFR)#PFR ID of HY ROST
length(unique(pfr))
site<-factor(Focals$SITE) #site codes

#create data frame with only above vars
df<-data.frame(beg, time, hour, day, site, pfr)

################################summary statistics#############################################

#look at the data
plot(beg~Focals$DayNum, data = df)
plot(beg~Focals$TIME, data = df)

#summarize beg by focal sample
summary(df$beg)

#standard error
sd(df$beg)/sqrt(length(df$beg))

#how many individuals were sampled?
length(unique(df$pfr))

#summary of time
summary(Focals$TIME)
boxplot(data=Focals, BEG~HOUR)

#summary of day
summary(Focals$DayNum)
boxplot(data=Focals, BEG~DayNum)

###############################Beg events models#################################################

nb1 <- glmmadmb(beg~day+time+I(day^2)+I(time^2)+(1|pfr)+(1|site), data = df, family = "nbinom1") 
summary(nb1)
confint(nb1)

nb2 <- glmmadmb(beg~day+time+(1|pfr)+(1|site), data = df, family = "nbinom1") 
summary(nb2)
confint(nb2)

nb3 <- glmmadmb(beg~(1|pfr)+(1|site), data = df, family = "nbinom1") 
summary(nb3)

anova(nb3, nb2, nb1)

# nb1.2 <- glmmadmb(beg~day+time+I(day^2)+I(time^2)+(1|pfr)+(1|site), data = df, family = "nbinom2") 
# summary(nb1.2) #nbinom1 fits better

AICtab<-AIC(nb1,nb2, nb3)

print(AICtab)

#Let's do some model diagnostics to look at how well the best model is fitting
r.squaredGLMM(nb2)

plot(nb2$residuals ~ df$day)
lines(lowess(nb2$residuals~df$day))

plot(nb2$residuals ~ df$time)
lines(lowess(nb2$residuals~df$time))

plot(predict(nb2)~nb2$residuals)
lines(lowess(predict(nb2)~nb2$residuals))


plot(nb2$fitted~nb2$residuals)
lines(lowess(nb2$fitted~nb2$residuals))

#let's examine the coefficients
coef(nb2)

#begging at mean day and time
exp(-0.25)

#effect of day
exp(-0.24) #Begging decreases by 21% with 1 standard deviation increase in day

#effect of time
exp(0.18) #Begging increases by 20% with every 1 standard deviation increase in time

#confidence intervals
exp(confint(nb2))

#compare AICc, do model selection
# use the mod.sel function to conduct model selection
# and put output into object out.put
out.put<-model.sel(nb1,nb2,nb3)
out.put.dataframe<-as.data.frame(out.put)

# coerce the object out.put into a data frame
sel.table<-as.data.frame(out.put.dataframe[6:10])
sel.table

#calculate likelihood
sel.table$Likelihood=exp(-0.5*sel.table$delta)

#calculate deviance
sel.table$Deviance = sel.table$logLik*-2

# a little clean-up, lets round things a bit
sel.table[,2:7]<- round(sel.table[,2:7],2)
sel.table

# number of parameters (df) should be K
names(sel.table)[1] = "K"

## lets be sure to put the model names in a column
sel.table$Model<-rownames(sel.table)

# replace Model name with formulas
for(i in 1:nrow(sel.table)) sel.table$Model[i]<-
  as.character(formula(paste(sel.table$Model[i])))[3]

# look at table
sel.table

#little reordering of columns
sel.table<-sel.table[,c(8,4,5,6,7,1)]
sel.table

# write to a file, here a comma separated values format
# make sure your working directory is properly specified
write.csv(sel.table,"BegModels.csv", row.names = F)

###############################################################################
###############################################################################
#predictions for day
newdata <- expand.grid(day= with(df, seq(min(day), max(day), length.out=31)),
                       time = mean(time))

Day<- seq(from = 204, to = 265, by=2)


# Calculate Predicitons with standard errors, and confidence intervals subsequently
pred.se <- predict(nb2, type="response", interval = "confidence", newdata = newdata, re.form = NA)
newdata$fit <- pred.se$fit
newdata$upr=pred.se$upr
newdata$lwr=pred.se$lwr

newdata$Day=Day

head(newdata)
tail(newdata)

#create a figure of predictions
Day.fig<-ggplot(newdata, aes(x = Day, y = fit)) + geom_ribbon(aes(ymin = lwr,
         ymax = upr),alpha=0.15, fill = "dodgerblue4") + 
  geom_line(size = 1.2, color = "dodgerblue4") + ylim(c(0, 3.2))+
  stat_sum(data = df, aes(x= Focals$DayNum + 204, y = beg)) + 
  theme_bw() +
  xlab("Day of Season") +
  ylab("Beg Events per Focal Sample") +
  scale_x_continuous(breaks=seq(204,265,5))+
  theme(axis.title.x = element_text(size = 19, vjust=-.5, face="bold")) + 
  theme(axis.title.y = element_text(size = 19, vjust=1.5, face="bold")) +
  theme(axis.text.x = element_text(size=16)) +
  theme(axis.text.y = element_text(size=16)) +
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major = element_blank()) +
  #theme(legend.position="none")+
  #theme(panel.border = element_blank())+
  theme(axis.line = element_line(colour = "black"))
Day.fig

#create a figure of predictions
Day.fig<-ggplot(newdata, aes(x = Day, y = fit)) + 
  stat_sum(data = df, aes(x= Focals$DayNum + 204, y = beg)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr),alpha=0.15, fill = "dodgerblue4") + 
  geom_line(size = 1.2, color = "dodgerblue4") + ylim(c(0, 15))+
  theme_bw() +
  xlab("Day of Season") +
  ylab("Beg Events per Focal Sample") +
  scale_x_continuous(breaks=seq(204,265,5))+
  theme(axis.title.x = element_text(size = 19, vjust=-.5, face="bold")) + 
  theme(axis.title.y = element_text(size = 19, vjust=1.5, face="bold")) +
  theme(axis.text.x = element_text(size=16)) +
  theme(axis.text.y = element_text(size=16)) +
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major = element_blank()) +
  theme(legend.position="none")+
  #theme(panel.border = element_blank())+
  theme(axis.line = element_line(colour = "black"))
Day.fig

#this code creates a higher resolution image and saves as png to filepath below
png(filename = "G://My Drive//R Working Directory//ROST//Beg//DayBegging_inset.png", width = 480 * 16, height = 480 * 12,  pointsize = 12 * 1.5, res = 600)
Day.fig
dev.off()


###############################################################################
###############################################################################
#predictions for time

newdata.t <- expand.grid(time= with(df, seq(min(time), max(time), length.out=13)),
                         day = mean(day))

Time<- seq(from = min(Focals$dec.time), to = max(Focals$dec.time), length.out = 13)

Time <- as.character(Time)
Time=strsplit(Time,"(?<=.)(?=[.])", perl = T)
Time = do.call(rbind.data.frame, Time)
colnames(Time)<-c("hour","min")
Time$hour=as.numeric(as.character(Time$hour))
Time$min=as.numeric(as.character(Time$min))




Time$hour = (Time$hour/60) 
Time$min = round(Time$min*60)
chr.time<-paste(Time$hour, Time$min, sep = ":")
#chr.time <- as.POSIXct(strptime(chr.time, format="%H:%M:%S"))
time <- strptime(chr.time, format="%H:%M", tz = '')
time <- times(format(time,"%H:%M:%S"))



# Calculate Predicitons with standard errors, and confidence intervals subsequently
pred.se.t <- predict(nb2, type="response", interval = "confidence", re.form = NA, newdata = newdata.t)

newdata.t$fit <- pred.se.t$fit
newdata.t$upr <- pred.se.t$upr
newdata.t$lwr <- pred.se.t$lwr
newdata.t$SE <- pred.se.t$se
newdata.t$upr=newdata.t$fit+1.96*newdata.t$SE
newdata.t$lwr=newdata.t$fit-1.96*newdata.t$SE


newdata.t$Hour=7:19

head(newdata.t)
tail(newdata.t)

#code for time figure
library(scales)
Time.fig<-ggplot(newdata.t, aes(x = time, y = fit)) + 
  stat_sum(data = df, aes(x= time, y = beg)) +
  geom_ribbon(aes(ymin = lwr,ymax = upr),alpha=0.2, fill="dodgerblue4") + 
  geom_line(size = 1.2, color="dodgerblue4") + ylim(c(0, 2.2))+xlim(c(7,19))+
  scale_x_continuous(breaks=seq(7,19,1))+
  theme_bw() +
  xlab("Hour of Day") +
  ylab("Beg Events per Focal Sample") +
  theme(axis.title.x = element_text(size = 19, vjust=-.5, face="bold")) + 
  theme(axis.title.y = element_text(size = 19, vjust=1.5, face="bold")) +
  theme(axis.text.x = element_text(size=16)) +
  theme(axis.text.y = element_text(size=16)) +
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major = element_blank()) +
  #theme(legend.position="none")+
  #theme(panel.border = element_blank())+
  theme(axis.line = element_line(colour = "black"))
Time.fig

ggplot(data = df, aes(x = time, y= beg))+
  stat_sum()
#this code creates a higher resolution image and saves as png to filepath below
png(filename = "G://My Drive//R Working Directory//ROST//Beg//TimeBegging.png", width = 480 * 16, height = 480 * 12,  pointsize = 12 * 1.5, res = 600)
Time.fig
dev.off()


##################################################################################################
#Appendix 1: correlation between banding date and days present at staging grounds

#get rid of error
band<-band[-c(11),]

#look at correlation between banding date and days of staging season
cor.test(band$DayNum, band$date.diff, method = "pearson")
plot(band$date.diff, band$DayNum, xlab = "Days Since Banding", ylab = "Day of Staging Season", pch = 16)
