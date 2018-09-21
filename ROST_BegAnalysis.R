#######################Analysis of ROST Begging Data for Behavioral Ecology Paper
#######################AUTHOR: KAYLA DAVIS


#set working directory
#location on local computer where data files saved
setwd("") 

#load libraries
#these packages must be installed first before this can run
library(lme4)
library(plyr)
library(DHARMa)
library(MuMIn)
library(ggplot2)

#load data
#parent-offspring association analysis
parent<-read.table("HY_Parent_Begs.csv",header = TRUE,sep = ",",as.is=TRUE)

#begging events data
Focals<-read.table("ROST_BegData.csv",header = TRUE,sep = ",",as.is=TRUE)


##############################Parent-offspring association analysis############################################
lr<-glmer(parent$Parent~(1|parent$HY), data=parent, family=binomial)
summary(lr)

predict<-predict(lr,type="response")
mean(predict)
sd(predict)
se<-sd(predict)/sqrt(length(predict))
se*1.96
mean(predict)-se*1.96
mean(predict)+se*1.96

############################Begging Events Analysis##########################################################
#convert time
format(Focals$TIME, format="%H:%M")
class(Focals$TIME) #the above function creates a character vector
Focals$TIME<-as.POSIXct(Focals$TIME, tz = "",format="%H:%M")

#take a look at the data
head(Focals)

#name variables
pfr<-Focals$PFR #PFR ID of HY ROST
day<-Focals$DayNum #Day number of season, 0-61 for 2014 and 0-59 for 2015
time<-Focals$TIME #sample time in
site<-Focals$SITE #site codes
beg<-Focals$BEG #dependent variable for begging (beg=1, other=0)
hour<-Focals$HOUR #hour of sample time in

#make pfr and site factors
pfr<-factor(pfr)
site<-factor(site)
#scale day and time
day_z<-scale(day)
time_z<-scale(time)
hour_z<-scale(hour)
#create data frame with only above vars
df<-data.frame(beg,time,time_z,hour, hour_z, day,day_z,site,pfr)

#summarize beg by focal sample
summary(df$beg)
#standard error
sd(df$beg)/sqrt(length(df$beg))

#how many individuals were sampled?
length(unique(df$pfr))

#this starts a function to run all models in candidate set in sequence
#some models below did not converge on first run
#ran update code to adjust starting values from previous fit and increased number of iterations
#delete # from summaries after model runs to see individual model results
cand.set<-list(
  m1.nb<-glmer.nb(beg~day_z+hour_z+I(day_z^2)+I(hour_z^2)+(1|pfr)+(1|site), data = df),
  #summary(m1.nb)
  ss1 <- getME(m1.nb,c("theta","fixef")),
  m1.nb.update <- update(m1.nb,start=ss1,control=glmerControl(optCtrl=list(maxfun=2e4))),
  #summary(m1.nb.update)
  
  m2.nb<-glmer.nb(beg~day_z+hour_z+I(hour_z^2)+(1|pfr)+(1|site), data = df),
  #summary(m2.nb)
  ss2 <- getME(m2.nb,c("theta","fixef")),
  m2.nb.update <- update(m2.nb,start=ss2,control=glmerControl(optCtrl=list(maxfun=2e4))),
  #summary(m2.nb.update)

  m3.nb<-glmer.nb(beg~day_z+hour_z+I(day_z^2)+(1|pfr)+(1|site), data = df),
  #summary(m3.nb)
  ss3 <- getME(m3.nb,c("theta","fixef")),
  m3.nb.update <- update(m3.nb,start=ss3,control=glmerControl(optCtrl=list(maxfun=2e4))),
  #summary(m3.nb.update)
  
  m4.nb<-glmer.nb(beg~day_z+hour_z+(1|pfr)+(1|site), data = df),
  #summary(m4.nb)
  ss4 <- getME(m4.nb,c("theta","fixef")),
  m4.nb.update <- update(m4.nb,start=ss4,control=glmerControl(optCtrl=list(maxfun=2e4))),
  #summary(m4.nb.update)
  
  m5.nb<-glmer.nb(beg~day_z+I(day_z^2)+(1|pfr)+(1|site), data = df),
  #summary(m5.nb)
  ss5 <- getME(m5.nb,c("theta","fixef")),
  m5.nb.update <- update(m5.nb,start=ss5,control=glmerControl(optCtrl=list(maxfun=2e4))),
  #summary(m5.nb.update)
  
  m6.nb<-glmer.nb(beg~hour_z+I(hour_z^2)+(1|pfr)+(1|site), data = df),
  #summary(m6.nb)
  ss6 <- getME(m6.nb,c("theta","fixef")),
  m6.nb.update <- update(m6.nb,start=ss6,control=glmerControl(optCtrl=list(maxfun=2e4))),
  #summary(m6.nb.update)
  
  m7.nb<-glmer.nb(beg~(1|pfr)+(1|site), data = df),
  #summary(m7.nb)
  ss7 <- getME(m7.nb,c("theta","fixef")),
  m7.nb.update <- update(m7.nb,start=ss7,control=glmerControl(optCtrl=list(maxfun=2e4))))
  #summary(m7.nb.update)

#model diagnostics for glmms
#check global model
sim_residuals <- simulateResiduals(m1.nb.update, 1000)
plot(sim_residuals)

testDispersion(sim_residuals)
testZeroInflation(sim_residuals)
testUniformity(sim_residuals)

#compare AICc, do model selection and model averaging
# use the mod.sel function to conduct model selection
# and put output into object out.put
out.put<-model.sel(m1.nb.update,m2.nb.update,m3.nb.update,m4.nb.update,m5.nb.update,m6.nb.update,m7.nb.update)
out.put.dataframe<-as.data.frame(out.put)

# coerce the object out.put into a data frame
sel.table<-as.data.frame(out.put.dataframe[8:12])
sel.table 

#calculate likelihood
sel.table$Likelihood=exp(-0.5*sel.table$delta)

# a little clean-up, lets round things a bit
sel.table[,2:6]<- round(sel.table[,2:6],2)
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
sel.table<-sel.table[,c(7,1,2,3,4,5,6)]
sel.table 

# write to a file, here a comma separated values format
# make sure your working directory is properly specified
write.csv(sel.table,"BegModels.csv", row.names = F) 

###############################################################################
###############################################################################
#model averaged predictions for day
newdata <- expand.grid(day_z= with(df, seq(min(day_z), max(day_z), length.out=61)),
                       hour_z = mean(hour_z))

Day_ma<- with(df, seq(from = 0, to = 60, length.out = 61))

#model average over all models
avg<-model.avg(m1.nb.update,m2.nb.update,m3.nb.update,m4.nb.update,m5.nb.update,m6.nb.update,m7.nb.update)

# Calculate Predicitons with standard errors, and confidence intervals subsequently
pred.se <- predict(avg, type="response", se.fit=TRUE, re.form=NA, full=T, newdata)
newdata$fit <- pred.se$fit
newdata$SE <- pred.se$se
newdata$upr=newdata$fit+1.96*newdata$SE
newdata$lwr=newdata$fit-1.96*newdata$SE
newdata$Day=Day_ma

head(newdata)
tail(newdata)

#create a figure of model-averaged predictions
Day.fig<-ggplot(newdata, aes(x = Day, y = fit)) + geom_ribbon(aes(ymin = lwr,
                ymax = upr),alpha=0.2) + geom_line(size = 2) + ylim(c(0, 4))+
  theme_bw() +
  xlab("Day of Season") +
  ylab("Beg Events per Focal Sample") +
  scale_x_continuous(breaks=seq(0,60,10))+
  theme(axis.title.x = element_text(size = 19, vjust=-.5, face="bold")) + 
  theme(axis.title.y = element_text(size = 19, vjust=1.5, face="bold")) +
  theme(axis.text.x = element_text(size=16)) +
  theme(axis.text.y = element_text(size=16)) +
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major = element_blank()) +
  theme(legend.position="none")
Day.fig

#this code creates a higher resolution image and saves as png to filepath below
png(filename = "~//DayBegging.png", width = 480 * 16, height = 480 * 12,  pointsize = 12 * 1.5, res = 600)
Day.fig
dev.off()


###############################################################################
###############################################################################
#model averaged predictions for time
newdata.t <- expand.grid(hour_z= with(df, seq(min(hour_z), max(hour_z), length.out=25)),
                         day_z = mean(day_z))

Time_ma<- with(df, seq(from = 7, to = 19, length.out = 25))

# Calculate Predicitons with standard errors, and confidence intervals subsequently
pred.se.t <- predict(avg, type="response", se.fit=TRUE, re.form=NA, full=T, newdata.t)
newdata.t$fit <- pred.se.t$fit
newdata.t$SE <- pred.se.t$se
newdata.t$upr=newdata.t$fit+1.96*newdata.t$SE
newdata.t$lwr=newdata.t$fit-1.96*newdata.t$SE
newdata.t$Hour=Time_ma

head(newdata.t)
tail(newdata.t)

#code for time figure
Time.fig<-ggplot(newdata.t, aes(x = Time_ma, y = fit)) + geom_ribbon(aes(ymin = lwr,
                 ymax = upr),alpha=0.2) + geom_line(size = 2) + ylim(c(0, 3))+xlim(c(7,19))+
  scale_x_continuous(breaks=seq(1,24,1))+
  theme_bw() +
  xlab("Hour of Day") +
  ylab("Beg Events per Focal Sample") +
  theme(axis.title.x = element_text(size = 19, vjust=-.5, face="bold")) + 
  theme(axis.title.y = element_text(size = 19, vjust=1.5, face="bold")) +
  theme(axis.text.x = element_text(size=16)) +
  theme(axis.text.y = element_text(size=16)) +
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major = element_blank()) +
  theme(legend.position="none")
#this code creates a higher resolution image and saves as png to filepath below
png(filename = "~//TimeBegging.png", width = 480 * 16, height = 480 * 12,  pointsize = 12 * 1.5, res = 600)
Time.fig
dev.off()
