#workshop 5
#One sample t-test miner birds
#read data in
miner<-read.csv("Full_miner_birds.csv")
attach(miner)
#exploratory analysis 
boxplot(Max_freq)
hist(Max_freq)

#write hypotheses:
#Ho: u = 2500hz
#Ha: u > 2500hz



#Checking conditions
#independence: Yes, assuming that they are 30 different birds randomly sampled
#                     <10% of the population in sample
#Normality: Yes, 30 obs with no extreme outliers
#                   Slightly right skewed
#                   qq-normal points are all relatively straight
#                   and fall close or on theoretical line
#                   shapiro-wilks p>0.05, so normally distributed
qqnorm(Max_freq)
qqnorm(Max_freq, col="orange")
#if not sure, run shapiro-wilks test
#Shapiro-wilks test: - formal test for normality
#Ho: the data is normally distributed
shapiro.test(Max_freq)

#write hypotheses:
#Ho: u = 2500hz
#Ha: u > 2500hz


#calculate point estimate, test statistic, p-value and CIs
#point estimate
xbar<-mean(Max_freq)
#SE
se<-sd(Max_freq)/sqrt(30)
#TS
t<-(xbar-2500)/se
#p-value
pt(t,df=29, lower.tail= FALSE) #greater than
1-pt(t,df=29)# same ^

#conclusion
#p-value (t_29 = 2.21, p=0.017) is less than 0.05, so 
#there is sufficient evidence to reject null hypotheses
#the max frequency of miner bird calls is greater than 2500hz

#calc CI
#tstar
tstar<-qt(0.025, df=29)

xbar-tstar*se
xbar+tstar*se

#with 95% confidence, mean maximum freq of miner bird calls
#is between 2513.12hz and 2829.31hz
#supports Null hypotheses

#using the test
t.test(Max_freq, mu=2500,alternative="greater")#greater than
t.test(Max_freq, mu=2500,alternative="less")#less than
t.test(Max_freq, mu=2500,alternative="two.sided")#some difference
#when running a one sided one-sample t-test, use the p-values 
#from the one-sided test, but, the CIs from the two-sided test.


####################################################
#Two sample t-test miner birds
#exploratory analysis
boxplot(di~Sex)

#Hypotheses
#Ho: u_f = u_m
#Ha: u_f /= u_m

#check conditions
#Independence: assume all different birds that were sampled randomly
#              assume no misidentification of sex, so no repeat
#              birds within groups
#              <10% of population
#Normality:    no or weak levels of skew in the two groups
#              no outliers
#              points in qq-normal plot relatively straight for
#              both sexes.
qqnorm(di, col=factor(Sex))

#calculate test statistics, etc
#means (point estimate)
aggregate(di,list(name=Sex),mean)
aggregate(di,list(name=Sex),sd)
aggregate(di,list(name=Sex),summary)

#test statistic, p-value, cI
t.test(di~Sex)

#Conclusion:
#The p-value (t_25.9= -2.5, p=0.019) is les than 0.05
#Sufficient evidence to reject Ha, therefor duration of the first part of the call 
#in miner birds is significantly different between the sexes.
#with 95% confidence, duration of the first part of the call is
#between 0.001 and 0.012 secs shorter for females

detach(miner)
#################################################
#one sample t-test  zinc conc in watertanks
zinc1<-read.csv("Zinc_Single.csv")
attach(zinc1)
View(zinc1)

#exploratory plot
boxplot(WT_2, ylim=c(0.5,0.9))

#Hypotheses
#Ho: u=0.5
#Ho: u>0.5

#conditions
#Independence: water samples taken from different areas of tank
#              <10% of population
#Normality: boxplot reasonably symmetrical and no outliers
qqnorm(WT_2)
qqline(WT_2)
#qq-normal points straight and close or on the theoretical line

#calculate test statistics
detach(zinc1)
############################################
#two sample t-test zinc concentration
zinc2<-read.csv("Zinc_2.csv")
#Make tank a factor
zinc2$Tank<-factor(zinc2$Tank)
View(zinc2)
attach(zinc2)

#exploratory plot
boxplot(Zinc_levels~Tank)

aggregate(Zinc_levels~Tank,FUN=mean)

#Hypotheses
#Ho: u_1 = u_2
#Ha: u_1 /= u_2

#Conditions
#Independence: assume samples taken from different areas of the tanks
#             assume water tanks on different run off systems
#             <10% population
#Normality: No skew for tank2, tank1 slightly left skewed
#           Points in qq are relatively straight fo both tanks
qqnorm(Zinc_levels,col=Tank)
t.test(Zinc_levels~Tank)

#conclusion: 
#P-value (t_12.13 = -4.5, p=0.0007) is less than 0.05, so there is sufficient evidence
#to reject Ha
#Therefor, mean zinc levels in tank 1 are significantly different to zinc levels in tank 2
