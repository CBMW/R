#week 8 workshop
possum<-read.csv("brush_tail_possums.csv")
str(possum)
table(possum$pop)
attach(possum)
#Relationships
#HeadL vs totalL
plot(headL~totalL)
plot(headL~totalL,
     xlab="Total length (cm)",
     ylab="Head length (mm)",
     col="purple",
     pch=15,
     cex=0.8,
     cex.lab=.8)
#descrribe relationship
#linear? yes
#strength? strong
#direction? positive
cor(headL,totalL)

#hypothesis testing
#hypotheses: Ho, there is no relationship between headL and totalL
#null hypotheses: Ha, there is a relationship between headL and totalL

#check conditions:
#linearity:
SLR<-lm(headL~totalL)
abline(SLR, col="red")
#normal distribution of residuals:
plot(SLR, which=2) #scatter is straight and mostly on line = normal dist
shapiro.test(SLR$residuals) #p-value > 0.05 = normal dist
#constant variance of residuals:
plot(SLR, which=1) #scatter is random and width is even horizontally
                  #so condition is met
                  #no influential outliers
#independence: possums were sampled randomly, no repeat measurements

#calculating test statistics
SLR<-lm(headL~totalL)
summary(SLR)

#beta 0 = 43.26
#beta 1 = 0.057
#r^2 = 0.45
#b_1 p-value = 7.95 x 10^-15 ~ 0

#conclusion
#pvalue (t_100 = 9.13, p = ~0) is less than 0.05, so there is
#evidence to reject the null Ho
#therefore there is  a relationship
# equation E(headL) = 43.26 + 0.57(totalL)
#about 45% fo the variability in headL is explained by totalL
confint(SLR)
#with 95% confidence, headL increases between 0.44 and 0.69mm
#on average for every 1cm increase in totalL.
#with 95% confidence, headL increases between 4.4 and 6.9mm
#on average for every 10cm increase in totalL

##########################
#tailL vs totalL

#exploratory analysis
plot(tailL~totalL,
     xlab="Total length (cm)",
     ylab="Tail Length (cm)",
     col="cyan2",
     pch="x"
)     
#Relationship
# linear yes
# direction positive
# stength moderately strong
cor(totalL,tailL)
# Ho: There is no relationship between tail length and head length
# Ha: There IS a relationship between tail length and head length

#Check conds
#normal distribution of residuals 
plot(SLR2, which=2) #straight scatter = yes

#linearity
SLR2<-lm(tailL~totalL)
abline(SLR2, col='red3')

#constant variance of residuals
plot(SLR2, which=1)
#plot confirms constant variance and no influential outliers

#independence: same as above

#calc test statistic and CI
summary(SLR2)
confint(SLR2)
#p-value (t_100 = 6.82, p=7.12 x 10^-10) is les than 0.05,
#reject null
#there IS a positive relationship between total L and tailL
#w 95% confidence, tail L increases between 0.19 and 0.34cm on avg fore each 1cm
#increase in totalL

#intercept: with 95% confidence when totalL is 0cm, tailL is between
#7.38 and 20.76cm

#32% of the variability in tailL is explained by totalL.


