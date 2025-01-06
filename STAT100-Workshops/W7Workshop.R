#Workshop wk 7
#ANOVA revision

mice<-read.csv(
  "mice_diets.csv"
)

head(mic)
names(mice)
summary(mice)
mice$group<-factor(mice$group, levels=c("Control","Casein","Soy")
attach(mice)
#exploratory plot - boxplot ofcourse
boxplot(IgG_intensity~group)
                   
#Conditions for an ANOVA:
#Independance: Assuming individual mice in each group
#               and that mouse weren't sample more than once
#Normality: 
qqnorm(IgG_intensity)
shapiro.test(IgG_intensity)
# Each of the groups have a normal distribution
# boxplot : Medians are roughly in centre of the IQR.
# qqplot: each of the groups have roughly straight points
#Equal variance: max sd/min sd <2, so constant variance
#is met.
aggregate(IgG_intensity~group, FUN=sd)
23328370/11851937
#all conditions for anova met, so can proceed with the test.
#run test:
mod.aov<-lm(IgG_intensity~group)
anova(mod.aov)
#The p-value (f_2,27 = 122.65, p=2.8 x 10^-14) is less
#than 0.05, so there is sufficient evidence to reject the Ha
#Therefore at least 1 mean IgG_intensity is different between three diets.

#pairwise t test:
pairwise.t.test(IgG_intensity,group,p.adjust.method = 'bonf')
#Caesin has significantly higher mean value

#checking for decision error with unadjusted p-value
1-pbinom(0,size=3,prob=0.05/3)
detach()

###################################################################
#Linear model intro: conditions
fake_lm<-read.csv("fake_lm_data.csv")
View(fake_lm)
str(fake_lm)

attach(fake_lm)
#checking conditions x and y1
plot(y1~x)
#3 points:
# linear: yes
# directions: positive
# strength: moderately strong (can easily see relationship)
cor(y1,x) #correlation value (close to 1)

lm1<-lm(y1~x)
abline(lm1, col='red')

#conditions:
#constant variance of residuals: width of the scatter in theresiduals
#                               is roughly even, no funneling or bunching
plot(lm1,which=1)
#Normality of residuals: scatter of residuals relatively straight with not much
#                         deviation from the theoretical line.
plot(lm1,which=2)
#Linearity: scatter in scatterplot is roughly straight
#Independence: randomly generated numbers, independence condition met
#all conditions for linear model have been met.

plot(y2~x)

#x and y2
plot(y2~x)
#linear: yes
#strength: overall moderate, stronger closer to lower x values
#         weaker closer to high x values
#direction: negative
cor(x,y2)
#make regression line
lm_2<-lm(y2~x)
plot(lm_2, which=1)
#non-constant variance because of evident fanning in the residuals
plot(lm_2,which=2)
#normality? no.
#non-normal residuals because scatter deviates widely from line
lm_3<-lm(y3~x)
plot(lm_3, which=1)
#Linear? no.
#Non-linear relationship, bend in the scatter
#constant variance, width of scatter fairly uniform
plot(lm_3,which=2)
#normal distribution, scatter is straight

#y4 and x
plot(y4~x)
#linear? no, slight bend
#stength: moderately-strong, weaker towards higher x values
#direction: positive (going up)
lm_4<-lm(y4~x)
abline(lm_4,col='red')
plot(lm_4,which=1)#linear residuals plot
#non linear, bend in scatter is obvious
#constant variance? no. narrower residuals at lower x values
#and wider residuals at higher x values
plot(lm_4,which=2)
#normality, not really with some high outliers
shapiro.test(lm_4$residuals) #p-value <0.05 confirms this normality
#some obs deviate widely from theoretical line

#would you run a linear regression? No.

#x and y5
plot(y5~x)
#3 points:
# linear: yes
# direction: negative
# strength: moderately-strong (can easily see trend)
cor(y5,x)
lm_5<-lm(y5~x)
plot(lm_5,which=1)
#variance is constant, even width of scatter
plot(lm_5, which=2)
#normal residuals distribution, scatter is relatively straight
#and falls on the theoretical line.
#Linear regression test? Yeah. 

#make sure to check independance for every test (especially in assignments)
detach()

##########################################################
## Extension
my_x<-runif(100,min=0,max=5)
my_y<-6+0.2*my_x+rnorm(100,mean=0,sd=1)#specified standard deviation
plot(my_y~my_x)
my_lm<-lm(my_y~my_x)
abline(my_lm,col="red")
plot(my_lm,which=1)
plot(my_lm,which=2)
