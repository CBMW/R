#workshop 6
#####################
#Miner birds paired t-test
#Ho: no difference in call length between
#     first part of call (di) and the second (dii)
#Ho: u_diff = 0, where diff = di-dii
#Ha: There is a difference between the first part of call and second
#ha: u_diff =/ 0, where diff = di-dii

birds<-read.csv("Full_miner_birds.csv")
attach(birds)

#calculate diff
diff<-(dii-di)
diff

boxplot(diff,ylim=c(0,0.15))

#check conditions
#Independence: <10% of miner bird pop in sample
#              No bird was measured more than once
#Normality:     with 30 obs and no extreme outliers,
#               condition is met
qqnorm(diff)
qqline(diff, col='red')

#calculate point estimate, test statistic, p-value
xbar<-mean(diff)
#standard dev
s<-(diff)
xbar
s
n<-length(diff)
n

se<-s/sqrt(n)
se

#assign null value
null_value<-0
t_stat<-(xbar-null_value)/se
t_stat
#calc p-value
2*pt(t_stat,df=n-1,lower.tail=F)# when t_stat is positive
2*pt(t_stat,df=n-1,lower.tail=T)# when t_stat is negative
#all in one calc p value
t.test(dii,di,paired=T)
t.test(diff)


#conclusion
#The p-value (t_29 = 14.63, p=6.4 x 10^-15) is less than
#0.05, we can therefor sufficient evidence to reject the
#null hypothesis. Therefore, there is a difference in call length
#between the first part of the call (di) and the second (dii).
#with 95% confidence, the second part of the call is between
#0.049 and 0.064msecs longer than the first part of the call.
detach()
###############################################################

zinc_paired<-read.csv("Zinc_paired.csv")
attach(zinc_paired)
#Ho: u=0, where diff = surface-bottom
#Ha: u=/0, where diff = surface-bottom
#check conditions
#independence: obs taken from different places
#               in the water tank.
#             <10% of the total water in the watertank
#normality:
diff2<-(Watertank1_surface-Watertank1_bottom)
diff2
boxplot(diff2, ylim=c(-0.18,0))
qqnorm(diff2)
#calculate test statistic etc
t.test(diff2)
#conclusion:
#the p-value (t_9 = -4.86, p=0.0009) is less than 0.05, so
#there is sufficient evidence to reject the null hypothesis
#Therefore, there is a difference in the concentration of zinc
#between the surface of watertank1 and bottom.
detach(zinc_paired)

###############################################################
#ONE WAY ANOVA:
PlantGrowth
str(PlantGrowth)
?PlantGrowth #info about dataset
attach(PlantGrowth)
#Hypotheses
#Ho: Mean weight is the same for all 3 treatments
#Ho: u_c = u_1 = u_2
#Ha: Atleast one mean weight of the treatment is different
#Independence: all different plants and no plant overlap in three treatments
#Normality
boxplot(weight~group, col=2:4)
qqnorm(weight, col=group, pch=19)
#constant variance
aggregate(weight,list(treatment=group), sd)
0.79/0.44
#1.8 <2, so constant variance is met
#running anova has 2 steps:
#make a linear model
mod.1<-lm(weight~group)
mod.1
#to get output
anova(mod.1)
#conclusion
#the p-value (f_2,27=4.85, p=0.02) is less than 0.05
#enough evidence to reject null hypotheses
#therefore, there is atleast one mean weight
#in treatment groups which are different from the others.
# Mean weight for treatment 1 is significantly  smaller than trt2.

#To find the differences
pairwise.t.test(weight,group, p.adjust.method='bonf')

#p=0.583, Ho: u_C = u_1 control and trt1 have the same mean weight
#p=0.263, Ho: u_C = u_2, control and trt2 have the same mean weight
#p0.013, Ho: u_1, = u_2, reject null, so trt1 and trt2 have different means

#have a look at the pvalues, and if any are <0.05, there are differences.
