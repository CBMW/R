#Workshop 3
#Normal distribution calculations

#P(z<-1.35) on standard normal dist
pnorm(-1.35, mean=0, sd=1, lower.tail=TRUE)
pnorm(-1.35) #^^^ Default settings for pnorm()
#8.9% that Z<-1.35

#P(Z>1.48) on std norm dist
pnorm(1.48, mean=0, sd=1, lower.tail=FALSE)#Upper tail
1-pnorm(1.48)#^^ Same same
diff(pnorm(c(-0.4,1.5), mean=0, sd=1,lower.tail = T))#Only for inner percentiles
1-pnorm(-0.4, mean=0, sd=1, lower.tail = TRUE)-pnorm(1.5,mean=0,sd=1,lower.tail=FALSE) # Same as..
pnorm(1.5)-(pnorm(-0.4)) # Same as ^
#58.9% <1.5 and >-0.4


#For absolute value
#P(|Z|>2) on std normal dist

2*pnorm(-2)

#N(45,3.2) P(X>50)
pnorm(50,mean=45,sd=3.2,lower.tail=FALSE) #or
1-pnorm(50,mean=45,sd=3.2,lower.tail=TRUE) #this = same answer

#N(89.15) P(X>100)
1-pnorm(100,mean=89,sd=15)
#23% of chemistry textbooks are above $100


#binomial distribution
# "Atleast" 1 nun, so 1-P(N=0)
1-dbinom(0,size3 prob=0.25)#(X=X*)
#atleast 1 nun
1-pbinom(0, size=3, =prob=0.25)#(X</=X*)
#if you have atleast 2 nuns, you would have to use pbinom command
#dbinom = exactly
#pbinom = at most (a range of)

# "Exactly" 2 nuns
dbinom(2,size=3,prob=0.25)

#"Exactly" 1 hei
dbinom(1,size=3,prob=0.25)

#"At most" 2 gimels
pbinom(2,size=3,prob=.25)

#"at least" 2 gimels
1-pbinom(1,size=3,prob=0.25)


#graphing
rugby<-read.csv("rugby_fitness.csv")
str(rugby)
attach(rugby)
hist(Jump_height,main="")
hist(Jump_height,main="", breaks=5)
hist(Jump_height,main="", 
     breaks=8,
     col="wheat",
     ylab="Jump height (cm)",
     xlab="Frequency"
     )
#Is there a difference between something? Use a boxplot
boxplot(Sprint1_10m)
summary(Sprint1_10m)
par(mfrow=c(1,3))
boxplot(Sprint1_20m,main="2nd 10m")
boxplot(Sprint1_10m,main="1st 10m")
boxplot(Sprint1_total, main="Total 20m")
par(mfrow=c(1,1))
boxplot(Height~Position)
boxplot(Yoyo_stage~Position, 
        col=c("wheat","pink","orangered","orange3","red4"),
        ylab="yo-yo stage",
        xlab="Position played")

#Scatterplots are good for relationships
plot(
  Agility_right~Sprint1_total,
  xlab="20m sprint time total",
  ylab="Agility in right direction(m)",
  pch=19,
  col="chocolate")
Position<-factor(Position)
pairs(~Jump_height+Sprint1_total+Agility_left+Agility_right+Yoyo_stage,
      col=Position)
