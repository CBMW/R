#Workshop 2

#Read data
email50<-read.csv("email50.csv")
#Look at data
head(email50)
names(email50)
dim(email50)

#attach our data
attach(email50)

#Use to check what data is attached
search()

#scatterplot for a relationship {Line breaks : Number of chars}

plot(
  main="Compairing things from emails",
  line_breaks~num_char,
  xlab="Number of Chars",
  ylab="Number of Line-Breaks",
  col="deepskyblue3",
  bg="lightblue",
  pch=21,
  cex=2,
  lwd=1.5,
  )

#calc mean
mean(num_char)
mean(line_breaks)

#calc standard dev
sd(num_char)
sd(line_breaks)

#categorical can only get counts or proportions
table(spam)

#histograms
hist(num_char)
hist(num_char,
    xlab="Number of characters",
    ylab="Frequency",
    col="deepskyblue",
    breaks=3)

#Detach
detach(email50)

#iris data
#Variance
var(iris$Sepal.Width) #s^2
sqrt(0.1899794)
sqrt(var(iris$Sepal.Width))

#boxplot
attach(email50)
boxplot(num_char, ylab="Number of characters", col="deepskyblue")
summary(num_char)

#barplot
table(number)
barplot(table(number),
        xlab="Number",ylab="Count",
        col="darkorchid",
        ylim=c(0,40))
      
d <- c(31.8,25.3,41.2,1.9,33.2,44.5)
sd(d)
mean(d)
