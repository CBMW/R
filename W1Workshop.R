#Workshop 1
#Intro to RStudio 
plot(discoveries)
#Colour & Line Width
plot(discoveries, col="magenta", lwd=2)

#Co2 Datasets
#plot(co2)
#Change axis labels
plot(
  co2, 
  xlab="Years",
  ylab="CO2 Concentration"
)

#Smoothscatter graphic
smoothScatter(
  co2,
  ylab="Years",
  xlab="CO2 Concentration",
)

#Iris dataset
iris

#Dataset search
iris[iris$Sepal.Length==5.1 & iris$Sepal.Width==3.4,]

#Boxplot
boxplot(
  Sepal.Width~Species,
  data = iris,
  col="green",
  xlab="Species of Bird"
)
