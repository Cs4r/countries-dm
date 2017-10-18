# countries-dm
data-mining on countries data


```

# Basic setup
library(readxl)
countries <- read_excel("./countries.xlsx")
View(countries)
library (lattice)
library (moments)
library(Rcmdr)
attach(countries)


# Show column names
names(countries)

# Countries with main group

# gdp vs men and women life expectation
xyplot(MEM_LIFE_EXP + WOMEN_LIFE_EXP ~ GDP_._PER_CAPITA, type="p", pch=16,
auto.key=list(border=TRUE), par.settings=simpleTheme(pch=16),
scales=list(x=list(relation='same'), y=list(relation='same')), data=countries)

# gdp vs men life expectation
scatterplot(MEM_LIFE_EXP~`GDP_$_PER_CAPITA`, reg.line=FALSE, smooth=TRUE,
spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9),
data=countries)

# gdp vs women life expectation
scatterplot(WOMEN_LIFE_EXP~`GDP_$_PER_CAPITA`, reg.line=FALSE, smooth=TRUE,
spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9),
data=countries)

withMainGroup = subset(countries, !is.na(countries$MAIN_GROUP))

rural = subset(withMainGroup, withMainGroup$MAIN_GROUP == "Rural")
urban = subset(withMainGroup, withMainGroup$MAIN_GROUP == "Urban")

# gdp vs kind of country (rural or urban)
dev.copy(png,'./gdp-vs-kind-of-country.png')
par( mfrow =c(1,2))
hist(rural$GDP_._PER_CAPITA, breaks=5, col="red", xlab="GDP", ylab="Frequency", main="Rural Country")
hist(urban$GDP_._PER_CAPITA, breaks=5, col="blue", xlab="GDP",ylab="Frequency", main="Urban Country")
dev.off()


# kind of country vs life expectation (women vs men)

dev.copy(png,'./life-exp-vs-kind-of-country.png')
par( mfrow =c(1,2))
boxplot(withMainGroup$WOMEN_LIFE_EXP~withMainGroup$MAIN_GROUP, col= "red")
title(main="Women", ylab="Life Expectation", xlab = "Kind of country")
boxplot(withMainGroup$MEM_LIFE_EXP~withMainGroup$MAIN_GROUP, col= "blue")
title(main="Men", ylab="Life Expectation", xlab = "Kind of country")
dev.off()


# main sector vs life expectation
dev.copy(png,'./life-exp-vs-main-sector.png')
par( mfrow =c(1,2))
boxplot(withMainGroup$WOMEN_LIFE_EXP~withMainGroup$MAIN_SECTOR, col= "red")
title(main="Women", ylab="Life Expectation", xlab = "Main sector of the country")
boxplot(withMainGroup$MEM_LIFE_EXP~withMainGroup$MAIN_SECTOR, col= "blue")
title(main="Men", ylab="Life Expectation", xlab = "Main sector of the country")
dev.off()

# gdp vs illiteracy
scatterplot(ILLITERACY~GDP_._PER_CAPITA, reg.line=FALSE, smooth=TRUE,
spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9),
data=countries)

# phones rate vs illiteracy
scatterplot(ILLITERACY~PHONES_RATE, reg.line=FALSE, smooth=TRUE,
  spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9),
  data=countries)


europe = subset(countries, CONTINENT == "Europe")
america = subset(countries, CONTINENT == "America")
asia = subset(countries, CONTINENT == "Asia")
africa = subset(countries, CONTINENT == "Africa")


# Illiteracy per continent

par( mfrow =c(2,2))
hist(america$ILLITERACY, breaks=5, col="red", xlab="ILLITERACY RATE", ylab="Frequency", main="America")
hist(asia$ILLITERACY, breaks=5, col="blue", xlab="ILLITERACY RATE", ylab="Frequency", main="Asia")
hist(africa$ILLITERACY, breaks=5, col="green", xlab="ILLITERACY RATE", ylab="Frequency", main="Africa")
hist(europe$ILLITERACY, breaks=5, col="yellow", xlab="ILLITERACY RATE", ylab="Frequency", main="Europe")

# gdp grow rate per continent

par( mfrow =c(2,2))
hist(america$GDP_GROW_RATE, breaks=5, col="red", xlab="GDP GROW RATE", ylab="Frequency", main="America")
hist(asia$GDP_GROW_RATE, breaks=5, col="blue", xlab="GDP GROW RATE", ylab="Frequency", main="Asia")
hist(africa$GDP_GROW_RATE, breaks=5, col="green", xlab="GDP GROW RATE", ylab="Frequency", main="Africa")
hist(europe$GDP_GROW_RATE, breaks=5, col="yellow", xlab="GDP GROW RATE", ylab="Frequency", main="Europe")

# Life expectation per continent

continentDensity <- function(continent, continentName, color = "red"){
  life.expectation = c(continent$WOMEN_LIFE_EXP, continent$MEM_LIFE_EXP)
  life.exp.den <- density (subset(life.expectation, !is.na(life.expectation)))
  plot(life.exp.den ,main= paste("Life expectation density in", continentName))
  polygon (life.exp.den , col=color, border ="black")
}

continentDensity(africa, "Africa", "purple")
continentDensity(america, "America", "blue")
continentDensity(asia, "Asia", "red")
continentDensity(europe, "Europe", "blue")

```
