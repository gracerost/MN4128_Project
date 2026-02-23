#MN4128 Project

library(ggplot2)
library(psych)
library(pastecs)
library(zoo)
library(dplyr)
library(tidyverse)
library(rio)
library(foreign)
library(haven)

#read in the olympic athlete dataset
athletes <- read.csv("athlete_events.csv", header=TRUE, sep=",")

#table(athletes$)
countries <- read.csv("olympics_medals_country_wise.csv", header=TRUE, sep=",")

gdp <- read.csv("gdp_data.csv", header=TRUE, sep=",")

#remove nas from gdp
gdp <- filter(gdp, Country.Code != "")

head(gdp)

#create a function that take a substring of the complex year variable and returns just the year
reformat_yearvar <- function(var){
  new_var <- substr(var, 2, 5)
  return(new_var)
}

#rename year variables to improve readability
gdp <- rename_with(gdp, .fn = reformat_yearvar, .cols= contains("YR"))


head(gdp)

#read in the population data
population <- read.csv("population.csv", header=TRUE, sep=",")
population <- filter(population, Country.Code != "")

#rename year variables to improve readability
population <- rename_with(population, .fn = reformat_yearvar, .cols= contains("YR"))

head(population)

#reshape the gdp dataset to long format
long_gdp <- reshape(gdp, idvar="Country.Code", varying= list(5:69), timevar="year", times= names(gdp)[5:69], direction="long")
long_gdp <- rename(long_gdp, "GDP"="1960")

long_pop <- reshape(population, idvar="Country.Code", varying= list(5:71), timevar="year", times= names(population)[5:71], direction="long")
long_pop <- rename(long_pop, "Population"="1960")
