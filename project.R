#MN4128 Project

#read in the olympic athlete dataset
athletes <- read.csv("athlete_events.csv", header=TRUE, sep=",")

#table(athletes$)
countries <- read.csv("olympics_medals_country_wise.csv", header=TRUE, sep=",")

gdp <- read.csv("gdp_data.csv", header=TRUE, sep=",")

head(gdp)

population <- read.csv("population.csv", header=TRUE, sep=",")

head(population)
