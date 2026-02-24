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
library(readxl)
library(countrycode)


################################################
# First task will be creating a dataset that displays the country, games, and medals won

#read in the GDP dataset
gdp <- read.csv("gdp_data.csv", header=TRUE, sep=",")

#remove blanks from gdp
gdp <- filter(gdp, Country.Code != "")

#create a function that take a substring of the complex year variable and returns just the year
reformat_yearvar <- function(var){
  new_var <- substr(var, 2, 5)
  return(new_var)
}

#rename year variables to improve readability
gdp <- rename_with(gdp, .fn = reformat_yearvar, .cols= contains("YR"))

#read in the population data
population <- read.csv("population.csv", header=TRUE, sep=",")

#remove blanks from population
population <- filter(population, Country.Code != "")

#rename year variables to improve readability
population <- rename_with(population, .fn = reformat_yearvar, .cols= contains("YR"))

#reshape the gdp dataset to long format
long_gdp <- reshape(gdp, idvar="Country.Code", varying= list(6:70), timevar="year", times= names(gdp)[5:69], direction="long")
long_gdp <- rename(long_gdp, "GDP"="1960")

#reshape the population dataset to long format
long_pop <- reshape(population, idvar="Country.Code", varying= list(5:71), timevar="year", times= names(population)[5:71], direction="long")
long_pop <- rename(long_pop, "Population"="1960")

#merge the datasets - this should drop the years 2025 and 2026
gdp_pop <- merge(long_gdp, long_pop, by=c("Country.Code", "Country.Name", "year"))

#create a new variable with GDP per Capita
gdp_pop <- gdp_pop %>%
  mutate(
    GDP = na_if(GDP, ".."),
    gdp_per_cap = as.numeric(GDP)/as.numeric(Population)
    )

#filter out the region observations from the dataset
gdp_pop <- filter(gdp_pop, Type=="Country")

######################################
# Second Task will be creating a dataset that has the country, year, and medals won

#read in the olympic athlete dataset
athletes <- read.csv("athlete_events.csv", header=TRUE, sep=",")

#create a new dataset that only includes medalists
medalists <- filter(athletes, !is.na(Medal))

#group the medalists by year, team, event, and medal, and count the number of medals earned in event per year
medalists <- medalists %>%
  group_by(NOC, Games, Event, Medal) %>%
  mutate(
    number_of_repeats = n()
  ) %>%
  ungroup()

#add a new variable event_type that records if the event is individual or team
medalists$event_type <- ifelse(medalists$number_of_repeats == 1, "individual", "team")

#create a new dataset that displays the country medals by year
country_medals <- medalists %>%
  group_by(NOC, Games, Event, Medal) %>%
  slice(1) %>% #only keep one row per event
  ungroup()%>%
  group_by(NOC, Games, Year)%>%
  summarise(
    num_gold = sum(Medal == "Gold", na.rm = TRUE),
    num_silver = sum(Medal == "Silver", na.rm = TRUE),
    num_bronze = sum(Medal == "Bronze", na.rm = TRUE),
    total_medals = n(),
    .groups = "drop"
  )


##################################################
#Combine the country_medals dataset with the gdp_pop dataset

#match the gdp country codes to ioc codes
gdp_pop$NOC <- countrycode(gdp_pop$Country.Code, origin = "wb", destination = "ioc", warn=TRUE, nomatch="no match")

#check the observations without a match in wb/ioc
nomatch <- filter(gdp_pop, NOC == "no match")
table(nomatch$Country.Name)

#confirm that nomatches are not in medal dataset
nomatch_medals <- merge(nomatch, country_medals, by.x = c("NOC"))

#gdp_medals contains the country's medals by year and their economic info
gdp_medals <- merge(country_medals, gdp_pop, by.x = c("NOC", "Year"), by.y = c("NOC", "year"))




