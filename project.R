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
library(stargazer)
library(plotly)
library(htmlwidgets)

# prevent the output to be displayed in scientific notation
options(scipen=999)

# Prevent R from truncating output 
options(dplyr.width = Inf)

#  clear out R environment ?
rm(list=ls())

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

gdp_medals$GDP <- as.numeric(gdp_medals$GDP)/1000000000 #GDP is now in millions of USD

medal_2008 <- filter(gdp_medals, Year == 2008)

gdp_medal_mod <- lm(num_gold ~ GDP, data = medal_2008)
stargazer(gdp_medal_mod, type="text")

p1 <- ggplot(medal_2008, aes(x = GDP, y = num_gold, text = NOC))+
  geom_point()+
  labs(title = "GDP vs Gold Medals for 2018",
       x = "GDP (millions 2015 USD",
       y = "Number of Gold Medals")
  # stat_summary_bin(fun="mean", bins=100, size=2, geom="point")+
  # stat_smooth(method="lm", se=FALSE)+
  theme_minimal()

#make an interactive plot for HTML
p1_interactive <- ggplotly(p1, tooltip = "text")

saveWidget(p1_interactive, "gdp_gold_interactive.html")

#filter out countries over 5000 million GDP
medal_smallgdp_2008 <-  filter(gdp_medals, GDP < 5000)

gdp_medal_mod2 <- lm(num_gold ~ GDP, data = medal_smallgdp_2008)
stargazer(gdp_medal_mod, type="text")

p2 <- ggplot(medal_smallgdp_2008, aes(x = GDP, y = num_gold))+
  stat_summary_bin(fun="mean", bins=100, size=2, geom="point")+
  stat_smooth(method="lm", se=FALSE)+
  theme_bw()

p2

##############################################
# Making a group of binned charts for every 8 years
# charts will take the average medal count and average gdp of each country over the period

#filter the dataset to only include 1960 to 1968
#gdp_medal_1960_to_1968 <- filter(gdp_medals, Year < 1972)

#group by NOC and get an average of gdp and medals
gdp_medal_1960_to_1968 <- filter(gdp_medals, Year < 1972) %>%
  group_by(NOC)%>%
  summarise(
    avg_GDP = mean(GDP),
    avg_medals = mean(total_medals)
  )%>%
  ungroup()

p1960 <- ggplot(gdp_medal_1960_to_1968, aes(x = avg_GDP, y = avg_medals, text = NOC))+
  geom_point()+
  labs(title = "GDP vs Medals for 1960-1968",
       x = "Average GDP (millions 2015 USD",
       y = "Average Number of Medals")
# stat_summary_bin(fun="mean", bins=100, size=2, geom="point")+
# stat_smooth(method="lm", se=FALSE)+
theme_minimal()

p1960

p1960_interactive <- ggplotly(p1960, tooltip = "text")

p1960_interactive


############################################################
# Make a stacked bar chart that displays the percentage of medals won by the top 5 GDP countries

#need to get a count of total medals for each year
tot_medals_by_year <- gdp_medals %>%
  group_by(Year)%>%
  summarise(
    tot_year_medals = sum(total_medals)
  )%>%
ungroup()

top5_gdp_year <- gdp_medals %>%
  group_by(Year)%>%
  arrange(desc(GDP), .by_group = TRUE) %>%
  slice_head(n=5) %>%
  ungroup()

top5_percent <- top5_gdp_year%>%
 left_join(tot_medals_by_year, by = "Year") %>%
  mutate(percent_medals = total_medals / tot_year_medals * 100)

p_top5 <- ggplot(top5_percent, aes(x = Year, y = percent_medals, fill = NOC)) +
  geom_bar(stat = "identity") +
  labs(title = "Percent of Medals Won by Top 5 GDP Countries Over Time",
       x = "Year",
       y = "Percent of Total Medals",
       fill = "Country") +
  scale_x_continuous(breaks = sort(unique(top5_percent$Year)))+
  scale_y_continuous(breaks = seq(0, 100, by=5))+
  annotate("text", x = 1980, y=17, label="Olympics\nBoycott", color="black", size = 4)+
  # scale_fill_brewer(palette = "Set2")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_top5

############################################################
# Creating a line plot that displays the top and bottom group of medal winners by GDP

#get the bottom group of medalists (tot_medals = 1)
# bottom_medals_year <- gdp_medals %>%
#   group_by(Year, NOC)%>%
#   summarise()
#   ungroup()
