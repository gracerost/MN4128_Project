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
library(flextable)
library(huxtable)
library(officer)

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

gdp_pop$NOC[gdp_pop$Country.Code == "URS"] <- "URS"

#gdp_medals contains the country's medals by year and their economic info
gdp_medals <- merge(country_medals, gdp_pop, by.x = c("NOC", "Year"), by.y = c("NOC", "year"))

#convert GDP unit to millions of USD
gdp_medals$GDP <- as.numeric(gdp_medals$GDP)/1000000000 #GDP is now in millions of USD

#make a new variable that is the log(medals)
gdp_medals$ltotal_medals <- log(gdp_medals$total_medals)
gdp_medals$lgold <- ifelse(gdp_medals$num_gold == 0, 0, log(gdp_medals$num_gold))
gdp_medals$lsilver <- ifelse(gdp_medals$num_silver == 0, 0, log(gdp_medals$num_silver))
gdp_medals$lbronze <- ifelse(gdp_medals$num_bronze == 0, 0, log(gdp_medals$num_bronze))

# regress GDP on log(medals)
gdp_medal_mod <- lm(ltotal_medals ~ GDP, data = gdp_medals)
gdp_medal_mod1 <- lm(lgold ~ GDP, data = gdp_medals)
gdp_medal_mod2 <- lm(lsilver ~ GDP, data = gdp_medals)
gdp_medal_mod3 <- lm(lbronze ~ GDP, data = gdp_medals)

stargazer(gdp_medal_mod,gdp_medal_mod1, gdp_medal_mod2, gdp_medal_mod3, type="text", digits=5)

#output models into a publication-style table
huxreg <- huxreg("log(Total Medals)"= gdp_medal_mod, "log(Gold Medals)"=gdp_medal_mod1,
                 "log(Silver Medals)"= gdp_medal_mod2, "log(Bronze Medals)" = gdp_medal_mod3,
                 number_format = "%.5f",
                 statistics = c(N="nobs", R2="r.squared"),
                 omit_coefs = "(Intercept)")

reg_table <- huxtable::as_flextable(huxreg)

reg_table <- add_header_lines(reg_table, values="Table 1. Effect of GDP on Medal Count (1960-2016)")

reg_table

# Show that population does not impact medal count
gdp_medals$Population <- as.numeric(gdp_medals$Population)

mod <- lm(ltotal_medals ~ Population, data = gdp_medals)
mod1 <- lm(lgold ~ Population, data = gdp_medals)
mod2 <- lm(lsilver ~ Population, data = gdp_medals)
mod3 <- lm(lbronze ~ Population, data = gdp_medals)

stargazer(mod, type="text", digits=5)

#output models in a publication-style table
huxreg2 <- huxreg("log(Total Medals)"= mod, "log(Gold Medals)"=mod1,
                 "log(Silver Medals)"= mod2, "log(Bronze Medals)" = mod3,
                 number_format = "%.5f",
                 statistics = c(N="nobs", R2="r.squared"),
                 omit_coefs = "(Intercept)")

reg_table2 <- huxtable::as_flextable(huxreg2)

reg_table2 <- add_header_lines(reg_table2, values="Table 2. Effect of Population on Medal Count (1960-2016)")

reg_table2

# save_as_docx(
#   reg_table, reg_table2,
#   path = "reg_tables.docx")

#create the equation for the log(totalmedals) ~ GDP relationship
eq <- paste0(
  "y = ", round(coef(gdp_medal_mod)[1], 3), 
  " + ", round(coef(gdp_medal_mod)[2], 5), "x"
)

#make a binned scatter plot with binned GDP in the x axis and log of total medals in the y axis
p1 <- ggplot(gdp_medals, aes(x = GDP, y = ltotal_medals))+
  stat_summary_bin(fun="mean", bins=200, size=2, geom="point")+ #break up into 200 bins
  stat_smooth(method="lm", se=FALSE)+ #draw a linear regression line
  labs(title = "Binned GDP vs Medals (1960-2016)",
        x = "Average GDP (millions 2015 USD)",
        y = "Log of Country's Total Medals Won")+
  annotate("text", x = 10000, y = 5.5, label = eq, size = 4, hjust = 0)+ #add the equation to the plot
  scale_x_continuous( #scale the x axis
    limits = c(0, 20000),
    breaks = seq(0, 20000, by = 2500))+
  scale_y_continuous( #scale the y axis
    limits = c(0, 6),
    breaks = seq(0, 6, by=1)
  )+
  theme_minimal()+
  theme( #adjust the ggplot theme for readability/style
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5)
  )

p1

##############################################
# Examining how the effect of GDP changes over time and accross Summer and Winter Olympic Games
# This relationship will be displayed in a line plot

#calculate and record the regression coefficient and p-value for each games, each year competed in a new dataset
year_relationship <- gdp_medals %>%
  group_by(Year, Games)%>%
  summarise(
  beta = coef(lm(ltotal_medals ~ GDP))[2], #record the coefficient
  p_value = coef(summary(lm(ltotal_medals ~ GDP)))[4] #record the p-value
  )%>%
  ungroup()


#coef(summary(lm(ltotal_medals ~ GDP, data=gdp_medals)))[4]

#turn the coefficient into a percent
year_relationship$beta <- year_relationship$beta * 100

#add a variable that denotes what season the games was
year_relationship$season <- ifelse(grepl(year_relationship$Games, pattern="Summer"), "Summer", "Winter")

#make a line plot that displays the effect of GDP over time
#there will be two lines - one for summer games and one for winter games
p2 <- ggplot(year_relationship, aes(x = Year, y = beta, color=season))+
  geom_line(stat="identity")+
  labs(title = "Figure 1: GDP Effect on Medals Won (Summer vs. Winter)",
       x = "Olympics Year",
       y = "Effect of GDP on Total Medals Won",
       color = "Olympics Type")+
  scale_x_continuous( #scale the x axis
    limits= c(1960, 2016),
    breaks = sort(unique(year_relationship$Year)))+
  theme_bw() +
theme( #adjust the ggplot theme for readability/style
  panel.background = element_blank(),
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black"),
  plot.title = element_text(hjust = 0.5),
  axis.text.x = element_text(angle = 45, hjust = 1)
)

p2

############################################################
# Make a stacked bar chart that displays the percentage of medals won by the top 5 GDP countries

#add a variable that denotes what season the games was
gdp_medals$season <- ifelse(grepl(gdp_medals$Games, pattern="Summer"), "Summer", "Winter")


########## Summer Olympics Chart #############
#Filter for only summer onlympics
summer <- filter(gdp_medals, gdp_medals$season == "Summer")

#get the total medals for each summer olympic games
tot_medals_by_year_summer <- summer %>%
  group_by(Year)%>%
  summarise(
    tot_year_medals = sum(total_medals)
  )%>%
ungroup()

#get the top 5 GDP countries for each year of summer olympics
top5_gdp_year_summer <- summer %>%
  group_by(Year)%>%
  arrange(desc(GDP), .by_group = TRUE) %>%
  slice_head(n=5) %>%
  ungroup()

# make a dataframe that has the top 5 GDP countries each year of summer olympics
# and the percent of total medals they won
top5_percent_summer <- top5_gdp_year_summer%>%
 left_join(tot_medals_by_year_summer, by = "Year") %>%
  mutate(percent_medals = total_medals / tot_year_medals * 100)

#make a stacked bar chart of the top 5 GDP countries an their percent of medals won each year
p_top5_summer <- ggplot(top5_percent_summer, aes(x = Year, y = percent_medals, fill = NOC)) +
  geom_bar(stat = "identity") +
  labs(title = "Percent of Summer Olympic Medals Won by the Top 5 GDP Countries of that Year",
       x = "Year",
       y = "Percent of Total Medals",
       fill = "Countries") +
  scale_x_continuous(breaks = sort(unique(top5_percent_summer$Year)))+
  scale_y_continuous(breaks = seq(0, 100, by=5))+
  annotate("text", x = 1980, y=17, label="Olympics Boycott", color="black", angle=90, hjust=0.5, size = 4)+
  scale_fill_brewer(palette = "Paired")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )

p_top5_summer

########### Winter Olympics Chart ##############

winter <- filter(gdp_medals, gdp_medals$season == "Winter")

#get the total medals for each winter olympic games
tot_medals_by_year_winter <- winter %>%
  group_by(Year)%>%
  summarise(
    tot_year_medals = sum(total_medals)
  )%>%
  ungroup()

#get the top 5 GDP countries for each year of winter olympics
top5_gdp_year_winter <- winter %>%
  group_by(Year)%>%
  arrange(desc(GDP), .by_group = TRUE) %>%
  slice_head(n=5) %>%
  ungroup()

# make a dataframe that has the top 5 GDP countries each year of winter olympics
# and the percent of total medals they won
top5_percent_winter <- top5_gdp_year_winter%>%
  left_join(tot_medals_by_year_winter, by = "Year") %>%
  mutate(percent_medals = total_medals / tot_year_medals * 100)

#make a stacked bar chart of the top 5 GDP countries an their percent of medals won each year
p_top5_winter <- ggplot(top5_percent_winter, aes(x = Year, y = percent_medals, fill = NOC)) +
  geom_bar(stat = "identity") +
  labs(title = "Percent of Winter Olympic Medals Won by the Top 5 GDP Countries of that Year",
       x = "Year",
       y = "Percent of Total Medals",
       fill = "Countries") +
  scale_x_continuous(breaks = sort(unique(top5_percent_winter$Year)))+
  scale_y_continuous(breaks = seq(0, 100, by=5))+
  scale_fill_brewer(palette = "Paired")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )

p_top5_winter






