# Load in necessary functions for analysis 
library(readr)
library(tidyverse)
library(gganimate)

# Read in data for the analysis 
gapminder_1997 <- read_csv("gapminder_1997.csv")
View(gapminder_1997)

# Explore different R commands 
?read_csv
sum(5, 6)
Sys.Date()

###########################################################
##################### R for Plotting ######################
###########################################################

# Creating a plot 
ggplot(data = gapminder_1997) + 
  aes(x = gdpPercap, y = lifeExp, color = continent, 
      size = pop/1000000) + 
  labs(x = "GDP Per Capita", y = "Life Expectancy (yrs)", 
       title = "Do people in wealthy countries life longer?",
       size = "Population (in millions)") + 
  geom_point() +
  theme_minimal() + 
  scale_color_brewer(palette = "Set1")


# Read in full gapminder dataset
gapminder_data <- read_csv(file = "gapminder_data.csv")
View(gapminder_data)

# Plot time (x-axis) and life expect (y-axis) points
ggplot(data = gapminder_data) + 
  # Add the aes objects time & lifeExpt
  aes(x = year, y = lifeExp, color = continent, 
      group = country) + 
  # Add the human readable labels 
  labs(x = "Year", y = "Life Expectancy (yrs)") + 
  geom_line() + 
  theme_minimal() + 
  scale_color_brewer(palette = "Set1")


# Geom boxplot/violin
ggplot(gapminder_1997) + 
  aes(x = continent, y = lifeExp, color = continent) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.5, size = 5)


# Animated plot
staticHansPlot <- ggplot(data = gapminder_data)+
  aes(x = log(gdpPercap), y = lifeExp, size = pop/1000000, color = continent)+
  geom_point(alpha = 0.5) + # we made our points slightly transparent, because it makes it easier to see overlapping points
  scale_color_brewer(palette = "Set1") +
  labs(x = "GDP Per Capita", y = "Life Expectancy", color= "Continent", size="Population (in millions)")+
  theme_classic()

staticHansPlot

# Let's save a ggplot!
ggsave(file = "staticHans.png", staticHansPlot, height = 10, width = 12, units = "in")

animatedHansPlot <- staticHansPlot +
  transition_states(year,  transition_length = 1, state_length = 1)+
  ggtitle("{closest_state}")

animatedHansPlot


###########################################################
################# R for Data Analysis #####################
###########################################################



# Summarize
summarize(gapminder_data, averageLifeExp=mean(lifeExp))
gapminder_data %>% summarize(averageLifeExp=mean(lifeExp))
gapminder_data %>%
  summarize(averageLifeExp=mean(lifeExp))



# Filter 
gapminder_data %>%
  filter(year == 2007) %>%
  summarize(average=mean(lifeExp))


# Group by 
gapminder_data %>%
  group_by(year) %>%
  summarize(average=mean(lifeExp))


# Mutate
gapminder_data %>%
  mutate(gdp = pop * gdpPercap)


# Subset
gapminder_data %>%
  select(pop, year)



############## Changing the shape of data

# Make data wide format
gapminder_data %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp )

gapminder_data_2007 <- read_csv("gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent)


read_csv("co2-un-data.csv", skip=1)

# Read in CO2 emissions data 
co2_emissions_dirty <- read_csv("co2-un-data.csv", skip=2,
                                col_names=c("region", "country", "year", "series", "value", "footnotes", "source"))
head(co2_emissions_dirty)


# Clean up data with mutate
co2_emissions_dirty %>% 
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"))



co2_emissions_dirty %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emission",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emission")) %>%
  pivot_wider(names_from=series, values_from=value)


co2_emissions_dirty %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita")) %>%
  pivot_wider(names_from=series, values_from=value) %>%
  filter(year==2005) %>%
  select(-year)


# Clean it up !
co2_emissions <- co2_emissions_dirty %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emission",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emission")) %>%
  pivot_wider(names_from=series, values_from=value) %>%
  filter(year==2005) %>%
  select(-year)

######## JOIN 
# Inner join 
inner_join(gapminder_data, co2_emissions)
gapminder_co2 <- inner_join(gapminder_data, co2_emissions, by="country")

# Anti join 
anti_join(gapminder_data, co2_emissions, by="country")

co2_emissions <- read_csv("co2-un-data.csv", skip=2,
                          col_names=c("region", "country", "year",
                                      "series", "value", "footnotes", "source")) %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita")) %>%
  pivot_wider(names_from=series, values_from=value) %>%
  filter(year==2005) %>%
  select(-year) %>%
  mutate(country=recode(country,
                        "Bolivia (Plurin. State of)" = "Bolivia",
                        "United States of America" = "United States",
                        "Venezuela (Boliv. Rep. of)" = "Venezuela")
  ) 



gapminder_data <- read_csv("gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent) %>%
  mutate(country = recode(country, "Puerto Rico" = "United States")) %>%
  group_by(country) %>%
  summarize(lifeExp = sum(lifeExp * pop)/sum(pop),
            gdpPercap = sum(gdpPercap * pop)/sum(pop),
            pop = sum(pop))



ggplot(gapminder_co2, aes(x=gdpPercap, y=per_capita)) +
  geom_point() +
  labs(x="GDP (per capita)",
       y="CO2 emitted (per capita)",
       title="There is a strong association between a nation's GDP \nand the amount of CO2 it produces"
  ) +
  geom_smooth(method="lm")
