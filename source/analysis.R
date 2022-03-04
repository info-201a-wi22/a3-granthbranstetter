#Load some necessary packages, Load the incarceration trends data into a
# variable
library("tidyverse")
library("ggplot2")
library("ggrepel")
library("maps")
library("ggstream")
library("plotly")


incarceration <- read.csv(
"https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Primary variable focused on: proportion of different groups jailed.
# In doing so, analyze the racial disparity in jailing, how the United States
# can move toward fixing the system.



# Summary Statistic #1 - Calculate the total Black, White, and Indigenous
# jailed population for 2018 in the United States
      # Calculate total jailed pops for each demographic
total_jailed_black_pop <- incarceration %>%
    filter(year == 2018) %>%
    select(black_jail_pop) %>%
    drop_na %>%
    summarize(sum(black_jail_pop)) %>%
    pull
total_jailed_white_pop <- incarceration %>%
  filter(year == 2018) %>%
  select(white_jail_pop) %>%
  drop_na %>%
  summarize(sum(white_jail_pop)) %>%
  pull
total_jailed_native_pop <- incarceration %>%
  filter(year == 2018) %>%
  select(native_jail_pop) %>%
  drop_na %>%
  summarize(sum(native_jail_pop)) %>%
  pull
      # Apply calculated pops in format optimal for rmd
get_total_jailed_black_pop <- function(data) {
total_jailed_black_pop <- data %>%
  filter(year == 2018) %>%
  select(black_jail_pop) %>%
  drop_na %>%
  summarize(sum(black_jail_pop)) %>%
  pull
return(prettyNum(total_jailed_black_pop, big.mark = ",", scientific = FALSE))
}

get_total_jailed_white_pop <- function(data2) {
total_jailed_white_pop <- data2 %>%
  filter(year == 2018) %>%
  select(white_jail_pop) %>%
  drop_na %>%
  summarize(sum(white_jail_pop)) %>%
  pull
return(prettyNum(total_jailed_white_pop, big.mark = ",", scientific = FALSE))
}

get_total_jailed_native_pop <- function(data3) {
total_jailed_native_pop <- data3 %>%
  filter(year == 2018) %>%
  select(native_jail_pop) %>%
  drop_na %>%
  summarize(sum(native_jail_pop)) %>%
pull
return(prettyNum(total_jailed_native_pop, big.mark = ",", scientific = FALSE))
}
# Summary Statistic #2 - Calculate the incarceration incident rate per 100,000
# for Black, White, and Indigenous individuals in 2018

# Calculate total applicable black population
total_black_pop_15to64 <- incarceration %>%
  filter(year == 2018) %>%
  select(black_pop_15to64) %>%
  drop_na %>%
  summarize(sum(black_pop_15to64))
# Pull Black jailing incidence per 100,000
get_black_jailing_incidence <- function(data4) {
black_jailing_incidence <- (total_jailed_black_pop /
                              total_black_pop_15to64) * 100000
pull(black_jailing_incidence)
return(prettyNum(black_jailing_incidence, big.mark = ",", scientific = FALSE))
}
# Calculate total applicable white population
total_white_pop_15to64 <- incarceration %>%
  filter(year == 2018) %>%
  select(white_pop_15to64) %>%
  drop_na %>%
  summarize(sum(white_pop_15to64))
# Pull White jailing incidence per 100,000
get_white_jailing_incidence <- function(data5) {
white_jailing_incidence <- (total_jailed_white_pop /
                              total_white_pop_15to64) * 100000
pull(white_jailing_incidence)
return(prettyNum(white_jailing_incidence, big.mark = ",", scientific = FALSE))
}
# Calculate total applicable native population
total_native_pop_15to64 <- incarceration %>%
  filter(year == 2018) %>%
  select(native_pop_15to64) %>%
  drop_na %>%
  summarize(sum(native_pop_15to64))
# Pull Native jailing incidence per 100,000
get_native_jailing_incidence <- function(data6) {
native_jailing_incidence <- (total_jailed_native_pop /
                               total_native_pop_15to64) * 100000
pull(native_jailing_incidence)
return(prettyNum(native_jailing_incidence, big.mark = ",", scientific = FALSE))
}
# Summary Statistic #3 - What has the maximum incarceration rate over the past
# 20 years by race?

get_native_jailing_summary_over_time <- function(data7) {
native_jailing_summary_over_time <- incarceration %>%
  group_by(year) %>%
  filter(year > 1989) %>%
  select(native_jail_pop) %>%
  drop_na %>%
  summarize((sum(native_jail_pop))) %>%
  max()
return(prettyNum(native_jailing_summary_over_time, big.mark = ",",
                 scientific = FALSE))
}

get_black_jailing_summary_over_time <- function(data8) {
  black_jailing_summary_over_time <- incarceration %>%
    group_by(year) %>%
    filter(year > 1989) %>%
    select(black_jail_pop) %>%
    drop_na %>%
    summarize((sum(black_jail_pop))) %>%
    max()
  return(prettyNum(black_jailing_summary_over_time, big.mark = ",",
                   scientific = FALSE))
}

get_white_jailing_summary_over_time <- function(data9) {
  white_jailing_summary_over_time <- incarceration %>%
    group_by(year) %>%
    filter(year > 1989) %>%
    select(white_jail_pop) %>%
    drop_na %>%
    summarize((sum(white_jail_pop))) %>%
    max()
  return(prettyNum(white_jailing_summary_over_time, big.mark = ",",
                   scientific = FALSE))
}

# Summary Statistic #4 - What are  max and min incarcerations in WA
# since 1993 by race? (year limitation eliminated a bunch of NAs)

get_wa_black_jailing_summary <- function(data10) {
wa_black_jailing_summary <- incarceration %>%
  select(year, state, black_jail_pop) %>%
  filter(state == "WA", year > 1989) %>%
  group_by(year) %>%
  summarize((sum(black_jail_pop))) %>%
  drop_na %>%
  max()
return(prettyNum(wa_black_jailing_summary, big.mark = ",", scientific = FALSE))
}

get_wa_native_jailing_summary <- function(data11) {
wa_native_jailing_summary <- incarceration %>%
  select(year, state, native_jail_pop) %>%
  filter(state == "WA", year > 1989) %>%
  group_by(year) %>%
  summarize((sum(native_jail_pop))) %>%
  drop_na %>%
  max()
return(prettyNum(wa_native_jailing_summary, big.mark = ",", scientific = FALSE))
}

get_wa_white_jailing_summary <- function(data12) {
wa_white_jailing_summary <- incarceration %>%
  select(year, state, white_jail_pop) %>%
  filter(state == "WA", year > 1989) %>%
  group_by(year) %>%
  summarize((sum(white_jail_pop))) %>%
  drop_na %>%
  max()
return(prettyNum(wa_white_jailing_summary, big.mark = ",", scientific = FALSE))
}
# Summary Statistic #5 - How many people are incarcerated by race in WA in the
# most recent year available?
get_wa_black_jailing <- function(data13) {
wa_black_jailing <- incarceration %>%
  filter(state == "WA") %>%
  filter(year == max(year)) %>%
  select(black_jail_pop) %>%
  summarize(sum(black_jail_pop))
pull(wa_black_jailing)
return(prettyNum(wa_black_jailing, big.mark = ",", scientific = FALSE))
}

get_wa_white_jailing <- function(data14) {
wa_white_jailing <- incarceration %>%
  filter(state == "WA") %>%
  filter(year == max(year)) %>%
  select(white_jail_pop) %>%
  summarize(sum(white_jail_pop))
pull(wa_white_jailing)
return(prettyNum(wa_white_jailing, big.mark = ",", scientific = FALSE))
}

get_wa_native_jailing <- function(data13) {
wa_native_jailing <- incarceration %>%
  filter(state == "WA") %>%
  filter(year == max(year)) %>%
  select(native_jail_pop) %>%
  summarize(sum(native_jail_pop))
pull(wa_native_jailing)
return(prettyNum(wa_native_jailing, big.mark = ",", scientific = FALSE))
}

# Chart #1: Trends over time chart - Calculated trend of the proportion of
# racial groups being targeting in jailing, by year.
# Step 1: Created data set of Black, White, and Native percent incarcerated by
# year
by_year_pop <- incarceration %>%
  group_by(year) %>%
  select(black_pop_15to64,
         black_jail_pop,
         white_pop_15to64,
         white_jail_pop,
         native_pop_15to64,
         native_jail_pop) %>%
  drop_na %>%
  summarize(sum(black_pop_15to64),
            sum(black_jail_pop),
            sum(white_pop_15to64),
            sum(white_jail_pop),
            sum(native_pop_15to64),
            sum(native_jail_pop))
by_year_pop <- by_year_pop %>%
  filter(year > 1989) %>%
  group_by(year) %>%
  mutate(Black = `sum(black_jail_pop)` /
           `sum(black_pop_15to64)` * 100000) %>%
  mutate(White = `sum(white_jail_pop)` /
           `sum(white_pop_15to64)` * 100000) %>%
  mutate("Native American" = `sum(native_jail_pop)` /
           `sum(native_pop_15to64)` * 100000)
by_year_prop <- by_year_pop %>%
  select(year, Black, White, "Native American")

by_year_prop <- pivot_longer(by_year_prop,
                             cols =
                               c("Black", "White", "Native American"),
                             names_to = "Race",
                             values_to = "proportion")
#Step 2: Visualized the dataframe
cols <- c("#C20008", "#FF020D", "#FFC740")

comparison <- ggplot(by_year_prop, aes(x = year, y = proportion, fill = Race)) +
  geom_stream(type = "ridge") +
  scale_fill_manual(
    values = cols, labels = c("Black", "Native American", "White")) +
  ggtitle("Inequity in Jailing and Policing since 1990") +
  xlab("Number Jailed Per 100k") + ylab("Year")
comparison

# Chart 2: Variable Comparison Chart - Compares jailing population since 1993
# against county in WA.
# Step 1: Pulled all county data for WA since 1993 from incarceration, for
# year, country, and specified prison population.
wa_county_data_black_jailings <- incarceration %>%
  filter(state == "WA") %>%
  select(year, state, county_name, black_jail_pop, total_pop) %>%
  filter(total_pop > 400000) %>%
  group_by(year, county_name)
wa_county_data_white_jailings <- incarceration %>%
  filter(state == "WA") %>%
  select(year, state, county_name, white_jail_pop, total_pop) %>%
  filter(total_pop > 400000) %>%
  group_by(year, county_name)
wa_county_data_native_jailings <- incarceration %>%
  filter(state == "WA") %>%
  select(year, state, county_name, native_jail_pop, total_pop) %>%
  filter(total_pop > 400000) %>%
  group_by(year, county_name)
# Step 2: Visualize as a scatterplot, where dot's color is based on year
# (group), x = county, and y = black_jail_pop(sum)
black_plot <- ggplot(wa_county_data_black_jailings, aes(x = total_pop,
       y = black_jail_pop,
       color = county_name,
       alpha = year,
       size = total_pop)) +
  geom_point() +
  ggtitle("5 Largest WA Counties and Black Jailing") +
  labs(x = "Total Population",
       y = "Black Jail Population",
       color = "County",
       alpha = "Year",
       size = "Total Population")
black_plot
white_plot <- ggplot(wa_county_data_white_jailings, aes(x = total_pop,
       y = white_jail_pop,
       color = county_name,
       alpha = year,
       size = total_pop)) +
  geom_point() +
  ggtitle("5 Largest WA Counties and White Jailing") +
  labs(x = "Total Population",
       y = "White Jail Population",
       color = "County",
       alpha = "Year",
       size = "Total Population")
white_plot
native_plot <- ggplot(wa_county_data_native_jailings, aes(x = total_pop,
       y = native_jail_pop,
       color = county_name,
       alpha = year,
       size = total_pop)) +
  geom_point() +
  ggtitle("5 Largest WA Counties and Native Jailing") +
  labs(x = "Total Population",
       y = "Native Jail Population",
       color = "County",
       alpha = "Year",
       size = "Total Population")
native_plot

# Chart #3: Maps White, Native American, and Black Prison Population Density
# throughout the United States most recent year.
# Get County Jailing Data Nationwide.

#First get us map set up
library(maps)
library(mapproj)
library(patchwork)
#filter incarceration
mod_incarceration <- incarceration %>%
  filter(year == max(year))
#most recent counties from data set
county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")
#merge map data and incarceration
map_data <- county_shapes %>%
  left_join(mod_incarceration, by = "fips") %>%
  select(long, lat, group, order, polyname,
         fips, yfips, year, state, county_name,
         native_jail_pop)
#minimalist
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )
#create map
native_jailing_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = native_jail_pop),
    color = "gray", size = 0.3
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$native_jail_pop)),
                        na.value = "white", low = "yellow", high = "red") +
  blank_theme +
  ggtitle("Jailing of Native Americans in the US") +
  labs(fill = "Jailed Native Pop")
native_jailing_map