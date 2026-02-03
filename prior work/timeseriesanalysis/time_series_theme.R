## time_series_theme.R
## Joseph E. Hibdon, Jr.
## Created - 7/24/2023
## The code will graph data based on the themes that were coded in our data.

# - Library's to load
library(academictwitteR)
library(tidytags)
library(tidyverse)
library(lubridate)
library(stringr)
library(visNetwork)
library(igraph)

#Load the all-data
#### Note you will need to load the all-data to run the code.
alldata_tidy <- read_csv("all-data.csv")
View(alldata_tidy)

##Trial to see if it works
############## - By Day of 2020 During JMM #####################
timeSeriesDay2020 <- alldata_tidy %>% 
  filter(str_detect(created_at, "2020-01"))

## Convert dates in new data frame from characters to format year-month-date
timeSeriesDay2020$created_at <- as.POSIXct(timeSeriesDay2020$created_at)

## Collect data for self-organization
self_orgJMM2020 <- timeSeriesDay2020[timeSeriesDay2020$`Self-organization` == '1', ]
## Create a month variable then group by days to summarize tweet numbers
self_orgJMM2020 <- self_orgJMM2020 %>% 
  select(created_at) %>%
  mutate(day = floor_date(created_at, "day")) %>%
  group_by(day) %>% 
  summarize(tweets = n())

## Collect data Building Community
building_comJMM2020 <- timeSeriesDay2020[timeSeriesDay2020$'Building community' == '1', ]
## Create a month variable then group by days to summarize tweet numbers
building_comJMM2020 <- building_comJMM2020 %>% 
  select(created_at) %>%
  mutate(day = floor_date(created_at, "day")) %>%
  group_by(day) %>% 
  summarize(tweets = n())

## Collect data Broadening the counterpublic
broad_counterJMM2020 <- timeSeriesDay2020[timeSeriesDay2020$'Broadening the counterpublic' == '1', ]
## Create a month variable then group by days to summarize tweet numbers
broad_counterJMM2020 <- broad_counterJMM2020 %>% 
  select(created_at) %>%
  mutate(day = floor_date(created_at, "day")) %>%
  group_by(day) %>% 
  summarize(tweets = n())

## Collect data Creating Change in Math
ccmJMM2020 <- timeSeriesDay2020[timeSeriesDay2020$'Creating change in math' == '1', ]
## Create a month variable then group by days to summarize tweet numbers
ccmJMM2020 <- ccmJMM2020 %>% 
  select(created_at) %>%
  mutate(day = floor_date(created_at, "day")) %>%
  group_by(day) %>% 
  summarize(tweets = n())

## Collect data SJEDI
sjediJMM2020 <- timeSeriesDay2020[timeSeriesDay2020$SJEDI == '1', ]
## Create a month variable then group by days to summarize tweet numbers
sjediJMM2020 <- sjediJMM2020 %>% 
  select(created_at) %>%
  mutate(day = floor_date(created_at, "day")) %>%
  group_by(day) %>% 
  summarize(tweets = n())

### Plot -------------
## Plot #disruptJMM Tweets Over Time During JMM Conference (2020)
plot_self_orgJMM2020 <- ggplot(self_orgJMM2020, aes(x = day, y = tweets)) + 
  geom_point() + 
  geom_line() +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 day",
                   date_labels = "%a, \n%d") +
  theme(axis.text.x = element_text(angle = 0,
                                   vjust = 1,
                                   hjust = 0.5),
        plot.title = element_text(face = "bold")) +
  labs (x = "Date", y = "Tweet Count",
        title = "#disruptJMM Tweets During JMM (2020) Coded Self Organization",
        subtitle = "JMM 2020: Jan. 15th - 18th",
        caption = "\nSource: Twitter's API via academictwitteR")
## View plot
plot_self_orgJMM2020


plot_building_comJMM2020 <- ggplot(building_comJMM2020, aes(x = day, y = tweets)) + 
  geom_point() + 
  geom_line() +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 day",
                   date_labels = "%a, \n%d") +
  theme(axis.text.x = element_text(angle = 0,
                                   vjust = 1,
                                   hjust = 0.5),
        plot.title = element_text(face = "bold")) +
  labs (x = "Date", y = "Tweet Count",
        title = "#disruptJMM Tweets During JMM (2020) Building Community",
        subtitle = "JMM 2020: Jan. 15th - 18th",
        caption = "\nSource: Twitter's API via academictwitteR")
## View plot
plot_building_comJMM2020

plot_broad_counterJMM2020 <- ggplot(broad_counterJMM2020, aes(x = day, y = tweets)) + 
  geom_point() + 
  geom_line() +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 day",
                   date_labels = "%a, \n%d") +
  theme(axis.text.x = element_text(angle = 0,
                                   vjust = 1,
                                   hjust = 0.5),
        plot.title = element_text(face = "bold")) +
  labs (x = "Date", y = "Tweet Count",
        title = "#disruptJMM Tweets During JMM (2020) Broadening the Counterpublic",
        subtitle = "JMM 2020: Jan. 15th - 18th",
        caption = "\nSource: Twitter's API via academictwitteR")
## View plot
plot_broad_counterJMM2020

plot_ccmJMM2020 <- ggplot(ccmJMM2020, aes(x = day, y = tweets)) + 
  geom_point() + 
  geom_line() +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 day",
                   date_labels = "%a, \n%d") +
  theme(axis.text.x = element_text(angle = 0,
                                   vjust = 1,
                                   hjust = 0.5),
        plot.title = element_text(face = "bold")) +
  labs (x = "Date", y = "Tweet Count",
        title = "#disruptJMM Tweets During JMM (2020) Creating Change in Mathematics",
        subtitle = "JMM 2020: Jan. 15th - 18th",
        caption = "\nSource: Twitter's API via academictwitteR")
## View plot
plot_ccmJMM2020

plot_sjediJMM2020 <- ggplot(sjediJMM2020, aes(x = day, y = tweets)) + 
  geom_point() + 
  geom_line() +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 day",
                   date_labels = "%a, \n%d") +
  theme(axis.text.x = element_text(angle = 0,
                                   vjust = 1,
                                   hjust = 0.5),
        plot.title = element_text(face = "bold")) +
  labs (x = "Date", y = "Tweet Count",
        title = "#disruptJMM Tweets During JMM (2020) SJEDI",
        subtitle = "JMM 2020: Jan. 15th - 18th",
        caption = "\nSource: Twitter's API via academictwitteR")
## View plot
plot_sjediJMM2020

#######################
## Create a folder for data visualizations
dir.create("jmm2020_coded_visualizations")

## Save plot as a png file
ggsave(filename = "plot_self_orgJMM2020.png",
       plot = plot_self_orgJMM2020,
       path = "jmm2020_coded_visualizations/")

## Save plot as a png file
ggsave(filename = "plot_building_comJMM2020.png",
       plot = plot_building_comJMM2020,
       path = "jmm2020_coded_visualizations/")

## Save plot as a png file
ggsave(filename = "plot_broad_counterJMM2020.png",
       plot = plot_broad_counterJMM2020,
       path = "jmm2020_coded_visualizations/")

## Save plot as a png file
ggsave(filename = "plot_ccmJMM2020.png",
       plot = plot_ccmJMM2020,
       path = "jmm2020_coded_visualizations/")

## Save plot as a png file
ggsave(filename = "plot_sjediJMM2020.png",
       plot = plot_sjediJMM2020,
       path = "jmm2020_coded_visualizations/")




