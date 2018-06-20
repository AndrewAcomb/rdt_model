### Andrew Acomb, Philip Brenninkmeyer PredictIt Model

## DATA TIDYING

# Loading Packages
library(tidyverse)
library(lubridate)

# Loading Data
rdt <- read_csv("Data/raw_data/rdt.csv")
speeches <- read_delim("Data/raw_data/trump_speeches.csv", delim = "~")
reddit <- read_delim("Data/raw_data/reddit_trump.csv", delim = "|")
sp500 <- read_csv("Data/raw_data/GSPC.csv")

# Parsing Date Vectors

a <- strsplit(rdt$created_at, " ") %>% unlist() %>% na.omit()
b <- matrix(a, nrow = 2)

rdt_dates <- b[1,] %>% parse_date("%m-%d-%Y")
rdt_times <- b[2,] %>% parse_time("%H:%M:%S")
rdt_days <- rdt_dates %>% day()
rdt_months <- rdt_dates %>% month()
rdt_years <- rdt_dates %>% year()
rdt_hours <- rdt_times %>% hour()
rdt_minutes <- rdt_times %>% minute()
rdt_seconds <- rdt_times %>% second()

rdt <- rdt %>% select(-c(id_str, created_at))
rdt <- rdt[1:16838,]

rdt <- rdt %>% mutate(date = rdt_dates, time = rdt_times)



# Trump watches Fox and Friends from 6-9 AM ET, often tweeting during the show. 

