### Andrew Acomb, Philip Brenninkmeyer PredictIt Model

## DATA TIDYING

## Loading Packages
library(tidyverse)
library(lubridate)

## Loading Data
rdt <- read_csv("Data/raw_data/rdt.csv")
speeches <- read_delim("Data/raw_data/trump_speeches.csv", delim = "~")
reddit <- read_delim("Data/raw_data/reddit_trump.csv", delim = "|")
sp500 <- read_csv("Data/raw_data/GSPC.csv")
dow <- read_csv("Data/raw_data/DJIA.csv")

## Parsing Date Vectors

# Parsing rdt dates and times

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
rdt <- rdt %>% arrange(date,time)
# Parsing speeches dates

speeches$upload_date <- speeches$upload_date %>% parse_date("%Y%m%d")
speech_day <- speeches$upload_date %>% day() %>% as.character()
speech_month <- speeches$upload_date %>% month() %>% as.character()
speech_year <- speeches$upload_date %>% year() %>% as.character()
c <- paste(speech_year,speech_month, speech_day, sep = "-")  %>% parse_date("%Y-%m-%d")

speeches <- speeches %>% select(-c(id, upload_date))
speeches <- speeches %>% mutate(date = c)
speeches <- speeches %>% arrange(date)

# Parsing dow dates

colnames(dow) <- c("date", "dow_price")
dow <- dow %>% filter( dow_price != ".")

# Parsing sp500 dates

sp500 <- sp500 %>% select(c(Date,Open))
colnames(sp500) <- c("date", "sp500_price")

## Joining Datasets

tweets <- rdt %>% mutate(speech = if_else(date %in% speeches$date, 1, 0))
tweets <- left_join(tweets, dow)
tweets <- left_join(tweets, sp500)

## Dealing with missing values
