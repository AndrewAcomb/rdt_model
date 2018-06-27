### Andrew Acomb, Philip Brenninkmeyer PredictIt Model

## DATA TIDYING

## Loading Packages
library(tidyverse)
library(lubridate)
library(plyr)

## Loading Data
rdt <- read_csv("Data/raw_data/rdt.csv")
speeches <- read_delim("Data/raw_data/trump_speeches.csv", delim = "~")
reddit <- read_delim("Data/raw_data/reddit_trump.csv", delim = "|")
sp500 <- read_csv("Data/raw_data/GSPC.csv")
dow <- read_csv("Data/raw_data/DJIA.csv")
schedules <- read_csv("Data/raw_data/RDT_Schedules_27-06-18.csv")

## Parsing Date Vectors

# Parsing rdt dates and times

rdt <- rdt %>% mutate(datetime = mdy_hms(created_at) - hours(5))
rdt <- rdt %>% mutate(date = date(datetime), time = substring(datetime, 11)) %>% head(nrow(rdt) -1)


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

# Parsing Schedule Dates

schedules$date <- as.Date(schedules$date, format = "%m/%d/%y")
schedules <- schedules %>% select(c(date, details))
colnames(schedules) <- c("date","sched_text")
schedules <- aggregate(data= schedules, sched_text~date, FUN = function(t) paste(t))

## Joining Datasets

tweets <- rdt %>% mutate(speech = if_else(date %in% speeches$date, 1, 0))
tweets <- left_join(tweets, dow)
tweets <- left_join(tweets, sp500)

## Creating time series data

by_date <- seq(as.Date("2014-01-01"), as.Date("2018-06-17"), "days") %>% as_data_frame()
colnames(by_date) <- c("date")

g <- tweets$date %>% count() %>% as.data.frame()
colnames(g) <- c("date","tweet_count")

# Joining with other data

by_date <- left_join(by_date, g)
by_date[is.na(by_date)] <- 0
by_date <- by_date %>% mutate(weekday = weekdays(date))
by_date <- left_join(by_date, dow)
by_date <- left_join(by_date, sp500)
by_date <-  by_date %>% mutate(speech = if_else(date %in% speeches$date, 1, 0))
by_date <- left_join(by_date, schedules)

by_date %>% mutate(index = 1:nrow(by_date)) %>%
  ggplot(aes(index, tweet_count)) + geom_bar(stat = "identity") +
  xlab("days since 2014/1/1") + ylab("daily tweets")

## Making multiple variables from schedule strings

sched_strings <- c("arrives", "departs", "press conference",
   "THE FIRST LADY", "has no public events scheduled", "Pool Call Time",
   "meets", "hosts", "signs", "intelligence briefing", "participates",
   "In-Town", "Out-of-Town", "Vice President",
   "Secretary of State", "Sarah Sanders", "remarks")

# Function for detecting a specific string
in_string <- function(x) {
  if_else(str_detect(by_date$sched_text,x) ==T, 1, 0)
}

# Creating a matrix
phrase_matrix <- sapply(sched_strings, in_string)
phrase_df <- phrase_matrix %>% as_data_frame()
colnames(phrase_df) <- paste0("sched_contains_", sched_strings)
phrase_df <- phrase_df %>% tail(514)
phrase_df <- phrase_df %>% mutate(date = tail(by_date$date,514))

# Adding to time series df
by_date <- left_join(by_date, phrase_df)

