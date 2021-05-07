# Reeperbahn Pop-N-Politics

# Analyze your data using Google Surveys and R

# By Michael Fichman, University of Pennsylvania Weitzman School of Design, PennPraxis
# mfichman@upenn.edu

# Load packages
require(tidyverse)
require(sf)
require(lubridate)

# Let's look at our survey, which was published here:

# https://docs.google.com/forms/d/e/1FAIpQLSdmjnR2IBgbcRX3cE6IpBEpstT6JsLrYVXENKT2SPFh4Tm6iA/viewform


# Publish your survey to a Google Sheet
# Using the File menu, publish the sheet to the web as a csv

# Paste the URL into this call here and get started!

myData <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTcXrNEYBwqG14X9Hf1QoLSLLGBaQpANaCHG4q0oCC3u0c2dxqGxWM49rZjRZqPn_IswuJOriDaH2MF/pub?gid=416277888&single=true&output=csv")

names(myData) <- c("timestamp", "country", "city", "privacyPhotos", "privacyGPS",
                   "privacyTransport", "privacyCreditCard", "privacyTickets",
                   "privacyCOVID", "sceneHealth")

# Filter out test data

myData <- myData %>%
  mutate(timestamp = mdy_hms(timestamp)) %>%
  slice(13:n())

# Country ggplot

ggplot()+
  geom_bar(data = myData, aes(country))+
  coord_flip()

# Question 1

myData %>%
  select(matches("privacy")) %>%
  gather() %>%
  mutate(key = case_when(key == "privacyPhotos" ~ "Digital Photos Inside a Club",
                         key == "privacyGPS" ~ "GPS Location Data",
                         key == "privacyTransport" ~ "Transport Ticket Records",
                         key == "privacyCreditCard" ~ "Credit Card Records",
                         key == "privacyTickets" ~ "Ticketing Receipts",
                         key == "privacyCOVID" ~ "COVID testing results")) %>%
  ggplot()+
  geom_bar(aes(value))+
  coord_flip()+
  facet_wrap(~key)

myData %>%
  select(matches("privacy")) %>%
  gather() %>%
  mutate(key = case_when(key == "privacyPhotos" ~ "Digital Photos Inside a Club",
                         key == "privacyGPS" ~ "GPS Location Data",
                         key == "privacyTransport" ~ "Transport Ticket Records",
                         key == "privacyCreditCard" ~ "Credit Card Records",
                         key == "privacyTickets" ~ "Ticketing Receipts",
                         key == "privacyCOVID" ~ "COVID testing results")) %>%
  mutate(score = case_when(str_detect(value, "1.") == TRUE ~ 1,
                           str_detect(value, "2.") == TRUE ~ 2,
                           str_detect(value, "3.") == TRUE ~ 3)) %>%
  group_by(key) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  ggplot()+
  geom_bar(aes(x = key, y = mean_score), stat = "identity")+
  coord_flip()

# Question 2

# Overall
myData %>%
  ggplot()+
  geom_bar(aes(sceneHealth))
  