# congress tweets data quality assessment

# packages
library(tidyverse)
library(lubridate)
library(kableExtra)
library(readxl)
library(stringr)
library(tidytext)
library(jsonlite)


# import ------------------------------------------------------------------

# load in tweet data and create simplified date column
data = read_rds("data/congresstweets")

# load in information on members
names = fromJSON("data/historical-users-filtered.json")

# voteview 116th Congress 
bioguide_116 = read_csv("data/bioguide_116.csv")
congress_116 = read_csv("data/116_congress.csv")
bios_116 = read_xlsx("data/Main_candidate_information_2020Congress.xlsx") %>% 
  select(candidate, icpsr, bioguideId)

crosswalk = read_csv("data/bioguide_icpsr_crosswalk.csv") %>% 
  mutate(bioguide = bioguide_id, 
         icpsr = icpsr_id) %>% 
  select(bioguide, icpsr)

# clean -------------------------------------------------------------------

# unnest multiple accounts in the names data and filter for members 
members = names %>% 
  unnest_longer(accounts) %>% 
  filter(type == "member") %>% 
  mutate(user_id = accounts$id,
         username = accounts$screen_name,
         account_type = accounts$account_type,
         prev_name = accounts$prev_name,
         account_party = accounts$party,
         deleted = accounts$deleted,
         account_name = accounts$name,
         type = accounts$type, 
         account_chamber = accounts$chamber,
         thomas_id = id$thomas_id,
         house_committee_id = id$house_committee_id,
         senate_committee_id = id$senate_committee_id,
         tag = id$tag, 
         bioguide = id$bioguide,
         govtrack = id$govtrack) %>% 
select(name:party,user_id:govtrack)

# add simplified date column to data
clean = data %>% 
  mutate(date = date(time),
         year = year(time))

# wrangle -----------------------------------------------------------------

# filter for just the tweets I am interested in 
tweets = clean %>% 
  filter(date >= "2019-01-03") %>% 
  filter(date <= "2023-01-02") %>% 
  mutate(congress = if_else(date >= "2019-01-03" & date <= "2021-01-02", "116th", "117th"))

# summarize by user id 
users = tweets %>% 
  group_by(user_id) %>% 
  summarize(n = n())

# merge with the names dataset 
counts = members %>% 
  left_join(users, by = "user_id") %>% 
  select(name, user_id, username, n, bioguide) 


# check 116th congress ----------------------------------------------------

# filter for 116th congress and aggregate tweets by account
counts_116 = tweets %>% 
  filter(congress == "116th") %>% 
  group_by(user_id) %>% 
  summarize(n = n()) 

# merge counts with member data
members_116 = counts_116 %>% 
  inner_join(members, by = "user_id")

# aggregate at the bioguide id level 
bio_116 = members_116 %>% 
  group_by(bioguide, name) %>%
  summarize(n_tweets = sum(n), .groups = "drop") %>% 
  left_join(crosswalk, by = "bioguide")

# merge to voteview data 
congress_tweets_116 = congress_116 %>% 
  left_join(bio_116, by = "icpsr")

test = congress_tweets_116 %>% 
  filter(is.na(n_tweets))




