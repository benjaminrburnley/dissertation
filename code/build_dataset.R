## compile member counts data set 

library(tidyverse)
library(jsonlite)
library(haven)

# load data
# tweet data 
congress = read_rds("data/congresstweets")
# account metadata
users = fromJSON("data/historical-users-filtered.json")
# voteview 116
c116 = read_csv("data/116_congress.csv")
# voteview 117 
c117 = read_csv("data/117_congress.csv")
# crosswalk 
crosswalk = read_csv("data/bioguide_icpsr_crosswalk.csv")
# house les 
house_les = read_dta("data/house_les.dta")
# senate les 
senate_les = read_dta("data/senate_les.dta")


# wrangle -----------------------------------------------------------------

# get twitter data 
names = users |> 
  filter(chamber %in% c("house", "senate")) |>  
  unnest_longer(col = accounts) |> 
  mutate(account_id = accounts$id, 
         screen_name = accounts$screen_name,
         account_type = accounts$account_type,
         prev_names = accounts$prev_names,
         bioguide = id$bioguide,
         gov_track = id$govtrack) |> 
  select(name:party, account_id:gov_track) |> 
  filter(type == "member")

write_rds(names, "data/congress_tweets_user_info.rds")

# tweets filtered for just 116th Congress, summarized by account
tweets_116 = congress |> 
  filter(time >= "2019-01-03") |> 
  filter(time < "2021-01-03") |> 
  mutate(retweet = str_starts(text, "RT"))

tweet_count_116 = tweets_116 |> 
  filter(retweet == F) |> 
  group_by(user_id) |> 
  summarize(n = n())

retweet_count_116 = tweets_116 |> 
  filter(retweet == T) |> 
  group_by(user_id) |> 
  summarize(n = n())
  

# tweets filtered for just the 116th Congress, summarized by account
tweets_117 = congress |> 
  filter(time >= "2021-01-03") |> 
  filter(time < "2023-01-03") |> 
  mutate(retweet = str_starts(text, "RT"))

tweet_count_117 = tweets_117 |> 
  filter(retweet == F) |> 
  group_by(user_id)  |>  
  summarize(n = n())

retweet_count_117 = tweets_117 |> 
  filter(retweet == T) |> 
  group_by(user_id) |> 
  summarize(n = n())

# official accounts aggregated - 116th
official_tweets_116 = names |> 
  filter(account_type == "office") |> 
  select(-screen_name, -prev_names, -type) |> 
  left_join(tweet_count_116, by = c("account_id" = "user_id")) |> 
  group_by(bioguide) |> 
  summarize(official = sum(n)) |> 
  replace_na(list(official = 0))

official_retweets_116 = names |> 
  filter(account_type == "office") |> 
  select(-screen_name, -prev_names, -type) |> 
  left_join(retweet_count_116, by = c("account_id" = "user_id")) |> 
  group_by(bioguide) |> 
  summarize(official_RT = sum(n)) |> 
  replace_na(list(official_RT = 0))

official_116 = official_tweets_116 |> 
  left_join(official_retweets_116, by = "bioguide")

# campaign accounts aggregated - 116th 
campaign_tweets_116 = names |> 
  filter(account_type == "campaign") |> 
  select(-screen_name, -prev_names, -type) |> 
  left_join(tweet_count_116, by = c("account_id" = "user_id")) |> 
  group_by(bioguide) |> 
  summarize(campaign = sum(n)) |> 
  replace_na(list(campaign = 0))

campaign_retweets_116 = names |> 
  filter(account_type == "campaign") |> 
  select(-screen_name, -prev_names, -type) |> 
  left_join(retweet_count_116, by = c("account_id" = "user_id")) |> 
  group_by(bioguide) |> 
  summarize(campaign_RT = sum(n)) |> 
  replace_na(list(campaign_RT = 0))

campaign_116 = campaign_tweets_116 |> 
  left_join(campaign_retweets_116, by = "bioguide")

# official accounts aggregated  - 117th
official_tweets_117 = names |> 
  filter(account_type == "office") |> 
  select(-screen_name, -prev_names, -type) |> 
  left_join(tweet_count_117, by = c("account_id" = "user_id")) |> 
  group_by(bioguide) |> 
  summarize(official = sum(n)) |> 
  replace_na(list(official = 0))

official_retweets_117 = names |> 
  filter(account_type == "office") |> 
  select(-screen_name, -prev_names, -type) |> 
  left_join(retweet_count_117, by = c("account_id" = "user_id")) |> 
  group_by(bioguide) |> 
  summarize(official_RT = sum(n)) |> 
  replace_na(list(official_RT = 0))

official_117 = official_tweets_117 |> 
  left_join(official_retweets_117, by = "bioguide")

# official accounts aggregated - 117th
campaign_tweets_117 = names |> 
  filter(account_type == "campaign") |> 
  select(-screen_name, -prev_names, -type) |> 
  left_join(tweet_count_117, by = c("account_id" = "user_id")) |> 
  group_by(bioguide) |> 
  summarize(campaign = sum(n)) |> 
  replace_na(list(campaign = 0))

campaign_retweets_117 = names |> 
  filter(account_type == "campaign") |> 
  select(-screen_name, -prev_names, -type) |> 
  left_join(retweet_count_117, by = c("account_id" = "user_id")) |> 
  group_by(bioguide) |> 
  summarize(campaign_RT = sum(n)) |> 
  replace_na(list(campaign_RT = 0))

campaign_117 = campaign_tweets_117 |> 
  left_join(campaign_retweets_117, by = "bioguide")

# filter les for 116 
house_les_116 = house_les |> 
  filter(congress == 116) 

# senate les for 116 
senate_les_116 = senate_les |> 
  filter(congress == 116)

# bind 116 les 
les_116 = house_les_116 |> 
  bind_rows(senate_les_116)

# filter les for 116 
house_les_117 = house_les |> 
  filter(congress == 117) 

# senate les for 116 
senate_les_117 = senate_les |> 
  filter(congress == 117)

# bind 116 les 
les_116 = house_les_116 |> 
  bind_rows(senate_les_116) |> 
  select(-congress, -bioname, -bioguide_id, -born, -died, -party_code)

# bind 117 les 
les_117 = house_les_117 |> 
  bind_rows(senate_les_117) |> 
  select(-congress, -bioname, -bioguide_id, -born, -died, -party_code)
  
  
# build 116th congress data -----------------------------------------------

# full 117th dataset
data_116 = c116 |> 
  filter(chamber != "President") |> 
  left_join(crosswalk, by = "icpsr") |>
  left_join(official_116, by = "bioguide") |> 
  left_join(campaign_116, by = "bioguide") |> 
  left_join(les_116, by = "icpsr") |> 
  mutate(official = if_else(is.na(official), 0, official),
         campaign = if_else(is.na(campaign), 0, campaign),
         all_tweets = official + campaign + official_RT + campaign_RT, 
         tweets_only = official + campaign)

# full 117th dataset
data_117 = c117 |> 
  filter(chamber != "President") |> 
  left_join(crosswalk, by = "icpsr") |> 
  left_join(official_117, by = "bioguide") |> 
  left_join(campaign_117, by ="bioguide") |> 
  left_join(les_117, by = "icpsr") |> 
  mutate(official = if_else(is.na(official), 0, official),
         campaign = if_else(is.na(campaign), 0, campaign),
         all_tweets = official + campaign + official_RT + campaign_RT,
         tweets_only = official + campaign)

# both bound together
data = data_116 |> 
  bind_rows(data_117)

# write datset
write_rds(data, file = "data/member_counts.rds")



