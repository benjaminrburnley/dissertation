# test 
library(tidyverse)
library(jsonlite)

# MDI Data 
c116 = load("data/congress_116.Rdata")
c117 = load("data/congress_117.Rdata")

one = congress_116 %>% 
  select(username, name) %>% 
  distinct()
two = congress_117 %>% 
  select(username, name) %>% 
  distinct()

names = one %>% 
  bind_rows(two) %>% 
  distinct()

test = filter(congress_116, username == "senatorisakson")

# remove to save memory 
rm(list = ls())
gc()

# congress tweets data
congress = read_rds("data/congresstweets")

names = congress %>% 
  select(screen_name) %>% 
  distinct()

hist = fromJSON("data/historical-users-filtered.json")
