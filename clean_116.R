#      _           _                       _          _            _       _  
#     / /\        / /\                   /\ \        /\ \         / /\    / /\
#    / /  \      / /  \                 /  \ \       \_\ \       / / /   / / /
#   /_/ /\ \    /_/ /\ \               / /\ \_\      /\__ \     / /_/   / / / 
#   \_\/\ \ \   \_\/\ \ \             / / /\/_/     / /_ \ \   / /\ \__/ / /  
#        \ \ \       \ \ \           / /_/_        / / /\ \ \ / /\ \___\/ /   
#         \ \ \       \ \ \         / /___/\      / / /  \/_// / /\/___/ /    
#          \ \ \       \ \ \       / /\__ \ \    / / /      / / /   / / /     
#         __\ \ \___  __\ \ \___  / / /__\ \ \  / / /      / / /   / / /      
#        /___\_\/__/\/___\_\/__/\/ / /____\ \ \/_/ /      / / /   / / /       
#        \_________\/\_________\/\/__________\/\_\/       \/_/    \/_/        
#   


#
##
###
#### 116th Congress Aggregation and Cleaning 
### October 10, 2023
## 
#

# libraries ---------------------------------------------------------------
library(tidyverse)
library(lubridate)


# data --------------------------------------------------------------------
# voteview collection of all memebers and their ideal points 
c116 = read_csv("data/116_congress.csv")

####  twitter data collected by MDI - pulled on October 10, 2023

# ----------------------------- 2019 --------------------------------------
# campaign accounts - 2019
c_01_19 = read_csv("data/2019_twint/Campaign_01_2019.csv") # campaigns/jan
c_02_19 = read_csv("data/2019_twint/Campaign_02_2019.csv") # campaigns/feb
c_03_19 = read_csv("data/2019_twint/Campaign_03_2019.csv", col_types = cols(quote_url = "c")) # campaigns/mar - problem - fixed
c_04_19 = read_csv("data/2019_twint/Campaign_04_2019.csv") # campaigns/apr
c_05_19 = read_csv("data/2019_twint/Campaign_05_2019.csv") # campaigns/may
c_06_19 = read_csv("data/2019_twint/Campaign_06_2019.csv", col_types = cols(quote_url = "c")) # campaigns/jun - problem- fixed
c_07_19 = read_csv("data/2019_twint/Campaign_07_2019.csv") # campaigns/jul
c_08_19 = read_csv("data/2019_twint/Campaign_08_2019.csv") # campaigns/aug
c_09_19 = read_csv("data/2019_twint/Campaign_09_2019.csv") # campaigns/sep
c_10_19 = read_csv("data/2019_twint/Campaign_10_2019.csv") # campaigns/oct
c_11_19 = read_csv("data/2019_twint/Campaign_11_2019.csv") # campaigns/nov
c_12_19 = read_csv("data/2019_twint/Campaign_12_2019.csv") # campaigns/dec

# bind campaign accounts into one 
c_19 = c_01_19 %>% 
  bind_rows(c_02_19, c_03_19, c_04_19, c_05_19, c_06_19,
            c_07_19, c_08_19, c_09_19, c_10_19, c_11_19, c_12_19) %>% 
  mutate(account_type = "Campaign",                                             # create flag for account type
         date = mdy(date))                                                      # correct error in dates

#official accounts - 2019
o_01_19 = read_csv("data/2019_twint/Official_01_2019.csv", col_types = cols(week = "d")) # official/jan
o_02_19 = read_csv("data/2019_twint/Official_02_2019.csv", col_types = cols(week = "d")) # official/feb
o_03_19 = read_csv("data/2019_twint/Official_03_2019.csv", col_types = cols(week = "d")) # official/mar
o_04_19 = read_csv("data/2019_twint/Official_04_2019.csv") # official/apr
o_05_19 = read_csv("data/2019_twint/Official_05_2019.csv") # official/may
o_06_19 = read_csv("data/2019_twint/Official_06_2019.csv") # official/jun
o_07_19 = read_csv("data/2019_twint/Official_07_2019.csv") # official/jul
o_08_19 = read_csv("data/2019_twint/Official_08_2019.csv") # official/aug
o_09_19 = read_csv("data/2019_twint/Official_09_2019.csv") # official/sep
o_10_19 = read_csv("data/2019_twint/Official_10_2019.csv") # official/oct
o_11_19 = read_csv("data/2019_twint/Official_11_2019.csv") # official/nov
o_12_19 = read_csv("data/2019_twint/Official_12_2019.csv") # official/jan

# bind official tweets from 2019 into one 
o_19 = o_01_19 %>% 
  bind_rows(o_02_19, o_03_19, o_04_19, o_05_19, o_06_19,
            o_07_19, o_08_19, o_09_19, o_10_19, o_11_19, o_12_19) %>% 
  mutate(account_type = "Official",                                             # create flag for account type 
         date = mdy(date))                                                      # correct error in dates 

# ------------------------------ 2020 -------------------------------------
# campaign accounts - 2019
c_01_20 = read_csv("data/2020_twint/Campaign_01_2020.csv") # campaigns/jan
c_02_20 = read_csv("data/2020_twint/Campaign_02_2020.csv") # campaigns/feb
c_03_20 = read_csv("data/2020_twint/Campaign_03_2020.csv", col_types = cols(week = "d")) # campaigns/mar
c_04_20 = read_csv("data/2020_twint/Campaign_04_2020.csv") # campaigns/apr
c_05_20 = read_csv("data/2020_twint/Campaign_05_2020.csv", col_types = cols(quote_url = "c")) # campaigns/may - problem
c_06_20 = read_csv("data/2020_twint/Campaign_06_2020.csv", col_types = cols(quote_url = "c")) # campaigns/jun - problem
c_07_20 = read_csv("data/2020_twint/Campaign_07_2020.csv", col_types = cols(quote_url = "c")) # campaigns/jul - problem 
c_08_20 = read_csv("data/2020_twint/Campaign_08_2020.csv") # campaigns/aug
c_09_20 = read_csv("data/2020_twint/Campaign_09_2020.csv") # campaigns/sep
c_10_20 = read_csv("data/2020_twint/Campaign_10_2020.csv", col_types = cols(quote_url = "c")) # campaigns/oct - problem 
c_11_20 = read_csv("data/2020_twint/Campaign_11_2020.csv", col_types = cols(quote_url = "c")) # campaigns/nov - problem 
c_12_20 = read_csv("data/2020_twint/Campaign_12_2020.csv", col_types = cols(quote_url = "c")) # campaigns/dec

# bind campaign accounts into one 

c_20 = c_01_20 %>% 
  bind_rows(c_02_20, c_03_20, c_04_20, c_05_20, c_06_20,
            c_07_20, c_08_20, c_09_20, c_10_20, c_11_20, c_12_20) %>% 
  mutate(account_type = "Campaign",          # create flag for account type 
         date = mdy(date))                   # fix error in dates 

#official accounts - 2020
o_01_20 = read_csv("data/2020_twint/Official_01_2020.csv", col_types = cols(week = "d")) # official/jan
o_02_20 = read_csv("data/2020_twint/Official_02_2020.csv", col_types = cols(week = "d")) # official/feb
o_03_20 = read_csv("data/2020_twint/Official_03_2020.csv", col_types = cols(week = "d")) # official/mar
o_04_20 = read_csv("data/2020_twint/Official_04_2020.csv") # official/apr
o_05_20 = read_csv("data/2020_twint/Official_05_2020.csv") # official/may
o_06_20 = read_csv("data/2020_twint/Official_06_2020.csv") # official/jun
o_07_20 = read_csv("data/2020_twint/Official_07_2020.csv") # official/jul
o_08_20 = read_csv("data/2020_twint/Official_08_2020.csv") # official/aug
o_09_20 = read_csv("data/2020_twint/Official_09_2020.csv") # official/sep
o_10_20 = read_csv("data/2020_twint/Official_10_2020.csv") # official/oct
o_11_20 = read_csv("data/2020_twint/Official_11_2020.csv") # official/nov
o_12_20 = read_csv("data/2020_twint/Official_12_2020.csv") # official/jan

# bind official tweets from 2020 into one 
o_20 = o_01_20 %>% 
  bind_rows(o_02_20, o_03_20, o_04_20, o_05_20, o_06_20,
            o_07_20, o_08_20, o_09_20, o_10_20, o_11_20, o_12_20) %>% 
  mutate(account_type = "Official",          # create flag for account type 
         date = mdy(date))                   # correct error in dates                      

# merge aggregated 
congress_116 = c_19 %>% 
  bind_rows(c_20, o_19, o_20)


congress_116 %>% 
  mutate(day = day(date)) %>% 
  group_by(day, year) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(day, n, color = factor(year)))+
  geom_point()+
  geom_line()



  


