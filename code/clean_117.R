#
##
###
#### 117th Congress Aggregation and Cleaning 
### October 12, 2023
## 
#

# libraries ---------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)


# data --------------------------------------------------------------------
# voteview collection of all memebers and their ideal points 
c117 = read_csv("data/117_congress.csv")
bio = read_xlsx("data/Main_candidate_information_2020Congress.xlsx") %>% 
  mutate(icpsr = as.numeric(icpsr))


# 2021 --------------------------------------------------------------------

# campaign accounts 
c_01_21 = read_csv("data/2021_api/Campaign_01_2021Congress.csv") # campaign/jan
c_02_21 = read_csv("data/2021_api/Campaign_02_2021Congress.csv") # campaign/feb
c_03_21 = read_csv("data/2021_api/Campaign_03_2021Congress.csv") # campaign/mar
c_04_21 = read_csv("data/2021_api/Campaign_04_2021Congress.csv") # campaign/apr
c_05_21 = read_csv("data/2021_api/Campaign_05_2021Congress.csv") # campaign/may
c_06_21 = read_csv("data/2021_api/Campaign_06_2021Congress.csv") # campaign/jun
c_07_21 = read_csv("data/2021_api/Campaign_07_2021Congress.csv") # campaign/jul  
c_08_21 = read_csv("data/2021_api/Campaign_08_2021Congress.csv") # campaign/aug
c_09_21 = read_csv("data/2021_api/Campaign_09_2021Congress.csv") # campaign/sep
c_10_21 = read_csv("data/2021_api/Campaign_10_2021Congress.csv") # campaign/oct
c_11_21 = read_csv("data/2021_api/Campaign_11_2021Congress.csv") # campaign/nov
c_12_21 = read_csv("data/2021_api/Campaign_12_2021Congress.csv") # campaign/dec

c_21 = c_01_21 %>% 
  bind_rows(c_02_21, c_03_21, c_04_21, c_05_21, c_06_21,
            c_07_21, c_08_21, c_09_21, c_10_21, c_11_21, c_12_21) %>% 
  mutate(account_type = "Campaign",
         date = mdy(date),
         date_scraped = mdy(date_scraped))

# official accounts 
o_01_21 = read_csv("data/2021_api/Official_01_2021Congress.csv") # official/jan
o_02_21 = read_csv("data/2021_api/Official_02_2021Congress.csv") # official/feb
o_03_21 = read_csv("data/2021_api/Official_03_2021Congress.csv") # official/mar
o_04_21 = read_csv("data/2021_api/Official_04_2021Congress.csv") # official/apr
o_05_21 = read_csv("data/2021_api/Official_05_2021Congress.csv") # official/may
o_06_21 = read_csv("data/2021_api/Official_06_2021Congress.csv") # official/jun
o_07_21 = read_csv("data/2021_api/Official_07_2021Congress.csv") # official/jul
o_08_21 = read_csv("data/2021_api/Official_08_2021Congress.csv") # official/aug
o_09_21 = read_csv("data/2021_api/Official_09_2021Congress.csv") # official/sep
o_10_21 = read_csv("data/2021_api/Official_10_2021Congress.csv") # official/oct
o_11_21 = read_csv("data/2021_api/Official_11_2021Congress.csv") # official/nov
o_12_21 = read_csv("data/2021_api/Official_12_2021Congress.csv") # official/dec

o_21 = o_01_21 %>% 
  bind_rows(o_02_21, o_03_21, o_04_21, o_05_21, o_06_21,
            o_07_21, o_08_21, o_09_21, o_10_21, o_11_21, o_12_21) %>% 
  mutate(account_type = "Official",
         date = mdy(date))

# 2022 --------------------------------------------------------------------

# campaign accounts 
c_01_22 = read_csv("data/2022_api/Campaign_01_2022Congress.csv") # campaign/jan
c_02_22 = read_csv("data/2022_api/Campaign_02_2022Congress.csv") # campaign/feb
c_03_22 = read_csv("data/2022_api/Campaign_03_2022Congress.csv") # campaign/mar
c_04_22 = read_csv("data/2022_api/Campaign_04_2022Congress.csv") # campaign/apr
c_05_22 = read_csv("data/2022_api/Campaign_05_2022Congress.csv") # campaign/may
c_06_22 = read_csv("data/2022_api/Campaign_06_2022Congress.csv") # campaign/jun
c_07_22 = read_csv("data/2022_api/Campaign_07_2022Congress.csv") # campaign/jul  
c_08_22 = read_csv("data/2022_api/Campaign_08_2022Congress.csv") # campaign/aug
c_09_22 = read_csv("data/2022_api/Campaign_09_2022Congress.csv") # campaign/sep
c_10_22 = read_csv("data/2022_api/Campaign_10_2022Congress.csv") # campaign/oct
c_11_22 = read_csv("data/2022_api/Campaign_11_2022Congress.csv") # campaign/nov
c_12_22 = read_csv("data/2022_api/Campaign_12_2022Congress.csv") # campaign/dec

c_22 = c_01_22 %>% 
  bind_rows(c_02_22, c_03_22, c_04_22, c_05_22, c_06_22,
            c_07_22, c_08_22, c_09_22, c_10_22, c_11_22, c_12_22) %>% 
  mutate(account_type = "Campaign",
         date = mdy(date),
         date_scraped = mdy(date_scraped))

# official accounts 
o_01_22 = read_csv("data/2022_api/Official_01_2022Congress.csv") # official/jan
o_02_22 = read_csv("data/2022_api/Official_02_2022Congress.csv") # official/feb
o_03_22 = read_csv("data/2022_api/Official_03_2022Congress.csv") # official/mar
o_04_22 = read_csv("data/2022_api/Official_04_2022Congress.csv") # official/apr
o_05_22 = read_csv("data/2022_api/Official_05_2022Congress.csv") # official/may
o_06_22 = read_csv("data/2022_api/Official_06_2022Congress.csv") # official/jun
o_07_22 = read_csv("data/2022_api/Official_07_2022Congress.csv") # official/jul
o_08_22 = read_csv("data/2022_api/Official_08_2022Congress.csv") # official/aug
o_09_22 = read_csv("data/2022_api/Official_09_2022Congress.csv") # official/sep
o_10_22 = read_csv("data/2022_api/Official_10_2022Congress.csv") # official/oct
o_11_22 = read_csv("data/2022_api/Official_11_2022Congress.csv") # official/nov
o_12_22 = read_csv("data/2022_api/Official_12_2022Congress.csv") # official/dec

# combine all official accounts from 2022
o_22 = o_01_22 %>% 
  bind_rows(o_02_22, o_03_22, o_04_22, o_05_22, o_06_22,
            o_07_22, o_08_22, o_09_22, o_10_22, o_11_22, o_12_22) %>% 
  mutate(account_type = "Offical", 
         date = mdy(date))

# combine all accounts for both years into one dataframe
congress_117 = o_21 %>% 
  bind_rows(o_22, c_21, c_22)


# find out which members are not included
stems_c117 = c117 %>% 
  anti_join(bio, by = "icpsr")

# save missing members
write_csv(stems_c117, "data/stems17.csv")

# save compiled tweets 
save(congress_117, file = "data/congress_117.Rdata")
