## creating personal crosswalk 


# setup -------------------------------------------------------------------

library(tidyverse)
library(stringr)
library(readxl)

# read in  ----------------------------------------------------------------

bioguide = read_csv("data/bioguides.csv") # from biographical directory of the U.S. Congress

c116 = read_csv("data/116_congress.csv") # from Voteview
c117 = read_csv("data/117_congress.csv") # from Voteview

crosswalk_start = read_csv("data/bioguide_icpsr_crosswalk.csv") %>% 
  select(bioguide, icpsr) 

## mdi databases 
mdi_2020 = read_xlsx("data/Main_candidate_information_2020Congress.xlsx")
mdi_2022 = read_csv("data/Main_candidate_information_2022Congress.csv")

# wrangle -----------------------------------------------------------------

# relevant columns from bioguide
bioguide = bioguide %>% 
  mutate(bio_name = paste(givenName, familyName, sep = " ")) %>% 
  select(id, bio_name)

# relevant columns from voteview 
my_congress = c116 %>% 
  bind_rows(c117) %>% 
  filter(chamber != "President") %>% 
  select(bioname, icpsr) %>% 
  rename("icpsr_name" = "bioname") %>% 
  distinct()

# select relevant columns from MDI dataset 
s_mdi_2020 = mdi_2020 %>% 
  select(candidate, icpsr, bioguideId, candStem) %>% 
  mutate(icpsr = as.numeric(icpsr)) %>% 
  rename("bioguide" = "bioguideId")

s_mdi_2022 = mdi_2022 %>% 
  select(candidate, icpsr, bioguide_id, candStem) %>% 
  mutate(icpsr = as.numeric(icpsr)) %>% 
  rename("bioguide" = "bioguide_id")

mdi_guide = s_mdi_2020 %>% 
  bind_rows(s_mdi_2022) %>% 
  distinct() %>% 
  filter(!is.na(icpsr)) %>% 
  filter(!is.na(bioguide))
  

# create crosswalk --------------------------------------------------------

# begin with McCrain's crosswalk

crosswalk = crosswalk_start %>% 
  full_join(bioguide, by = c("bioguide" = "id")) %>% 
  full_join(my_congress, by = "icpsr") %>% 
  filter(!is.na(icpsr_name)) %>% 
  left_join(mdi_guide, by = c("icpsr", "bioguide"))


crosswalk_missing = crosswalk %>% 
  filter(is.na(candStem))

