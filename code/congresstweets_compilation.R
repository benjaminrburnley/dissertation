
## read in all tweets 

library(jsonlite)
library(tidyverse)

file_path = "~/Desktop/Projects/dissertation/congresstweets-master/data"

file_list = list.files("~/Desktop/Projects/dissertation/congresstweets-master/data")

tweets = tibble()
# loop 
for(file in file_list){
  x = fromJSON(file.path(file_path, file))
  
tweets = tweets %>% 
  bind_rows(x)
}


write_csv(tweets, "data/congresstweets")
write_rds(tweets, "data/congresstweets")
