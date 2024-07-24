# dissertation repo

## data

`data/congress_tweets_user_info.rds`
This data set includes individual level user metadata for every person included in the `congresstweets` data set such as name, account id, screen name, bioguide ID, and gov_track id. 

`data/member_counts.Rdata`
This data set includes all members of the 116th and 117th Congresses and their total tweet counts for the entirety of their congress for official and campaign accounts. Data set also includes a lot of helpful member level metadata.

## scripts
`code/build_dataset.R`
Takes raw data and builds member id and member count data sets.

`code/build_daily_tweets.R`
Goes from `congresstweets` data set to daily counts.




