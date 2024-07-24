## 
library(tidyverse)
library(patchwork)

# import
topics = read_rds("data/top_tweet_topic.rds")
congress_tweets = read_rds("data/tweets_labeled")
sentiment = read_rds("data/tweets_sentiment.rds")


# add topics to tweets

# test = topics |> 
#   count(tweet_id) 
# 
# arr = test |> 
#   arrange(desc(n)) |> 
#   mutate(duplicate = if_else(n > 1, 1, 0))
# 
# data = topics |> 
#   left_join(arr, by = "tweet_id") |> 
#   mutate(topic = if_else(duplicate == 1, "multiple topics", topic)) |> 
#   distinct(tweet_id, .keep_all = T)
# 
# tweets_with_topics = congress_tweets |> 
#   left_join(data, by = c("id.x" = "tweet_id"))
# 
# write_rds(tweets_with_topics, "data/tweets_with_topics.rds")

# import this file 
data = read_rds("data/tweets_with_topics.rds")
topics = read_csv("data/group_topics.csv")


weekly_topics = data |> 
  filter(!is.na(topic)) |> 
  group_by(week, year) |> 
  mutate(start_date = min(day)) |> 
  group_by(group, topic, start_date) |> 
  count() |> 
  group_by(start_date, group) |> 
  mutate(tot_tweets = sum(n)) |> 
  ungroup() |> 
  mutate(pct_topic = n/tot_tweets)

monthly_topics = data |> 
  filter(!is.na(topic)) |> 
  mutate(month = month(time)) |> 
  group_by(month, year) |> 
  mutate(start_date = min(day)) |> 
  group_by(group, topic, start_date) |> 
  count() |> 
  group_by(start_date, group) |> 
  mutate(tot_tweets = sum(n)) |> 
  ungroup() |> 
  mutate(pct_topic = n/tot_tweets)

## figures 

# heatmap of topics 
topics |> 
  pivot_longer(`Climate & Infrastructure`:`Covid-19`, names_to = "topic", values_to = "post_prob") |> 
  ggplot(aes(group, topic, fill = post_prob))+
  geom_tile()+
  scale_fill_viridis_c()+
  theme_bw()+
  labs(
    x = NULL,
    y = NULL,
    fill = "Percent of Tweets"
  )

# Elections
monthly_topics |> 
  filter(topic == "elections") |> 
  ggplot(aes(start_date, pct_topic, color = group))+
  geom_line()+
  theme_bw()+
  labs(
    x = NULL,
    y = "Monthly Average Percent",
    color = NULL
  )

# Economy
monthly_topics |> 
  filter(topic == "economy") |> 
  ggplot(aes(start_date, pct_topic, color = group))+
  geom_line()+
  theme_bw()+
  labs(
    title = "Tweets about \"Economy\" Over Time",
    x = NULL,
    y = "Monthly Average Percent",
    color = NULL
  )

# compare tweeters to leaders
monthly_topics |> 
  mutate(label = case_when(
    topic == "activity" ~ "Daily Activities",
    topic == "border" ~ "Border",
    topic == "climate" ~ "Infrastructure/Climate",
    topic == "constituent" ~ "Constituent Service",
    topic == "covid" ~ "Covid-19",
    topic == "economy" ~ "Economy",
    topic == "elections" ~ "Elections",
    topic == "healthcare" ~ "Healthcare",
    topic == "legislation" ~ "Legislation",
    topic == "multiple topics" ~ "Multiple Topics",
    topic == "trump_biden" ~ "Trump/Biden"
  )) |> 
  filter(topic != "multiple topics") |> 
  filter(group %in% c("Tweeter", "Party Leader")) |> 
  ggplot(aes(start_date, pct_topic, color = group))+
  geom_line()+
  theme_bw()+
  facet_wrap(~label)+
  labs(
    x = NULL,
    y = "Percent of Tweets on Topic",
    color = NULL
  )+
  theme(
    legend.position = "bottom"
  )

# compare Tweeters to Rank and File 
monthly_topics |> 
  mutate(label = case_when(
    topic == "activity" ~ "Daily Activities",
    topic == "border" ~ "Border",
    topic == "climate" ~ "Infrastructure/Climate",
    topic == "constituent" ~ "Constituent Service",
    topic == "covid" ~ "Covid-19",
    topic == "economy" ~ "Economy",
    topic == "elections" ~ "Elections",
    topic == "healthcare" ~ "Healthcare",
    topic == "legislation" ~ "Legislation",
    topic == "multiple topics" ~ "Multiple Topics",
    topic == "trump_biden" ~ "Trump/Biden"
  )) |> 
  filter(topic != "multiple topics") |> 
  filter(group %in% c("Tweeter", "Rank-and-File")) |> 
  ggplot(aes(start_date, pct_topic, color = group))+
  geom_line()+
  theme_bw()+
  facet_wrap(~label)+
  labs(
    x = NULL,
    y = "Percent of Tweets on Topic",
    color = NULL
  )+
  theme(
    legend.position = "bottom"
  )

# sentiment plots 

sentiment |> 
  group_by(group) |> 
  summarize(sentiment = mean(sentiment2, na.rm = T),
            subjectivity = mean(subjectivity, na.rm = T))

monthly_sentiment = sentiment |>
  mutate(pct_positive = positive/length,
         pct_negative = negative/length,
         month = month(time)) |>
  group_by(month, year) |> 
  mutate(start_date = min(day)) |> 
  filter(start_date < "2022-12-01") |> 
  group_by(group, start_date) |> 
  summarize(sentiment = mean(sentiment2, na.rm = T),
            subjectivity = mean(subjectivity, na.rm = T),
            positivity = mean(pct_positive, na.rm = T),
            negativity = mean(pct_negative, na.rm = T))

# sentiment 
sent = monthly_sentiment |> 
  filter(group %in% c("Tweeter", "Party Leader")) |> 
  ggplot(aes(start_date, sentiment, color = group))+
  geom_line(alpha = 0.5)+
  geom_smooth(se = F)+
  theme_bw()+
  labs(
    y = "Sentiment",
    x = NULL,
    color = NULL
  )+
  theme(
    legend.position = "none"
  )+
  ylim(0,.16)

monthly_sentiment |> 
  filter(group %in% c("Tweeter", "Party Leader")) |> 
  ggplot(aes(start_date, subjectivity, color = group))+
  geom_line(alpha = 0.5)+
  geom_smooth(se = F)+
  theme_bw()+
  labs(
    y = "Subjectivity",
    x = NULL,
    color = NULL
  )



p = monthly_sentiment |> 
  filter(group %in% c("Tweeter", "Party Leader")) |> 
  ggplot(aes(start_date, positivity, color = group))+
  geom_line(alpha = 0.5)+
  geom_smooth(se = F)+
  theme_bw()+
  labs(
    y = "Positivity",
    x = NULL,
    color = NULL
  )+
  theme(
    legend.position = "none"
  )+
  ylim(0,.15)


n = monthly_sentiment |> 
  filter(group %in% c("Tweeter", "Party Leader")) |> 
  ggplot(aes(start_date, negativity, color = group))+
  geom_line(alpha = 0.5)+
  geom_smooth(se = F)+
  theme_bw()+
  labs(
    y = "Negativity",
    x = NULL,
    color = NULL
  )+
  ylim(0,.15)

p + n

## difference in means 
lm_data = monthly_sentiment |> 
  filter(group %in% c("Tweeter", "Party Leader"))

summary(lm(sentiment ~ group, data = lm_data))
summary(lm(subjectivity ~ group, data = lm_data))
summary(lm(positivity ~ group, data = lm_data))
summary(lm(negativity ~ group, data = lm_data))
