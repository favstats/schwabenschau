
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(schwabr)
library(rtweet)
library(rex)

source("utils.R")

print("authenticate")

# Create a token containing your Twitter keys
rtweet::create_token(
  app = "schwabschau",  # the name of the Twitter app
  consumer_key = Sys.getenv("consumer_key"),
  consumer_secret = Sys.getenv("consumer_secret"),
  access_token = Sys.getenv("token"),
  access_secret = Sys.getenv("secret")
)

print("get tweets")

statuses <- readLines("statuses.txt") %>% unique()

ts <- rtweet::search_tweets("from:tagesschau", n = 20, include_rts = F) %>% 
  bind_rows(rtweet::search_tweets("from:StZ_NEWS", n = 20, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:SZ", n = 20, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:Schwaebische", n = 20, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:RegierungBW", n = 20, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:SWRAktuellBW", n = 20, include_rts = F)) %>% 
  distinct(text, .keep_all = T) %>% 
  filter(created_at > lubridate::now() - lubridate::dhours(2)) %>% 
  filter(!(status_id %in% statuses)) %>% 
  filter(is.na(reply_to_status_id))

ts_rows <- nrow(ts) 

if(ts_rows>10){
  ts <- ts %>% 
    sample_n(5)
}

print("schwabify")


ts_schwabs <- ts %>% 
  rowwise() %>% 
  mutate(schwabtext = get_schwab(text)) %>% 
  ungroup() %>% 
  clean_schwabtext() %>% 
  distinct(schwabtext, .keep_all = T) %>% 
  replace_links() %>% 
  replace_mentions() %>% 
  distinct(status_id, .keep_all = T) 

ts_rows <- nrow(ts) 

if(ts_rows==0){
  
  print("nothing to tweet")
  
} else {
  print(paste0("tweet out ", ts_rows, " tweets."))
  
  # post_tweet(status = ts_schwabs$schwabtext[1], in_reply_to_status_id = ts_schwabs$status_id[1], auto_populate_reply_metadata = T)

 ts_schwabs %>% 
  split(1:nrow(.)) %>% 
  purrr::walk(~{
    
    print(.x$schwabtext)
    
    post_tweet(status = .x$schwabtext)
    
    Sys.sleep(60)
    
  })
 
 statuses <- ts_schwabs %>% 
   pull(status_id)
 
 cat(statuses, file = "statuses.txt", sep = "\n", append = T)
 

}

print("send translation replies")

replies <- readLines("replies.txt")

schwabtweets <- rtweet::get_mentions(n = 200, 
                      include_rts = F,
                      tweet_mode = "extended")  %>% 
  filter(!(status_id %in% replies)) %>% 
  filter(as.Date(created_at) >= lubridate::today()-lubridate::ddays(1)) %>% 
  filter(status_in_reply_to_status_id != "1410272146027433995" | !is.na(status_in_reply_to_status_id))
  
if (nrow(schwabtweets) == 0){
  print("No replies.")
} else {
  
  original_tweets <- rtweet::lookup_tweets(statuses = schwabtweets$status_in_reply_to_status_id)
  
  replytweets <- original_tweets %>% 
    rowwise() %>%
    mutate(schwabtext = get_schwab(text)) %>%
    ungroup()  %>% 
    clean_schwabtext()  %>% 
    replace_links() %>% 
    replace_mentions() %>% 
    select(original_id = status_id, schwabtext, contains("media_url")) %>% 
    left_join(schwabtweets %>% select(original_id = status_in_reply_to_status_id, status_id))  %>% 
    distinct(status_id, .keep_all = T) 
  
  print(paste0("tweet out ", nrow(replytweets), " replies."))
  
  replytweets %>% 
    split(1:nrow(.)) %>% 
    purrr::walk(~{
      
      print(.x$schwabtext)

      post_tweet(status = .x$schwabtext,
                 in_reply_to_status_id = .x$status_id, 
                 auto_populate_reply_metadata = T)
      
      Sys.sleep(60)
      
    })
  
  replies <- replytweets %>% 
    pull(status_id)
  
  cat(replies, file = "replies.txt", sep = "\n", append = T)
  
}

