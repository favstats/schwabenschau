
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)
library(schwabr)
library(rtweet)

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
  bind_rows(rtweet::search_tweets("from:StN_News", n = 20, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:RegierungBW", n = 20, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:SWRAktuellBW", n = 20, include_rts = F)) %>% 
  distinct(text, .keep_all = T) %>% 
  filter(created_at > lubridate::now() - lubridate::dhours(2)) %>% 
  filter(!(status_id %in% statuses))

ts_rows <- nrow(ts) 

if(ts_rows>10){
  ts <- ts %>% 
    sample_n(10)
  
  ts_rows <- nrow(ts) 
  
}

print("schwabify")


ts_schwabs <- ts %>% 
  rowwise() %>% 
  mutate(schwabtext = get_schwab(text)) %>% 
  ungroup() %>% 
  mutate(link = stringr::str_extract(text, "http[^[:space:]]*"),
         schwabtext = stringr::str_replace(schwabtext, "hddb[^[:space:]]*", link),
         # schwabtext = paste0(schwabtext, " (@", screen_name, ")"),
         schwabtext = str_replace(schwabtext, " inna", ":inna"),
         schwabtext = str_replace(schwabtext, " ungern ", " ogern "),
         schwabtext = str_replace(schwabtext, "Ungern ", "Ogern "),
         schwabtext = str_replace(schwabtext, " auch ", " au "),
         schwabtext = str_replace(schwabtext, "Auch ", "Au ")) %>% 
  distinct(schwabtext, .keep_all = T)




# original_mentions <- ts_schwabs %>% 
#   # tidyr::drop_na(mentions_screen_name) %>% View
#   mutate(mentions = stringr::str_extract_all(text, "@[:alpha:]*"))  %>%
#   tidyr::unnest(mentions) %>% 
#   tidyr::drop_na(mentions) %>% 
#   select(status_id, mentions)


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
                      include_rts = F)  %>% 
  filter(!(status_id %in% replies)) %>% 
  filter(as.Date(created_at) >= lubridate::today())
  
if (nrow(schwabtweets) == 0){
  print("No replies.")
} else {
  
  print(paste0("tweet out ", nrow(schwabtweets), " replies."))
  
  replytweets <- schwabtweets %>% 
    rowwise() %>% 
    mutate(schwabtext = get_schwab(text)) %>% 
    ungroup() %>% 
    mutate(schwabtext = str_replace_all(schwabtext, "@schwabenschau", "") %>% 
             str_squish() %>% paste0(" #schwabify"))
  
  post_tweet(status = replytweets$schwabtext, in_reply_to_status_id = replytweets$status_id, auto_populate_reply_metadata = T)
  
  
  replytweets %>% 
    split(1:nrow(.)) %>% 
    purrr::walk(~{
      
      print(.x$schwabtext)
      
      post_tweet(status = .x$schwabtext, in_reply_to_status_id = .x$status_id, auto_populate_reply_metadata = T)
      
      Sys.sleep(60)
      
    })
  
  replies <- replytweets %>% 
    pull(status_id)
  
  cat(replies, file = "replies.txt", sep = "\n", append = T)
  
}



