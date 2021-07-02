
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

ts <- rtweet::search_tweets("from:tagesschau", n = 20, include_rts = F) %>% 
  bind_rows(rtweet::search_tweets("from:StZ_NEWS", n = 20, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:SZ", n = 20, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:Schwaebische", n = 20, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:StN_News", n = 20, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:RegierungBW", n = 20, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:SWRAktuellBW", n = 20, include_rts = F)) %>% 
  distinct(text, .keep_all = T) %>% 
  filter(created_at > lubridate::now() - lubridate::dhours(2)) 

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
         schwabtext = paste0(schwabtext, " (@", screen_name, ")"),
         schwabtext = str_replace(schwabtext, " inna", ":inna"),
         schwabtext = str_replace(schwabtext, " ungern ", " ogern "),
         schwabtext = str_replace(schwabtext, "Ungern ", "Ogern "),
         schwabtext = str_replace(schwabtext, " auch ", " au "),
         schwabtext = str_replace(schwabtext, "Auch ", "au ")) 



if(ts_rows==0){
  
    print("nothing to tweet")
  
} else {
  print(paste0("tweet out ", ts_rows, " tweets."))
  
  # post_tweet(status = ts_schwabs$schwabtext[1], in_reply_to_status_id = ts_schwabs$status_id[1], auto_populate_reply_metadata = T)
  
  
 ts_schwabs %>% 
  split(1:nrow(.)) %>% 
  purrr::walk(~{
    
    print(.x$schwabtext)
    
    post_tweet(status = .x$schwabtext, in_reply_to_status_id = .x$status_id, auto_populate_reply_metadata = T)
    
    Sys.sleep(10)
    
  })
 
 print("now retweet the tweets")
 
 last_tweets <- rtweet::get_timeline(user = "schwabenschau", n = ts_rows) %>% 
   filter(created_at > lubridate::now() - lubridate::dhours(1))
 
 if(nrow(last_tweets)==0){
   
   print("nothing to retweet")
   
 }  else {
   
   last_tweets %>% 
     split(1:nrow(.)) %>% 
     purrr::walk(~{
       
       post_tweet(retweet_id = .x$status_id)
       
       Sys.sleep(10)
       
     })   
   
 }
 

}



