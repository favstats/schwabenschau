
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(schwabr)
library(rtweet)

download_imgs <- function(x, media, id) {
  
  
  img_dat <- x %>% 
    rename(media_link = {{media}},
           id_status = {{id}}) %>% 
    mutate(media_extracted = paste0(media_link, "")) %>% 
    filter(media_extracted != "NA") 
  
  if(nrow(img_dat)!=0){
    
    if(!dir.exists("tmp")) dir.create("tmp")
    
    img_dat %>% 
      unnest(media_link) %>% 
      split(1:nrow(.)) %>% 
      walk(~{
        
        tmp_path <- glue::glue("tmp/{.x$id_status}")
        
        if(!dir.exists(tmp_path)) dir.create(tmp_path)
        
        download.file(.x$media_link, glue::glue("{tmp_path}/{basename(.x$media_link)}"), mode = "wb")
      })
  }

}


clean_schwabtext <- function(x){
  x %>%
    mutate(link = stringr::str_extract(text, "http[^[:space:]]*"),
           schwabtext = stringr::str_replace(schwabtext, "hddb[^[:space:]]*", link),
           schwabtext = str_replace(schwabtext, " inna", ":inna"),
           schwabtext = str_replace(schwabtext, " ungern ", " ogern "),
           schwabtext = str_replace(schwabtext, "Ungern ", "Ogern "),
           schwabtext = str_replace(schwabtext, " auch ", " au "),
           schwabtext = str_replace(schwabtext, "Auch ", "Au "),
           schwabtext = str_remove_all(schwabtext, "hddb[^[:space:]]*"),
           schwabtext = ifelse(str_count(schwabtext > 280), str_trunc(schwabtext, 280), schwabtext)) 
}


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
  filter(!(status_id %in% statuses)) %>% 
  filter(is.na(reply_to_status_id))

ts_rows <- nrow(ts) 

if(ts_rows>10){
  ts <- ts %>% 
    sample_n(5)
  
  ts_rows <- nrow(ts) 
  
}

print("schwabify")


ts_schwabs <- ts %>% 
  rowwise() %>% 
  mutate(schwabtext = get_schwab(text)) %>% 
  ungroup() %>% 
  clean_schwabtext() %>% 
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
    
    .x %>% 
      download_imgs(media_url, status_id)
    
    img_path <- glue::glue("tmp/{.x$status_id}")
    
    if(dir.exists(img_path)){
      
      imgs <- dir(img_path, full.names = T)
      
    } else {
      
      imgs <- NULL
      
    }  
    
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
    clean_schwabtext() %>% 
    select(original_id = status_id, schwabtext, contains("media_url")) %>% 
    left_join(schwabtweets %>% select(original_id = status_in_reply_to_status_id, status_id)) 
  
  
  
  print(paste0("tweet out ", nrow(replytweets), " replies."))
  
  replytweets %>% 
    split(1:nrow(.)) %>% 
    purrr::walk(~{
      
      print(.x$schwabtext)
      
      .x %>% 
        download_imgs(media_url, original_id)
      
      img_path <- glue::glue("tmp/{.x$original_id}")
      
      if(dir.exists(img_path)){
        
        imgs <- dir(img_path, full.names = T)
        
      } else {
        
        imgs <- NULL
        
      }
      
      post_tweet(status = .x$schwabtext,
                 in_reply_to_status_id = .x$status_id, 
                 auto_populate_reply_metadata = T,
                 media = imgs)
      
      Sys.sleep(60)
      
    })
  
  replies <- replytweets %>% 
    pull(status_id)
  
  cat(replies, file = "replies.txt", sep = "\n", append = T)
  
}


unlink("tmp", recursive = T)

