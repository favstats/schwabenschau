
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(rvest)
library(rtweet)
library(rex)
library(R6)
library(processx)
library(jsonlite)

rvest::read_html("https://github.com/rstudio/chromote/tree/master/R") %>% 
  rvest::html_elements(".Box-row") %>% 
  rvest::html_elements(".flex-auto") %>% 
  rvest::html_elements(".js-navigation-open") %>% 
  rvest::html_text() %>% 
  .[str_detect(., "\\.R")] %>% 
  paste0("https://raw.githubusercontent.com/rstudio/chromote/master/R/", .) %>% 
  walk(source)

source("utils.R")
source("https://raw.githubusercontent.com/favstats/schwabr/master/R/connect.R")

source("https://raw.githubusercontent.com/rstudio/webshot2/master/R/webshot.R")
source("https://raw.githubusercontent.com/rstudio/webshot2/master/R/utils.R")


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

ts <- rtweet::search_tweets("from:tagesschau", n = 10, include_rts = F) %>% 
  bind_rows(rtweet::search_tweets("from:StZ_NEWS", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:SZ", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:Schwaebische", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:stuttgart_stadt", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:BWjetzt", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:RegierungBW", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:SWRAktuellBW", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:PolizeiUL", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:PolizeiHN", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:PolizeiLB", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:bpol_bw", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:PolizeiAalen", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:MSI_BW", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:LkaBaWue", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:PolizeiRT", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:WM_BW", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:Uni_Stuttgart", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:KM_BW", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:UmweltBW", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:visitbawu", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:FinanzenBW", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:Landtag_BW", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:enjoy_stuttgart", n = 10, include_rts = F)) %>% 
  bind_rows(rtweet::search_tweets("from:jensspahn", n = 10, include_rts = F)) %>% 
  distinct(text, .keep_all = T) %>% 
  filter(created_at > lubridate::now() - lubridate::dhours(2)) %>% 
  filter(!(status_id %in% statuses)) %>% 
  filter(is.na(reply_to_status_id))

ts_rows <- nrow(ts) 

if(ts_rows>10){
  ts <- ts %>% 
    sample_n(5)
} 

if(ts_rows==0){
  
  print("nothing to tweet")
  
} else {
  
  print("schwabify")
  
  if(!dir.exists("img")) dir.create("img")
  
  ts_schwabs <- ts %>% 
    # print_data %>% 
    rowwise() %>% 
    mutate(schwabtext = get_schwab(text)) %>% 
    # print_data %>% 
    ungroup() %>% 
    clean_schwabtext() %>% 
    # print_data %>% 
    distinct(schwabtext, .keep_all = T) %>% 
    print_data %>% 
    replace_links() %>% 
    print_data %>% 
    replace_mentions() %>% 
    print_data %>% 
    distinct(status_id, .keep_all = T)  %>% 
    print_data %>% 
    mutate(schwabtext = str_replace_all(schwabtext, "&amb;", "&"))
  
  ts_rows <- nrow(ts_schwabs) 

  print(paste0("tweet out ", ts_rows, " tweets."))
    
    # post_tweet(status = ts_schwabs$schwabtext[1], in_reply_to_status_id = ts_schwabs$status_id[1], auto_populate_reply_metadata = T)
    
  ts_schwabs %>% 
    split(1:nrow(.)) %>% 
    purrr::walk(~{
        
        print(.x$schwabtext)
      
        get_tweet_screenshots(.x$status_id)
        
        post_tweet(status = .x$schwabtext,
                   media = paste0("img/", .x$status_id, ".png"))
        
        Sys.sleep(60)
        
      })
    
    statuses <- ts_schwabs %>% 
      pull(status_id)
    
    cat(statuses, file = "statuses.txt", sep = "\n", append = T)
    
    
} 




print("send translation replies")

replies <- readLines("replies.txt")

schwabtweets <- rtweet::get_mentions(n = 100, 
                                     include_rts = F,
                                     tweet_mode = "extended")  %>% 
  filter(!(status_id %in% replies)) %>% 
  filter(!(status_in_reply_to_status_id %in% replies)) %>% 
  filter(as.Date(created_at) >= lubridate::today()-lubridate::ddays(1)) %>%
  filter(status_in_reply_to_user_id != "1410272146027433995") %>% 
  filter(!is.na(status_in_reply_to_status_id))


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
    distinct(status_id, .keep_all = T) %>% 
    mutate(schwabtext = str_replace_all(schwabtext, "&amb;", "&"))
  
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

if(lubridate::hour(lubridate::now()) %in% c(18:22)){
  
  install.packages("telegram.bot")
  
  library(telegram.bot)
  
  if(!dir.exists("img")) dir.create("img")
  
  token <- Sys.getenv("schwabenschau_token")  
  
  bot <- Bot(token = token)

  adam <- Sys.getenv("adam_telegram")  
  
  statuses_telegram <- readLines("telegram.txt") %>% unique()

  my_tweets <- rtweet::get_timeline("schwabenschau", n = 100) %>% 
    distinct(text, .keep_all = T) %>% 
    filter(created_at > lubridate::now() - lubridate::dhours(12)) %>% 
    filter(!(status_id %in% statuses_telegram)) %>% 
    sample_n(3, replace = 3) %>% 
    distinct(text, .keep_all = T) %>% 
    mutate(tweet_url = paste0("https://twitter.com/schwabenschau/status/", status_id))
  
  my_tweets %>% 
    split(1:nrow(.)) %>% 
    purrr::walk(~{
      
      
      try({
        print(.x$text)
        
        print("get screenshot")
        
        if(!file.exists(paste0("img/", .x$status_id, ".png"))){
          get_tweet_screenshots(.x$status_id, 
                                s = "#image_result > div:nth-child(3) > div:nth-child(2) > img",
                                d = 20)
          
        }
        
        
        print("send photo")
        
        bot$sendPhoto(
          chat_id = adam,
          photo = paste0("img/", .x$status_id, ".png"),
          caption	= .x$tweet_url
        )  
        
        print("jetzt sleeped er")
        
        Sys.sleep(10)        
      })

      
    })
  
  telegram_posts <- my_tweets %>% 
    pull(status_id)
  
  cat(telegram_posts, file = "telegram.txt", sep = "\n", append = T)
  
  
}


if(dir.exists("img")) unlink("img", recursive=TRUE)

# https://pikaso.me/pricing
# https://tweetpik.com/dashboard
# https://www.bannerbear.com/demos/tweetagram?tweet_id=1423889471808802816
# #image_result > div:nth-child(3) > div:nth-child(2) > img

