
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(rvest)
library(rex)
library(R6)
library(processx)
library(jsonlite)
library(fastmap)
library(later)
library(websocket)
library(promises)
library(rlang)
library(curl)

library(atrrr)


# 
# rvest::read_html("https://github.com/rstudio/chromote/tree/master/R") %>% 
#   rvest::html_elements(".Box-row") %>% 
#   rvest::html_elements(".flex-auto") %>% 
#   rvest::html_elements(".js-navigation-open") %>% 
#   rvest::html_text() %>% 
#   .[str_detect(., "\\.R")] %>% 
#   paste0("https://raw.githubusercontent.com/rstudio/chromote/master/R/", .) %>% 
#   walk(source)

source("utils.R")
source("https://raw.githubusercontent.com/favstats/schwabr/master/R/connect.R")

source("https://raw.githubusercontent.com/rstudio/webshot2/master/R/webshot.R")
source("https://raw.githubusercontent.com/rstudio/webshot2/master/R/utils.R")


print("authenticate")

# Create a token containing your blue sky key

atrrr::auth(user = Sys.getenv("bsky_name"), password = Sys.getenv("bsky_pw"))

print("get skeets")

statuses <- readLines("statuses.txt") %>% unique()

schwabenbase <- c("oezdemir.de",
                  "regierungbw.bsky.social",
                  "bwegt.bsky.social",
                  "unistuttgart.bsky.social",
                  "stuttgarterzeitung.bsky.social",
                  "spiegelonline.bsky.social",
                  "tagesspiegel.de", 
                  "belltowernews.bsky.social", 
                  "netzpolitik.org",
                  "table.media", 
                  "uebermedien.de", 
                  "taz.de", 
                  "correctiv.org",  
                  "analysekritik.bsky.social",
                  "der-postillon.com")

# savee <- atrrr::get_skeets_authored_by("der-postillon.com") 
# 
# 
# savee %>%
#   slice(1:2)  %>% pull(record)
#   mutate(links = record |> map_chr(~{.x$embed$external$uri}))  
#   

# savee %>% 
#   mutate(handle = author |> map_chr(~{.x$handle}))  %>%
#   mutate(text = record |> map_chr(~{.x$text}))  %>% 
#   mutate(links = record |> map_chr(~{
#     # print(.x$embed$external$uri)
#   if(length(.x$embed$external$uri)!=0){
#     # print("hi")
#     return(.x$embed$external$uri)
#   } else {
#     # print("hiss")
#     return(NA)
#   }
#             }
#   )) %>%
#   mutate(links = ifelse(is.na(links), paste0("@", handle), links)) %>% View()
#   
#   
gettem <- purrr::possibly(atrrr::get_skeets_authored_by, otherwise = NULL, quiet = F)

ts <- schwabenbase %>% 
  # .[2:3] %>% 
  map_dfr(~{
    print(.x)
    gettem(.x) %>% 
      mutate(handle = author_handle)  %>%
      mutate(links = uri) %>%
      mutate(links = ifelse(is.na(links), paste0("@", handle), links)) %>% 
      mutate_all(as.character) %>% 
      mutate(skeet_author = .x) %>% 
      filter(skeet_author == handle) 
  }) %>% 
  distinct(text, .keep_all = T) %>% 
  mutate(created_at = lubridate::ymd_hms(indexed_at)) %>% 
  filter(created_at > lubridate::now() - lubridate::dhours(3)) %>% 
  filter(!(uri %in% statuses)) 


post_skeet <- purrr::possibly(post_skeet, otherwise = NULL, quiet = F)


ts_rows <- nrow(ts) 

if(ts_rows>10){
  ts <- ts %>% 
    sample_n(5)
} 

if(ts_rows==0){
  
  print("nothing to tweet")
  
} else {
  
  print("schwabify")
  
  # if(!dir.exists("img")) dir.create("img")
  
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
    replace_links(uri) %>% 
    print_data %>% 
    replace_mentions(uri) %>% 
    print_data %>% 
    distinct(uri, .keep_all = T)  %>% 
    print_data %>% 
    mutate(schwabtext = str_replace_all(schwabtext, "&amb;", "&")) %>% 
    mutate(schwabtext = paste0(schwabtext, "\n\nGeklaud vo:"))
  
  ts_rows <- nrow(ts_schwabs) 
  
  print(paste0("tweet out ", ts_rows, " tweets."))
  
  # post_tweet(status = ts_schwabs$schwabtext[1], in_reply_to_status_id = ts_schwabs$status_id[1], auto_populate_reply_metadata = T)
  
  ts_schwabs %>% 
    split(1:nrow(.)) %>% 
    purrr::walk(~{
      
      print(.x$schwabtext)
      
      # get_tweet_screenshots(.x$id)
      
      # print(paste0("img/", as.character(.x$id), ".png"))
      
      post_skeet(text = .x$schwabtext, quote = .x$uri)
      
      Sys.sleep(60)
      
    })
  
  statuses <- ts_schwabs %>% 
    pull(uri)
  
  cat(statuses, file = "statuses.txt", sep = "\n", append = T)
  
  
} 




# print("send translation replies")

# replies <- readLines("replies.txt")
# 
# yp <-  rtweet::get_mentions(n = 100, 
#                             include_rts = F,
#                             tweet_mode = "extended") 
# 
# schwabtweets <- rtweet::get_mentions(n = 100, 
#                                      include_rts = F,
#                                      tweet_mode = "extended")  %>% 
#   filter(!(id %in% replies)) %>% 
#   filter(!(in_reply_to_status_id  %in% replies)) %>% 
#   filter(as.Date(created_at) >= lubridate::today()-lubridate::ddays(1)) %>%
#   filter(in_reply_to_status_id  != "1410272146027433995") %>% 
#   filter(!is.na(in_reply_to_status_id ))
# 
# 
# if (nrow(schwabtweets) == 0){
#   print("No replies.")
# } else {
#   
#   original_tweets <- rtweet::lookup_tweets(statuses = schwabtweets$in_reply_to_status_id)
#   
#   replytweets <- original_tweets %>% 
#     rowwise() %>%
#     mutate(schwabtext = get_schwab(text)) %>%
#     ungroup()  %>% 
#     clean_schwabtext()  %>% 
#     replace_links() %>% 
#     replace_mentions() %>% 
#     select(original_id = id, schwabtext, contains("media_url")) %>% 
#     left_join(schwabtweets %>% select(original_id = in_reply_to_status_id , id))  %>% 
#     distinct(id, .keep_all = T) %>% 
#     mutate(schwabtext = str_replace_all(schwabtext, "&amb;", "&"))
#   
#   print(paste0("tweet out ", nrow(replytweets), " replies."))
#   
#   replytweets %>% 
#     split(1:nrow(.)) %>% 
#     purrr::walk(~{
#       
#       print(.x$schwabtext)
# 
#       post_tweet(status = .x$schwabtext,
#                  in_reply_to_status_id = .x$id, 
#                  auto_populate_reply_metadata = T)
#       
#       Sys.sleep(60)
#       
#     })
#   
#   replies <- replytweets %>% 
#     pull(id)
#   
#   cat(replies, file = "replies.txt", sep = "\n", append = T)
#   
# }
# 
# print(lubridate::now())
# 
# print(lubridate::hour(lubridate::now()))
# 
# if(lubridate::wday(lubridate::now()) %in% sample(1:365, size = 1)){
#   
#   install.packages("telegram.bot")
#   
#   library(telegram.bot)
#   
#   if(!dir.exists("img")) dir.create("img")
#   
#   token <- Sys.getenv("schwabenschau_token")  
#   
#   bot <- Bot(token = token)
# 
#   adam <- Sys.getenv("adam_telegram")  
#   
#   statuses_telegram <- readLines("telegram.txt") %>% unique()
# 
#   my_tweets <- rtweet::get_timeline("schwabenschau", n = 100) %>% 
#     distinct(text, .keep_all = T) %>% 
#     filter(created_at > lubridate::now() - lubridate::dhours(12)) %>% 
#     filter(!(id %in% statuses_telegram)) %>% 
#     sample_n(3, replace = 3) %>% 
#     distinct(text, .keep_all = T) %>% 
#     mutate(tweet_url = paste0("https://twitter.com/schwabenschau/status/", id))
#   
#   my_tweets %>% 
#     split(1:nrow(.)) %>% 
#     purrr::walk(~{
#       
#       
#       try({
#         print(.x$text)
#         
#         print("get screenshot")
#         
#         if(!file.exists(paste0("img/", .x$id, ".png"))){
#           get_tweet_screenshots(.x$id, 
#                                 s = "#image_result > div:nth-child(3) > div:nth-child(2) > img",
#                                 d = 20)
#           
#         }
#         
#         
#         print("send photo")
#         
#         bot$sendPhoto(
#           chat_id = adam,
#           photo = paste0("img/", .x$id, ".png"),
#           caption	= .x$tweet_url
#         )  
#         
#         print("jetzt sleeped er")
#         
#         Sys.sleep(10)        
#       })
# 
#       
#     })
#   
#   telegram_posts <- my_tweets %>% 
#     pull(id)
#   
#   cat(telegram_posts, file = "telegram.txt", sep = "\n", append = T)
#   
#   
# }
# 
# 
# if(dir.exists("img")) unlink("img", recursive=TRUE)

# https://pikaso.me/pricing
# https://tweetpik.com/dashboard
# https://www.bannerbear.com/demos/tweetagram?tweet_id=1423889471808802816
# #image_result > div:nth-child(3) > div:nth-child(2) > img
