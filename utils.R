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
    mutate(schwabtext = str_replace(schwabtext, " inna", ":inna"),
           schwabtext = str_replace(schwabtext, " ungern ", " ogern "),
           schwabtext = str_replace(schwabtext, "Ungern ", "Ogern "),
           schwabtext = str_replace(schwabtext, " auch ", " au "),
           schwabtext = str_replace(schwabtext, "Auch ", "Au "),
           schwabtext = ifelse(str_count(schwabtext > 280), str_trunc(schwabtext, 280), schwabtext)) 
}


replace_links <- function(.data, status = id) {
  
  links_dat <- .data %>% 
    mutate(links = stringr::str_extract_all(text, "http[^[:space:]]*")) %>% 
    unnest(links) 
  
  print("links_dat")
  print(links_dat)
  
  if(nrow(links_dat)!=0){
    schwab_links <- .data %>%  
      mutate(schwablinks = stringr::str_extract_all(schwabtext, "hddb[^[:space:]]*")) %>% 
      unnest(schwablinks) %>% 
      select(schwablinks)
    
    updated_links_dat <- links_dat %>% 
      bind_cols(schwab_links) %>% 
      rowwise() %>% 
      mutate(schwablinks = rex::rex(schwablinks)) %>% 
      ungroup() %>% 
      group_split( {{status}} ) %>% 
      map_dfr(~{
        
        new_text <- .x$schwabtext[1]
        
        for (jj in seq_along(1:nrow(.x))) {
          new_text <- str_replace_all(new_text, .x$schwablinks[jj], .x$links[jj])
          
        }
        
        .x$schwabtext <- new_text
        
        return(.x)
      }) %>% 
      filter(str_detect(schwabtext, "hddb[^[:space:]]*", negate = T))
    
    if(nrow(updated_links_dat)==0) updated_links_dat <- .data    
  } else {
    updated_links_dat <- .data  
  }
  
  return(updated_links_dat)
}

replace_mentions <- function(.data, status = id) {
  
    mentions_dat <- .data %>% 
      mutate(mentions = stringr::str_extract_all(text, "@[^[:space:]]*")) %>% 
      unnest(mentions) 
    
    print("mentions_dat")
    print(mentions_dat)
    
    if(nrow(mentions_dat)!=0){    
      schwab_mentions_dat <- .data %>%  
        mutate(schwab_mentions = stringr::str_extract_all(schwabtext, "@[^[:space:]]*")) %>% 
        unnest(schwab_mentions) %>% 
        select(schwab_mentions)
      
      updated_mentions_dat <- mentions_dat %>% 
        bind_cols(schwab_mentions_dat) %>% 
        rowwise() %>% 
        mutate(schwab_mentions = rex::rex(schwab_mentions)) %>% 
        ungroup() %>% 
        group_split( {{status}} ) %>% 
        map_dfr(~{
        
        new_text <- .x$schwabtext[1]
        
        for (jj in seq_along(1:nrow(.x))) {
          new_text <- str_replace_all(new_text, .x$schwab_mentions[jj], .x$mentions[jj])
          
        }
        
        .x$schwabtext <- new_text
        
        return(.x)
      }) 
    
      if(nrow(updated_mentions_dat)==0) updated_mentions_dat <- .data
    } else {
      
      updated_mentions_dat <- .data  
  }
    
  return(updated_mentions_dat)
}


print_data <- function(x) {
  print(x)
  res <<- x
  return(x)
}


get_tweet_screenshots <- function(id, s = "#image_result > div:nth-child(2) > div:nth-child(1) > img", d = 10, r = F, zoom_in = 2){
  
  if(r){
    sels <- c(
      "#image_result > div:nth-child(1) > div:nth-child(1) > img",
      "#image_result > div:nth-child(1) > div:nth-child(2) > img",
      "#image_result > div:nth-child(2) > div:nth-child(1) > img",
      "#image_result > div:nth-child(2) > div:nth-child(2) > img",
      "#image_result > div:nth-child(3) > div:nth-child(1) > img",
      "#image_result > div:nth-child(3) > div:nth-child(2) > img",
      "#image_result > div:nth-child(4) > div:nth-child(1) > img",
      "#image_result > div:nth-child(4) > div:nth-child(2) > img"
      )
    
    s <- sample(sels, 1)
  }
  
  webshot(paste0("https://www.bannerbear.com/demos/tweetagram/?tweet_id=", id),
                    selector = s, 
                    file = paste0("img/", id, ".png"),
                    zoom = zoom_in, delay = d) 
}

get_tweet_screenshots <- purrr::possibly(get_tweet_screenshots, otherwise = NULL, quiet = F)





# get_tweet_screenshots(1423889471808802816, zoom_in = 2)


