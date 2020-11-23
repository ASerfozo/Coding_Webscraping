library(rvest)
library(data.table)

# The starting search result was "notebook"
my_url <- 'https://www.pcmag.com/search/results?query=notebook&page=1'

# Page scraper
get_one_page <- function(my_url) {
  
  t <- read_html(my_url)
  boxes <- 
    t %>% 
    html_nodes('#app .border-gray-lighter')
  
  #x<- boxes[[8]]
  
  box_dfs <- lapply(boxes, function(x){
    tlist <- list()
    
  # The title needed to be cleared as in many cases the pcmag review, where 
  # existed, got mixed with the title producing more columns  
    ttitle <- x %>% 
      html_node('.font-bold')%>%
      html_text()
    if (endsWith(ttitle, 'Review')) {
      ttile <- substring(ttitle, 1, (nchar(ttitle)-6) )
    }
    tlist[['Title']] <- ttitle
    
    tlist[['Author']] <-
      gsub('\n', ' ', trimws(  x %>% 
                                 html_nodes('.mr-3+ .font-brand')%>%
                                 html_text()
      ), fixed = T)
    
    tlist[['Summary']] <-
      x %>% 
      html_nodes('.line-clamp')%>%
      html_text()
    
    my_relative_link <- 
      x%>% 
      html_node('a')%>%
      html_attr('href')
    
    tlist[['Link']] <- paste0('https://www.pcmag.com', my_relative_link)
    
    return(tlist)
  })
  
  df <- rbindlist(box_dfs, fill = T)
  return(df)
  
}

# View(get_one_page('my_url'))

# Link creation and final function
get_data_of_pcmag <- function(keyword,number_of_page) {
  
  pages <- paste0('https://www.pcmag.com/search/results?query=',keyword,'&page=', 1:number_of_page)
  pages_df <- rbindlist(lapply(pages, get_one_page))
  return(pages_df)
}

pcmag_scraping <- get_data_of_pcmag('printer',4)

# write out to csv
write.csv(pcmag_scraping, file = 'pcmag_scraped_data.csv')
