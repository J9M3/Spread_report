## Load Packages ----
pacman::p_load(magrittr ,
               ggplot2 ,
               lubridate ,
               stringr ,
               quantmod ,
               data.table ,
               purrr , 
               rvest
               )


## Set up url

url2scrape <- "https://companiesmarketcap.com/tech/largest-tech-companies-by-market-cap/"

html.sourse <- read_html(url2scrape) 

paragraphs <- html_elements(html.sourse, "div.company-code")
tickers <- html_text(paragraphs)


tickers <- tickers[1:5]