## Load packages ----

pacman::p_load(magrittr ,
               ggplot2 ,
               lubridate ,
               stringr ,
               quantmod ,
               data.table ,
               DescTools ,
               purrr , 
               corrplot)
## Load data via quantmod ----

tickers <- c("KO" , "SB=F" , "PEP" , "NSRGY" , "UNH")
start_date <- "2016-01-01"

quantmod::getSymbols(tickers , from = start_date) %>%
  as.data.table()

prices <- map(tickers,function(x) Ad(get(x)))
prices <- reduce(prices,merge)
colnames(prices) <- tickers



## Create data.table ----

dt <- prices %>% as.data.table()
rm(list = tickers )
rm(prices)

dt <- copy(dt %>% na.omit())

## Clean Names ----
data.table::setnames(dt , "index" , "Date"  )

new_tickers <- stringr::str_replace(tickers , "=" , "")

data.table::setnames(dt , tickers , new_tickers )

tickers <- new_tickers
rm(new_tickers)
## Normalize ----

dt[ , (tickers) :=
      lapply(.SD , function(x){ ( x - mean(x)) / sd(x)}) 
    , .SDcols = tickers]

## Correlation Matrix ----
cor.matrix <- cor(dt[ , .SD , .SDcols = tickers]) 

## Spreads Generation ----

new_feats <- outer(tickers , tickers, FUN=paste)
new_feats <- new_feats[lower.tri(new_feats , diag = F)] %>% 
                        stringr::str_replace(. , " " , "sprd")



spreads.list <- map(new_feats , function(x){
            feats2split <- 
              stringr::str_split(x , "sprd" , simplify = T) %>% 
              as.character()
            
            left    <- feats2split[1]
            right   <- feats2split[2]
            
            vec_out <- dt[[left]] - dt[[right]]
            
            vec_out %>%  return()
})


names(spreads.list) <- new_feats

spreads.dt <- spreads.list %>% as.data.table()
rm(spreads.list)

dt <- cbind(dt , spreads.dt)


new_feats.r22 <- paste0(new_feats , ".r22")


dt[ , (new_feats.r22) := frollmean(.SD ,  22) 
    , .SDcols = new_feats]

dt <- dt %>% na.omit()

spreads.r22.plot <- list()


spreads.r22.plot <- lapply(new_feats.r22, function(tick){
  ggplot(data = dt , 
         aes(x = Date,
             y = dt[[tick]])
  ) + 
    geom_line() + 
    geom_line(aes(y = mean(dt[[tick]])) ,
              color = " red" ,
              alpha = 0.8 ,
              linetype = "dashed"
              ) +
    ylab(paste0(tick)) + 
    ggtitle(paste(tick))
})


## Price Plots ----
plot.list <- list()

plot.list <- lapply(tickers , 
                    function(tick){
                      ggplot(data = dt , 
                             aes(x = Date)
                      ) + 
                        geom_line(aes(y = dt[[tick]])) + 
                        ylab(paste0(tick)) + 
                        ggtitle(paste(tick))
                      
                    })


## Render HTML -----

rmarkdown::render( input = "Markdown_files/spreads.Rmd")

