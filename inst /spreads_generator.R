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

## Data settings ----

corrfilt <- T
corrfilt.metric <- 0.9
rolling_window <- 5
#tickers <- c("KO" , "SB=F" , "PEP" , "NSRGY" , "UNH")
start_date <- "2023-06-01"


## Load data via quantmod ----

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

tickers.norm <- paste0(tickers, ".norm")

dt[ , (tickers.norm) :=
      lapply(.SD , function(x){ ( x - mean(x)) / sd(x)}) 
    , .SDcols = tickers]

## Correlation Matrix ----
cor.matrix <- cor(dt[ , .SD , .SDcols = tickers]) 
# Filtering

if(corrfilt == T){
  
cor.matrix.filter <- which(abs(cor.matrix) > corrfilt.metric
                           , arr.ind = T)
cor.matrix.filter <- data.table(
  r1 = rownames(cor.matrix)[cor.matrix.filter[,1]] %>% paste0(. , ".norm"), 
  r2 = rownames(cor.matrix)[cor.matrix.filter[,2]] %>% paste0(. , ".norm")                          
)


cor.matrix.filter <- paste0(cor.matrix.filter$r1,
                            "sprd" ,
                            cor.matrix.filter$r2
                            )

}
## Spreads Generation ----

new_feats <- outer(tickers.norm , tickers.norm, FUN=paste)
new_feats <- new_feats[lower.tri(new_feats , diag = F)] %>% 
                        stringr::str_replace(. , " " , "sprd")


if(corrfilt == T){

  new_feats <- cor.matrix.filter[cor.matrix.filter %in% new_feats]
   
}

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


new_feats.rolling <- paste0(new_feats , ".r" ,  rolling_window)


dt[ , (new_feats.rolling) := frollmean(.SD ,  rolling_window) 
    , .SDcols = new_feats]

dt <- dt %>% na.omit()

spreads.plot <- list()


spreads.plot <- lapply(new_feats.rolling, function(tick){
  
  # Augmenting Title 
  p.title <- tick %>% 
    stringr::str_remove_all(. , ".norm" ) %>% 
    stringr::str_replace(. , "sprd" , " - ")
  
  # Generating Plot
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
    ylab(paste0(p.title)) + 
    ggtitle(paste(p.title))
})

names(spreads.plot) <- new_feats.rolling

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
names(plot.list) <- tickers

## Render HTML -----

rmarkdown::render(input = "Markdown_files/spreads.Rmd", 
                  output_file = "Spreads_assessment",
                  output_dir = "/Users/j9m3/Documents/Code/R/Output_HTMLs"
                  )
