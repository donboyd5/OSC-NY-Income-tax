

# code folding ----
# alt-o, shift-alt-o
# alt-l, shift-alt-l
# alt-r

# libraries ----
library(tidyverse)
library(readxl)
library(btools)
library(microweight)
devtools::session_info()


# globals ----
# datdir <- "D:/Data/"
#migd <- "D:/Data/SOI Migration/CountyData/"


# get HT2 ----
# 0 = No AGI Stub
# 1 = ‘Under $1’
# 2 = '$1 under $10,000'
# 3 = '$10,000 under $25,000'
# 4 = '$25,000 under $50,000'
# 5 = '$50,000 under $75,000'
# 6 = '$75,000 under $100,000'
# 7 = '$100,000 under $200,000'
# 8 = ‘$200,000 under $500,000’
# 9 = ‘$500,000 under $1,000,000’
# 10 = ‘$1,000,000 or more’

# https://www.irs.gov/pub/irs-soi/17in54cmcsv.csv
# https://www.irs.gov/pub/irs-soi/16in54cmcsv.csv
# 
ubase <- "https://www.irs.gov/pub/irs-soi/"
for(y in 2012:2017){
  if(y==2013) suffix <- "xlsx" else suffix <- "csv"
  fn <- paste0(str_sub(y, 3, 4), "in54cmcsv.", suffix)
  url <- paste0(ubase, fn)
  download.file(url, here::here("data", "soi", fn), mode="wb")
}

# create a csv version of the 2013 file
df <- read_excel(here::here("data", "soi", "13in54cmcsv.xlsx"))
glimpse(df)
names(df)
# "State"    "agi_stub" "N1"
df %>%
  write_csv(here::here("data", "soi", "13in54cmcsv.csv"))

# now get the time series
f <- function(y) {
  print(y)
  fn <- paste0(str_sub(y, 3, 4), "in54cmcsv.csv")
  df <- read_csv(here::here("data", "soi", fn), col_types=cols(.default = col_character()))
  df %>%
    setNames(str_to_lower(names(.))) %>%
    mutate(year=y) %>%
    rename(stabbr=state) %>%
    select(year, everything())
}
tmp <- map_df(2012:2017, f)
glimpse(tmp)
count(tmp, agi_stub)

# which variables should be character? only stabbr
cvars <- c("stabbr")
glimpse(tmp %>% filter(year==2016))

ht2_all <- tmp %>%
  mutate(across(-c(stabbr, year, agi_stub), parse_number),
         agi_stub=as.integer(agi_stub))
glimpse(ht2_all)
saveRDS(ht2_all, here::here("data", "soi", "ht2.rds"))



