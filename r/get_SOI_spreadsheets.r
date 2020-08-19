
# notes ----
# SOI Tax Stats - Individual Income Tax Returns Publication 1304
# https://www.irs.gov/pub/irs-pdf/p1304.pdf
# https://www.irs.gov/statistics/soi-tax-stats-individual-statistical-tables-by-size-of-adjusted-gross-income

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


# HT2 stubs ----
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

# spreadsheet stubs ----

# get spreadsheets ----
# https://www.irs.gov/statistics/soi-tax-stats-individual-statistical-tables-by-size-of-adjusted-gross-income

fn <- "17in11si.xls" # si = sources of income, Table 1.1 of Publication 1304
urlbase <- "https://www.irs.gov/pub/irs-soi/"
url <- paste0(urlbase, fn)
download.file(url, here::here("data", "soi/spreadsheets", "fn"), mode = "wb")

df <- read_excel(here::here("data", "soi/spreadsheets", "fn"), 
                 range = "A10:B29",
                 col_names = c("group", "nret"))
df

df2 <- df %>%
  mutate(stub = row_number() - 1,
         agi_stub = fct_collapse(as.character(stub),
                                 `0` = "0",
                                 `1` = "1",
                                 `2` = c("2", "3"),
                                 `3` = as.character(4:6),
                                 `4` = as.character(7:9),
                                 `5` = "10",
                                 `6` = "11",
                                 `7` = "12",
                                 `8` = "13",
                                 `9` = "14",
                                 `10` = as.character(15:19)))
df2

df3 <- df2 %>%
  group_by(agi_stub) %>%
  summarise(nret = sum(nret), .groups = "drop")
df3
df4 <- df3 %>%
  mutate(agi_stub = as.integer(levels(agi_stub))) %>%
  arrange(agi_stub)

# check
df4 %>%
  group_by(agi_stub == 0) %>%
  summarise(nret = sum(nret))
saveRDS(df4, here::here("data", "soi/spreadsheets", "soi1.1.rds"))

