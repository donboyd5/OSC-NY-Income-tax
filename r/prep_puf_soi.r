
# notes ----
# SOI
# https://www.irs.gov/statistics/soi-tax-stats-individual-statistical-tables-by-size-of-adjusted-gross-income


# Peter Metz's work:
# https://github.com/Peter-Metz/state_taxdata/blob/master/state_taxdata/prepdata.py


# my taxdata repo:
# C:\Users\donbo\Documents\GitHub\taxdata


# taxdata extrapolation ----

# puf_stage3.md

# For the targeted variable, Stage III uses information from 
# [SOI Tax Stats](https://www.irs.gov/uac/soi-tax-stats-individual-statistical-tables-by-size-of-adjusted-gross-income)
# tables to determine what percent of its total is in each AGI bin for
# 2009-2014 (see appendix for the bin breakdown). The distribution is
# assumed to hold at 2014 levels for the years 2015-2026. These are the
# goal distributions.
# 
# For each year adjustment factors are needed, the targeted variable is
# extrapolated to that year using the same routine as in
# Tax-Calculator. The goal bin amounts are then found using the goal
# distributions and aggregate total found in the PUF.
# 
# The goal bin amounts are divided by the actual bin amounts, which are
# calculated using the AGI variable found in the PUF before final
# processing, in order to find a set of adjustment factors that can be
# multiplied by each record in each AGI bin so that bin totals reach
# their targeted levels.
# 
# While this process does benefit from its simplicity, there are some
# trade-offs:
#   
#   * Because AGI bin is determined using 2009 AGI levels, it does not
# account for any change in AGI overtime that could result in a tax unit
# moving into a different AGI bin. Thus, the final distribution will not
# be identical to what is found in IRS tax data.
# 
# * Because the factor is only being applied to one element of income,
# any possible relationship between two types of income will be
# lost. However, in the case of interest income, there do not appear to
# be any strong correlations with other income items (see appendix).


# code folding ----
# alt-o, shift-alt-o
# alt-l, shift-alt-l
# alt-r

# libraries ----
library(tidyverse)
library(btools)
library(readxl)
library(janitor)
library(knitr)
library(microweight)
devtools::session_info()

# globals ----

dyear <- 2017

pufname <- paste0("puf_", dyear, "_djb.csv")

# get ht2 and soi ----
ht2 <- readRDS(here::here("data", "soi", "ht2.rds")) %>%
  filter(year==dyear)

soi <- readRDS(here::here("data", "soi/spreadsheets", "soi1.1.rds"))

ht2
soi

# get puf ----

puf <- read_csv(here::here("data", "puf", pufname))
glimpse(puf)
sum(puf$s006)
ns(puf)


# stubs ----
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
HT2_AGI_STUBS <- c(-Inf, 1.0, 10e3, 25e3, 50e3, 75e3, 100e3, 200e3, 500e3, 1e6, Inf)
xwalk <- tibble(agi_stub = 1:length(HT2_AGI_STUBS),
                agi_cut = cut(HT2_AGI_STUBS, HT2_AGI_STUBS, right = FALSE)) %>%
  filter(!is.na(agi_cut))
xwalk

puf2 <- puf %>%
  # filter out filers imputed from CPS
  filter(data_source == 1) %>%
  mutate(agi_cut = cut(c00100, HT2_AGI_STUBS, right = FALSE),
         agi_stub=as.integer(agi_cut))
glimpse(puf2)
(levlabs <- count(puf2, agi_stub, agi_cut))


# how do variables match up by income range?? ----
nret_soi <- soi %>%
  filter(agi_stub != 0) %>%
  left_join(xwalk)

nret_ht2 <- ht2 %>%
  filter(stabbr == "US", agi_stub != 0) %>%
  select(agi_stub, nret = n1) %>%
  mutate(agi_cut=factor(agi_stub, levels=levlabs$agi_stub, labels=levlabs$agi_cut))

# %>%   adorn_totals()

nret_puf <- puf2 %>%
  group_by(agi_stub, agi_cut) %>%
  summarise(nret = sum(s006), .groups = "drop")

comp <- nret_soi %>%
  left_join(nret_puf, by=c("agi_stub", "agi_cut"), suffix = c("_soi", "_puf")) %>%
  left_join(nret_ht2 %>% rename(nret_ht2=nret), by=c("agi_stub", "agi_cut")) %>%
  select(agi_stub, agi_cut, nret_soi, nret_ht2, nret_puf) %>%
  adorn_totals() %>%
  mutate(dpuf_soi=nret_puf - nret_soi,
         ppuf_soi=dpuf_soi / nret_soi * 100,
         dpuf_ht2=nret_puf - nret_ht2,
         ppuf_ht2=dpuf_ht2 / nret_ht2 * 100)
comp

comp %>%
  kable(digits = c(rep(0, 6), 1, 0, 1), format.args = list(big.mark = ','))

comp %>%
  select(-contains("ht2")) %>%
  kable(digits = c(rep(0, 5), 1), format.args = list(big.mark = ','))



