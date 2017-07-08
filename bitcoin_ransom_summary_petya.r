#install.packages("Rcpp")
#install.packages("anytime")
library(jsonlite)
library(hrbrthemes)
library(tidyverse)
library(dplyr)
library(anytime)


# the wallets address accepting ransom payments for petya ransomware from thegaurdian.com

wallets <- c(
  "1Mz7153HMuxXTuR2R1t78mGSdzaAtNbBWX"
)

# easy way to get each wallet info vs bringing in the Rbitcoin package

sprintf("https://blockchain.info/rawaddr/%s", wallets) %>%
  map(jsonlite::fromJSON) -> chains

# get the current USD conversion (tho the above has this, too)

curr_price <- jsonlite::fromJSON("https://blockchain.info/ticker")

# calculate some basic stats

tot_bc <- sum(map_dbl(chains, "total_received")) / 10e7
tot_usd <- tot_bc * curr_price$USD$last
tot_xts <- sum(map_dbl(chains, "n_tx"))

# This needs to be modified once the counters go above 100 and also needs to
# account for rate limits in the blockchain.info API

paged <- which(map_dbl(chains, "n_tx") > 50)
if (length(paged) > 0) {
  sprintf("https://blockchain.info/rawaddr/%s?offset=50", wallets[paged]) %>%
    map(jsonlite::fromJSON) -> chains2
}

# We want hourly data across all transactions

map_df(chains, "txs") %>%
  bind_rows(map_df(chains2, "txs")) %>% 
  mutate(xts = anytime::anytime(time),
         xts = as.POSIXct(format(xts, "%Y-%m-%d %H:00:00"), origin="GMT")) %>%
  count(xts) -> xdf

# Plot it

ggplot(xdf, aes(xts, y = n)) +
  geom_col() +
  scale_y_comma(limits = c(0, max(xdf$n))) +
  labs(x = "Day/Time (GMT)", y = "# Transactions",
       title = "Bitcoin Payments-per-hour summary for Petya Ransomware",
       subtitle=sprintf("%s transactions to-date; %s total bitcoin; %s USD; Chart generated at: %s EDT",
                        scales::comma(tot_xts), tot_bc, scales::dollar(tot_usd), Sys.time())) +
  theme_ipsum_rc(grid="Y")