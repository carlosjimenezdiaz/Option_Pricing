# Loading Libraries
library(tidyquant)
library(tidyverse)
library(derivmkts)

# Variables
Opt_Type        <- "Call"
Strike_Price    <- 480
Spot_Price      <- 475
Implied_Vol     <- 30 # If you want to simulate different IV: seq(20, 50, 5)
Risk_Free_Rate  <- 2.8
Dividend_Yield  <- 1.35
Time_Expiration <- 45 # If you want to simulate the passage of time: seq(45,5,-5)

# Pricing
df_pricing <- derivmkts::greeks(bscall(Spot_Price,
                                       Strike_Price,
                                       Implied_Vol/100,
                                       Risk_Free_Rate/100,
                                       Time_Expiration/365,
                                       Dividend_Yield/100)) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Description")

# View
df_pricing
