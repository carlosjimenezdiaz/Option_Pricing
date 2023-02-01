# Loading Libraries
library(tidyquant)
library(tidyverse)
library(derivmkts)

# Variables
Opt_Type        <- "Call"
Strike_Price    <- 480
Spot_Price      <- 450
Implied_Vol     <- seq(20, 50, 5) # If you want to simulate different IV: seq(20, 50, 5)
Risk_Free_Rate  <- 2.8
Dividend_Yield  <- 1.35
Time_Expiration <- 15 # If you want to simulate the passage of time: seq(45,5,-5)

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

# What happens with GEX when Vanna Appears
Spot_Price  <- seq(400, 550, 5)
Implied_Vol <- seq(20, 50, 5) 

Gamma_DF <- NULL
Delta_DF <- NULL

for(i in Spot_Price){ # i <- 405
  df_pricing <- derivmkts::greeks(bscall(i,
                                         Strike_Price,
                                         Implied_Vol/100,
                                         Risk_Free_Rate/100,
                                         Time_Expiration/365,
                                         Dividend_Yield/100)) %>%
    as.data.frame() %>%
    rownames_to_column(var = "Description")  
  
  # Saving Gamma info
  Gamma_DF <- Gamma_DF %>%
    bind_cols(df_pricing[3,] %>% 
    t() %>%
    as.data.frame() %>%
    slice(2:length(Implied_Vol)))
  
  # Saving Delta info
  Delta_DF <- Delta_DF %>%
    bind_cols(df_pricing[2,] %>% 
                t() %>%
                as.data.frame() %>%
                slice(2:length(Implied_Vol)))
}

# Plotting
Gamma_DF %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "Spot") %>%
  dplyr::mutate(Spot = Spot_Price) %>% 
  mutate_if(is.character,as.numeric) %>%
  gather("IV", "Gamma", 2:length(Implied_Vol)) %>%
  ggplot(aes(x = Spot, y = Gamma, colour = IV)) +
    geom_line(linewidth  = 1) +
    labs(title    = "What happens to Gamma when IV increases",
         subtitle = "Calculations for a Call Option", 
         caption  = "Own calculations.",
         x = "Spot Price",
         y = "Gamma") +
    theme_minimal() + 
    geom_vline(xintercept = Strike_Price, 
               linetype   = "dotted",
               color      = "blue", 
               linewidth  = 0.7)

Delta_DF %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "Spot") %>%
  dplyr::mutate(Spot = Spot_Price) %>% 
  mutate_if(is.character,as.numeric) %>%
  gather("IV", "Delta", 2:length(Implied_Vol)) %>%
  ggplot(aes(x = Spot, y = Delta, colour = IV)) +
  geom_line(linewidth  = 1) +
  labs(title    = "What happens to Delta when IV increases",
       subtitle = "Calculations for a Call Option", 
       caption  = "Own calculations.",
       x = "Spot Price",
       y = "Delta") +
  theme_minimal() + 
  geom_vline(xintercept = Strike_Price, 
             linetype   = "dotted",
             color      = "blue", 
             linewidth  = 0.7)
