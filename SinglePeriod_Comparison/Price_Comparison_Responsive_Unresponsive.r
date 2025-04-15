
library(discreteRV)
library(openxlsx)
library(cubature)
library(xlsx)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

rm(list = ls())

#wd = "C:/Users/ELIFNURD/OneDrive - flypgs/Masaüstü/Rcode"

#wd="/Users/elifnurdogruoz/Desktop"
wd="/Users/elifnurdogruoz/Desktop/Tez Dosyaları/Makale/SinglePeri0d_Comparison"
setwd(wd)




df <- read_xlsx("Price_Comparison_Responsive_Unresponsive_updated.xlsx", sheet="Sheet3")
colnames(df) <-c("r","h","p*_res","p*_unres")



# Filter the data for specific h values
filtered_df <- df %>% filter(h %in% c(0, 0.06, 0.1))

#filtered_df <- df %>% filter(h %in% c(0, 0.06, 0.1) & r < 0.4)

gg<- ggplot(melt(filtered_df, id.vars = c("r", "h")), aes(x = r, y = value, color = variable, shape = factor(h))) +
  geom_point(size = 3) +
  geom_line(aes(group = interaction(variable, h))) +
  labs(
    #title = "p*_res and p*_unres for each r value",
    x = "r",
    y = "p*",
    color = "p*",
    shape = "h"
  ) +
  theme_minimal() +
  theme(
    #panel.background = element_rect(fill = "white"),
    #panel.grid.major = element_line(color = "grey80"),
    #panel.grid.minor = element_line(color = "grey90"),
    plot.background = element_rect(fill = "white")
    #aspect.ratio = 1
    #legend.background = element_rect(fill = "white"),
   # legend.key = element_rect(fill = "white")
  )

gg <- ggplot(
  melt(filtered_df, id.vars = c("r", "h")), 
  aes(x = r, y = value, color = variable, shape = factor(h))
) +
  geom_point(size = 3) +
  geom_line(aes(group = interaction(variable, h))) +
  labs(
    x = "r",
    y = "p*",
    color = "p*",
    shape = "h"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    legend.title = element_text(size = 12),         # Legend title size
    legend.text = element_text(size = 10),          # Legend text size
    axis.title = element_text(size = 14),           # Axis title size
    axis.text = element_text(size = 12)             # Axis tick label size
  ) +
  scale_x_continuous(
    breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 0.25),  # Tick marks at intervals of 0.04
    labels = scales::number_format(accuracy = 0.01)  # Format x-axis labels to 2 decimal places
  )


ggsave("Comparison_SinglePeriod_Resp_Unresp_v4.png", gg, width = 10, height = 6, units = "in")
  
