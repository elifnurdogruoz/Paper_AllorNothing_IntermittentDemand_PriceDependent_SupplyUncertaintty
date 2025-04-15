
library(openxlsx)
library(ggplot2)
library(lattice)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(r2symbols)
library(readxl)

rm(list=ls())

#wd = "C:/Users/ELIFNURD/Desktop/RCode"

wd="/Users/elifnurdogruoz/Desktop/Tez Dosyaları/Makale/TwoPeriod_Responsive"

setwd(wd)

df1 <- read_xlsx("Responsive_TwoPeriod_f2_newDRV1 & Q=2_short.xlsx", sheet="Sheet1")
df2 <- read_xlsx("Responsive_TwoPeriod_f2_newDRV2 & Q=2_short.xlsx", sheet="Sheet1")
df3 <- read_xlsx("Responsive_TwoPeriod_f2_newDRV3 & Q=2_short.xlsx", sheet="Sheet1")
#df4 <- read_xlsx("Responsive_TwoPeriod_f2_newDRV4_TrPois & Q=2_short.xlsx", sheet="Sheet1")
#df4 <- df4 %>% filter(x <= 12)

#df <-rbind(df1,df2,df3,df4)
df <-rbind(df1,df2,df3)
colnames(df) <-c("DRV","r","x","J2","p*")



gg <- ggplot(df, aes(x = x, y = J2, color = factor(r), linetype = factor(DRV))) +
  geom_line() +
  labs(
    x = 'x',
    y = 'J2(x)',
    color = 'r',
    linetype = 'DRV'
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),  # Background color
    legend.title = element_text(size = 12),         # Legend title size
    legend.text = element_text(size = 10),          # Legend text size
    axis.title = element_text(size = 14),           # Axis title size
    axis.text = element_text(size = 12),            # Axis tick label size
    strip.text = element_text(size = 14)            # Facet title size
  ) +
  scale_x_continuous(
    breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 3),  # Set breaks at intervals of 4
    labels = scales::number_format(accuracy = 1)  # Format labels as integers
  ) +
  scale_y_continuous() +                           # Leave y-axis free as well
  facet_wrap(~ DRV, ncol = 3, scales = "free")     # Keep scales free for facets

# Save the ggplot as a PNG file
ggsave("Responsive_TwoPeriod_DiscreteRV_woDRV4_v4.png", gg, width = 10, height = 4, units = "in")