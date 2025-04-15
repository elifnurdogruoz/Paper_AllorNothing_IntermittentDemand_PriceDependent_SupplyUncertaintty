
library(openxlsx)
library(ggplot2)
library(lattice)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(r2symbols)


rm(list=ls())

#wd = "C:/Users/ELIFNURD/Desktop/RCode"

wd="/Users/elifnurdogruoz/Desktop/Tez Dosyaları/Makale/SinglePeriod_Unresponsive_Beta"

setwd(wd)

library(readxl)
# Reading the Excel file using readxl package
df <- read_excel("Unresponsive_SinglePeriod_ProfitFcn_Beta_h002.xlsx", sheet = 1)
                                                                                                                
colnames(df) <- c("alfa","y","p","G")
custom_labels <- c(
  "1" = "alpha = beta = 1",
  "2" = "alpha = beta = 2",
  "3" = "alpha = beta = 3",
  "4" = "alpha = beta = 4",
  "5" = "alpha = beta = 5",
  "6" = "alpha = beta = 6"
)


 Create a ggplot object with facet_wrap and custom labels
gg <- ggplot(df, aes(x = p, y = G, color = factor(y), linetype = factor(alfa))) +
  geom_line() +
  labs(
    x = 'p',
    y = 'G(x,p)',
    color = 'x',
    linetype = 'alpha & beta'
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    legend.title = element_text(size = 10),  # Optional: Adjust legend title size
    legend.text = element_text(size = 8)    # Optional: Adjust legend text size
  ) +
  facet_wrap(~alfa, ncol = 2, scales = "free_y", labeller = as_labeller(custom_labels)) +
  guides(linetype = "none")  # Removes the legend for 'linetype'

# Save the ggplot as a PNG file
ggsave("Unresponsive_SinglePeriod_ProfitFcn_Beta_v4.png", gg, width = 8, height = 6, units = "in")
