
library(openxlsx)
library(ggplot2)
library(lattice)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(r2symbols)
library(readxl)

rm(list=ls())

#wd = "C:/Users/ELIFNURD/Desktop/RCode"

wd="/Users/elifnurdogruoz/Desktop/Tez Dosyaları/Makale/TwoPeriod_Unresponsive"

setwd(wd)

df1 <- read_xlsx("Unresponsive_TwoPeriod_newDRV1_20241019_035352.xlsx", sheet="Sheet1",col_names = FALSE )
df1 <- cbind(rep("DRV1",times=nrow(df1)),df1)
colnames(df1)<-c("DRV","x","J2","p*", "G","f1")
df2 <- read_xlsx("Unresponsive_TwoPeriod_newDRV2_20241019_163825.xlsx", sheet="Sheet1", col_names = FALSE)
df2 <- cbind(rep("DRV2",times=nrow(df2)),df2)
colnames(df2)<-c("DRV","x","J2","p*", "G","f1")
df3 <- read_xlsx("Unresponsive_TwoPeriod_newDRV3_20241019_162827.xlsx", sheet="Sheet1", col_names = FALSE)
df3 <- cbind(rep("DRV3",times=nrow(df3)),df3)
colnames(df3)<-c("DRV","x","J2","p*", "G","f1")
#df4 <- read_xlsx("Unresponsive_TwoPeriod_newDRV4_TrPois_20241020_234015.xlsx", sheet="Sheet1", col_names = FALSE)
#df4 <- cbind(rep("DRV4",times=nrow(df4)),df4)
#colnames(df4)<-c("DRV","x","J2","p*", "G","f1")

#df <-rbind(df1,df2,df3,df4)
#Without DRV4
df <-rbind(df1,df2,df3)
colnames(df) <-c("DRV","x","J2","p*", "G","f1")



gg <- ggplot(df, aes(x = x, y = J2, linetype = factor(DRV))) +
  geom_line() +
  labs(
    x = 'x',
    y = 'J2(x)',
    linetype = 'DRV'
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14) 
  ) +
  #scale_x_continuous(limits = c(0, 100)) +  # Example x-axis limits
  #scale_y_continuous(limits = c(0, 200)) + # Example y-axis limits
  facet_wrap(~ DRV, ncol = 3, scales = "free") 


# Save the ggplot as a PNG file
ggsave("Unresponsive_TwoPeriod_DiscreteRV_21102024_woDRV4_v4.png", gg, width = 10, height = 4, units = "in")