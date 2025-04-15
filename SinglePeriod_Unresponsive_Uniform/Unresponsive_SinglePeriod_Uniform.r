
library(discreteRV)
library(openxlsx)
library(cubature)
library(xlsx)
library(ggplot2)

rm(list = ls())

#wd = "C:/Users/ELIFNURD/OneDrive - flypgs/Masaüstü/Rcode"

#wd="/Users/elifnurdogruoz/Desktop"
wd="/Users/elifnurdogruoz/Desktop/Tez Dosyaları/Makale/SinglePeriod_Unresponsive_Uniform"
setwd(wd)

h <- 0.02 #holding cost
#pi <- 1   #backorder cost
a <- 1 
b1 <- 1 
b2 <- 1
lm <- 1 #exponential dist. parameter
Q <- 2 #order amount
c <- 0 #variable cost
K <- 0.1 #fixed cost


#z_vals <- c(1, 2, 3)
#z_probs <- c(0.3, 0.4, 0.3)


#Define random variable with the package discreteRV\n
#z_vals <- c(1,2,4,5) #DRV1
#z_probs <- c(0.1,0.3, 0.5,0.1)
#z_vals <- c(1, 2, 3) #DRV2
#z_probs <- c(0.3, 0.4, 0.3)
#z_vals <- c(2,3,10) #DRV3
#z_probs <- c(0.1, 0.1, 0.8)
#z_vals <- c(1,2,3,5,7,10,11) #DRV4
#z_probs <- c(0.05,0.1, 0.2,0.25,0.25,0.1,0.05)
#z <- RV(z_vals, z_probs)


#Get expectation and variance of z\n
#E(z)
#V(z)

r_mean <- 0.5

A <- function(r) {
	r
}

g <- function(p,r) {
	#ar <-A(r)
	1/(1+(b1*p+b2*p^2)*r)
}


Eg <- function(p) {
	#ar <-A(r)
	#log (1+(b1*p+b2*p^2))/(b1*p+b2*p^2)
	log(1+(b1*p+b2*(p^2)))/(b1*p+(b2*p^2))
}


G <- function(x, p) {
	(x - L(x)) * Eg(p) * (p + h) - h*x
}


L <- function(x) {
(exp(-lm*x)+lm*x-1)/lm
}


J1 <- function(x) {
	#search over p's to maximize
	maxp <- 0.01
	for (i in p_list) {
		if (G(x,i) > G(x,maxp) ) {
			maxp <- i
		}
	}
	c(G(x,maxp),maxp)
}




p_list <- seq(0.000001, 10.000001, 0.05)
x_max <- 10
x_list <- seq(0, x_max+Q, 1)
r_list <-seq(0.1, 1, 0.1)




Gs <- matrix(0, nrow=length(x_list)*length(p_list), ncol=3)
Gs [,1] <- rep(p_list, each=length(x_list))
Gs [,2] <- rep(x_list, times=length(p_list))

for (i in 1:nrow(Gs)){
	p <- Gs[i,1]
	print(paste("p:", Gs[i,1], " x:",Gs[i,2]))
	Gs[i,3] <- G(Gs[i,2],Gs[i,1])
}

df <- data.frame(Gs)
colnames(df) <- c("p","x","G")

# Save G data to Excel
write.xlsx(df, file = "Unresponsive_SinglePeriod_G_Data.xlsx", row.names = FALSE)


gg <- ggplot(df, aes(x = p, y = G, color = factor(x))) +
  geom_line() +
  labs(
    x = 'p',
    y = 'G(x,p)',
    color =  'x'
  ) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))

# Save the ggplot as a PNG file
ggsave("Unresponsive_SinglePeriod_ProfitFcn_Uniform_v2.png", gg, width = 8, height = 6, units = "in")


gg <- ggplot(df, aes(x = p, y = G, color = factor(x))) +
  geom_line() +
  labs(
    x = "p",
    y = "G(x, p)",
    color = "x"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
ggsave("Unresponsive_SinglePeriod_ProfitFcn_Uniform_v4.png", gg, width = 8, height = 6, units = "in")


J1s <- matrix(0, nrow=length(x_list), ncol=3)
J1s [,1] <- x_list

for (i in 1:nrow(J1s)){
	print(paste(" x:",J1s[i,1]))
	temp <- J1(J1s[i,1])
	J1s[i,2] = temp[1]
	J1s[i,3] = temp[2]
}
J1s



df2 <- data.frame(J1s)
colnames(df2) <- c("x","G","p")  # This should be df2, not df

# Save J1 data to Excel
write.xlsx(df2, file = "Unresponsive_SinglePeriod_J1_Data.xlsx", row.names = FALSE)


df2_filtered <- df2[df2$x <= 10, ]

gg2 <- ggplot(df2_filtered , aes(x = x, y = G)) +  # Plot y on x-axis and G on y-axis
  geom_line(color = "black") +  # Use black for the line color, adjust if necessary
  labs(
    x = 'x',  # Label for x-axis
    y = 'G(x,p)'  # Label for y-axis
  ) +
  theme_minimal() +  # Apply a minimal theme
  theme(plot.background = element_rect(fill = "white"))
  
  
 gg2 <- ggplot(df2_filtered, aes(x = x, y = G)) +
  geom_line(color = "black") +
  labs(
    x = "x",
    y = "G(x, p)"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
  
ggsave("Unresponsive_SinglePeriod_ProfitFcn_Uniform_OptimalPrice_v4.png", gg2, width = 8, height = 6, units = "in")
