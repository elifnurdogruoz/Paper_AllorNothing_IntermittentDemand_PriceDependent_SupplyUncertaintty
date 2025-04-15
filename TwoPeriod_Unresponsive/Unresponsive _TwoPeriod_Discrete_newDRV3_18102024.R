
library(discreteRV)
library(openxlsx)
library(cubature)
library(xlsx)

rm(list = ls())

#wd = "C:/Users/ELIFNURD/OneDrive - flypgs/Masaüstü/Rcode" #Windows

#wd="/Users/elifnurdogruoz/Desktop"
wd="/Users/elifnurdogruoz/Desktop/Tez Dosyaları/Supply Uncertainty/Single Period/Makale/TwoPeriod_Unresponsive" #Mac
setwd(wd)

h <- 0.02 #holding cost
#pi <- 1   #backorder cost
#a <- 1 
b1 <- 1 
b2 <- 1
#lm <- 0.5 #exponential dist. parameter
Q <- 2 #order amount
c <- 0 #variable cost
K <- 0.1 #fixed cost


#z_vals <- c(1, 2, 3)
#z_probs <- c(0.3, 0.4, 0.3)


Define random variable with the package discreteRV\n
#z_vals <- c(3,4,5) #DRV1
#z_probs <- c(0.1,0.8, 0.1)#
#z_vals <- c(2,3,4,5,6) #DRV2
#z_probs <- c(0.1,0.1,0.6,0.1,0.1)
z_vals <- c(1,2,3,4,5,6,7) #DRV3
z_probs <- c(0.1,0.1,0.1,0.4,0.1,0.1,0.1)
z <- RV(z_vals, z_probs)

#Get expectation and variance of z\n
E(z)
V(z)

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
	log(1+(b1*p+b2*p^2))/(b1*p+b2*p^2)
}

G_old <- function(x, p) {
  val <- 0
  for (i in 1:length(outcomes(z))) {
    if (any(outcomes(z)[i] <= x)) {
      val <- val + probs(z)[i] * outcomes(z)[i]
    } else {
      val <- val + probs(z)[i] * x
    }
  }
  p * Eg(p) * val - Eg(p) * h * L(x) - (h * (1 - Eg(p)) * x)
}


G <- function(x, p, r) {
  val <- 0
  for (i in 1:length(outcomes(z))) {
    if (any(outcomes(z)[i] <= x)) {
      val <- val + probs(z)[i] * outcomes(z)[i]
    } else {
      val <- val + probs(z)[i] * x
    }
  }
  p * g(p, r) * val - g(p, r) * h * L(x) - (h * (1 - g(p, r)) * x)
}




L <- function(x) {
  val <- 0
  for (i in 1:length(outcomes(z))) {
    if (any(outcomes(z)[i] <= x)) {
      val <- val + probs(z)[i] * (x - outcomes(z)[i])
    }
  }
  val
}


#####Single period#####


J1 <- function(x, r) {
  # search over p's to maximize
  maxp_index <- which.max(sapply(p_list, function(i) G(x, i, r)))
  maxp <- p_list[maxp_index]
  c(G(x, maxp, r), maxp)
}


f1 <- function(x,r) {
	max(J1(x,r)[1], J1(x+r*Q,r)[1]- K -c*r*Q)
}


#Calculation of E[f1(x-z): i<=x]
f1_xz<- function(x, r) {
  val <- 0
  for (i in 1:length(outcomes(z))) {
    if (any(outcomes(z)[i] <= x)) {
      val <- val + probs(z)[i] * f1(x - outcomes(z)[i], r)
      #print(paste("ilk if"," val:",val ))
    }
    if (any(outcomes(z)[i] > x)) {
      val <- val + probs(z)[i] * f1(0, r)
      #print(paste("ikinci if","val:",val ))
    }
  }
  val
}



### Two Period ######

# Define the function f with parameters p2, r2, and x
f <- function(r1, p2, r2, x) {
  (1 - g(p2, r2)) * f1(x, r1) + g(p2, r2) * f1_xz(x, r1)
}

# Vectorize the function to accept vectors of parameters
f_vec <- Vectorize(f, vectorize.args = c("r1", "p2", "r2", "x"))


fp1 <- function(r1, x) {
	f1(x, r1)
}

fp2 <- function(r1, x) {
	f1_xz(x, r1)
}
fp1_vec <- Vectorize(fp1, vectorize.args = c("r1", "x"))
fp2_vec <- Vectorize(fp2, vectorize.args = c("r1", "x"))

x_max <- 13
x_list <- seq(13, x_max+Q, 1)
p_list <- seq(0.001, 4.001, 0.05) ##0.1 aralıklı 10'a kadar kullandık, burada test amaçlı 4'e düşüreceğim

#resp1 <-integrate(fp1_vec, lower = 0, upper = 1,  x = x)$value
#resp2 <-integrate(fp2_vec, lower = 0, upper = 1,  x = x)$value

gp2 <- function(r2, p2) {
	1- g(p2, r2) *resp1 + g(p2, r2) * resp2
}
gp2_vec <- Vectorize(gp2, vectorize.args = c("r2", "p2"))

#x=1
#p2=0.1
#finalres <- integrate(gp2_vec, lower = 0, upper = 1,  p2=p2)$value



#p_list <- seq(0, 4, 0.05)

# Define the parameters
#p2 <- 5
#r2 <- 0.9
#x <- 18

# Use integrate function to integrate over r1
#result <- integrate(f_vec, lower = 0, upper = 1, p2 = p2, r2 = r2, x = x)

# Access the result
#result$value

J2 <- function(x,r2) {
	#search over p's to maximize

	val <- -10
	max_p2<- 0.01
	#max_p1<- 0.5

			for (i in p_list) {
				#print(paste ("i:",i, " x:",x))
				p2 <- i 
				resp1 <-integrate(fp1_vec, lower = 0, upper = 1,  x = x)$value				
				#print(paste ("resp1:",resp1))
				resp2 <-integrate(fp2_vec, lower = 0, upper = 1,  x = x)$value
				#print(paste ("resp2:",resp2))
				gp2 <- function(r2, p2) {
						1- g(p2, r2) *resp1 + g(p2, r2) * resp2
					}
				gp2_vec <- Vectorize(gp2, vectorize.args = c("r2", "p2"))
				result <- integrate(gp2_vec, lower = 0, upper = 1,  p2=p2)$value
				#print(paste("result ", result, " with p2 ",i))
				#result <- integrate(f_vec, lower = 0, upper = 1, p2 = i, r2  = r2, x = x)
				# Access the result		
				if ( (G_old(x,i) + result) > val ){
					max_p2<- i
					val <- G_old(x,max_p2) + result
					gmax <- G_old(x,max_p2)
					resmax <- result
					#print(paste("val changed to ", val, " with p2 ",i, " with maxp2 ", max_p2))
				}
			}
			
	#print(paste("max price:",max_p))
	c(val,max_p2,gmax,resmax)
}

J2s <- matrix(0, nrow=length(x_list), ncol=5)
J2s[,1] <- x_list

for (i in 1:nrow(J2s)){
	temp <- J2(J2s[i,1])
	print(paste("x:", J2s[i,1]," J2:",temp[1]))
	J2s[i,2] = temp[1]
	J2s[i,3] = temp[2]
	J2s[i,4] = temp[3]
	J2s[i,5] = temp[4]
}


current_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
# Define the file name with the current date and time
file_name <- paste0("Unresponsive_TwoPeriod_newDRV3_part2", current_time, ".xlsx")
write.xlsx(J2s, file_name, sheetName = "Sheet1", row.names = FALSE, col.names= FALSE)
