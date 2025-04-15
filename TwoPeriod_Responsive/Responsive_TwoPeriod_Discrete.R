
library(discreteRV)
library(openxlsx)
library(cubature)
library(xlsx)

rm(list = ls())

#wd = "C:/Users/ELIFNURD/OneDrive - flypgs/Masaüstü/Rcode"

#wd="/Users/elifnurdogruoz/Desktop"
wd="/Users/elifnurdogruoz/Desktop/Tez Dosyaları/Supply Uncertainty/Single Period/Makale/TwoPeriod_Responsive"
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




#Define random variable with the package discreteRV\n
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

#p_list <- seq(0, 4, 0.05)

# Define the parameters
#p2 <- 5
#r2 <- 0.9
#x <- 18

# Use integrate function to integrate over r1
#result <- integrate(f_vec, lower = 0, upper = 1, p2 = p2, r2 = r2, x = x)

# Access the result
#result$value

J2 <- function(x, r2) {
	#search over p's to maximize

	val <- -10
	max_p2<- 0.01
	#max_p1<- 0.5
	
			for (i in p_list) {
				#print(paste ("i:",i))
				p2 <- i 
				result <- integrate(f_vec, lower = 0, upper = 1, p2 = i, r2  = r2, x = x)
				# Access the result		
				if ( (G(x,i,r2) + result$value ) > val ){
					max_p2<- i
					val <- G(x,max_p2,r2) + result$value
					gmax <- G(x,max_p2,r2)
					resmax <- result$value
					#print(paste("val changed to ", val, " with p2 ",i, "and p1 ", j))
				}
			}
			
	#print(paste("max price:",max_p))
	c(val,max_p2,gmax,resmax)
}




f2_fast <- function(x,r2) {
	dec <- c()
	 a<-J2(x,r2)
	 b<-J2(x + r2*Q,r2)
	
	if (a[1] >= b[1]- K -c*r2*Q) {
		dec <- c("don't order", a[1],a[2],a[3],a[4],b[1],b[2],b[3], b[4], a[1],a[2],a[3],a[4],b[1]-a[1] )
	} else {
		dec <- c("order",  a[1],a[2],a[3], a[4],b[1],b[2], b[3],b[4], b[1],b[2], b[3],b[4], b[1]-a[1])
	}
	dec
	#max(J2(x),J2(x+Q))\n
}

x_max <- 10
x_list <- seq(11, x_max+Q, 1)
#p_list <- seq(0, 10, 0.5)
p_list <- seq(0, 6, 0.2)
r_list <- c(0.5)
#r2 <- 0.9
f2ss <- c()

for (r2 in r_list){
for (j in x_list) {
    #print(paste("r:", r, " x:", j))
    temp <- f2_fast(j, r2)  
    print(paste("r2:", r2, " x:", j, "dec:", temp[1]))
    f2ss <- rbind(f2ss, c(r2, j, temp))
}
}

current_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
# Define the file name with the current date and time
file_name <- paste0("Responsive_TwoPeriod_f2_newDRV3 & Q=2_part1_2_", current_time, ".xlsx")
write.xlsx(f2ss, file_name, sheetName = "Sheet1", row.names = FALSE, col.names= FALSE)
##### BURAYA KADAR RUN ALDIK##########




r2 <- 0.5
f2ss5 <- c()

for (j in x_list) {
    #print(paste("r:", r, " x:", j))
    temp <- f2_fast(j, r2)  
    print(paste("r2:", r2, " x:", j, "dec:", temp[1]))
    f2ss5 <- rbind(f2ss5, c(r2, j, temp))
}



current_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
# Define the file name with the current date and time
file_name <- paste0("Deneme2_f2_DRV3_ & Q=1_", current_time, ".xlsx")
write.xlsx(f2ss5, file_name, sheetName = "Sheet1", row.names = FALSE, col.names= FALSE)

