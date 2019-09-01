setwd("C:/Users/Tay/Downloads/NUS/ST/ST2137/Tutorials/Tutorial 2/Data set")

###################################### Q1 ######################################
varnames = c("id", "gender", "height", "weight", "siblings")
htwt2 <- read.fwf("tut2htwtfixed.txt", width = c(3,1,3,2,1), header = F, col.names = varnames)

###################################### Q2 ######################################

#important to attach the data to make the variables accessible by name. Without this step,
#it would not make sense to call 'gender' in the next command
attach(htwt2)
htwt2m <- htwt2[gender == "M",]
dim(htwt2m)

#There are 48 males in htwt2.

###################################### Q3 ######################################

tut2test <- read.csv("tut2test.csv", header = T)
htwttest2 <- merge(htwt2, tut2test, by = "id")
attach(htwttest2)
higherThan182 <- htwttest2[height > 182,]

# id gender height weight siblings test
# 67 267      M    183     85        0   55
# 71 271      M    188     60        2   76
# 85 285      M    184     67        3   54

#The testscores of subjects whose height is greater than 182cm: 55, 76, 54.

###################################### Q4 ######################################
htwttest2remo <- htwttest2[id != 210,]

###################################### Q5 ######################################
htwttest2remo <- htwttest2[id != 210,]
htwttest2[210 - htwttest2[1,1] + 1, 4] <- 68

###################################### Q6 ######################################
htwttest2female <- htwttest2[gender == "F",]
attach(htwttest2female)
htwttest2female[rev(order(height)),]
htwttest2female[rev(order(height)),][2, c("height", "weight", "test")]

#Second tallest female's height, weight and test score:
#height weight test
#73    174     64   57

###################################### Q7 ######################################
htwttest2["grade"] <- ""
attach(htwttest2)
for (i in 1:nrow(htwttest2)){
  score = htwttest2[i,"test"]
  if (score >= 80){
    htwttest2[i, "grade"] = "A"
  }
  else if (score >= 70){
    htwttest2[i, "grade"] = "B"
  }
  else if (score >= 60){
    htwttest2[i, "grade"] = "C"
  }
  else if (score >= 50){
    htwttest2[i, "grade"] = "D"
  }
  else htwttest2[i, "grade"] = "F"
}

###################################### Q8 ######################################
X <- cbind(c(1,1,1,1,1), c(1,3,4,7,11))
y <- c(4,6,13,15,19)
beta <- solve(t(X) %*% X) %*% t(X) %*% y

###################################### Q9 ######################################
x_n_2 = 0
x_n_1 = 2
sum = x_n_2 + x_n_1

for (i in 3:18){
  x_n = x_n_1 - 2*x_n_2
  
  if (i == 18){
    cat("x_18 =", x_n)
  }
  if (i<=15){
    sum = sum + x_n
  }
    
  x_n_2 = x_n_1
  x_n_1 = x_n
}
cat("The sum of the first 15 terms in this sequence is", sum)

###################################### Q10 ######################################

mean <- function(x){
  n <- length(x)
  return(sum(x) / n)
}
rth_moment <- function(r, x){
  n <- length(x)
  moment <- sum((x - mean(x)) ^ r) / n
  return(moment)
}
calc_moments <- function(x){
  stats <- c(mean(x), rth_moment(2,x), rth_moment(3,x), rth_moment(4,x))
  return(stats)
}

cat(calc_moments(htwt2$height))
