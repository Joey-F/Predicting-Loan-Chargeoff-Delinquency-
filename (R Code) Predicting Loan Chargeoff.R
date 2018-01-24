


data <- read.csv("D:\\Google Drive\\Programming\\Projects\\Upstart - Loan Chargeoff\\(Data) Predicting Loan Chargeoff.csv")


#####
##### Exploratory Analysis #####
#####


daily.chargeoff <- vector()  # Rate of chargeoffs for each day


n <- nrow(data)  # number of borrowers observed


finalday <- apply(data, 1, min, na.rm=T)  # When a borrower commits chargeoff or exceeds days since origination


maxday <- max(data[,1])  # latest day of observation in data


survivors.left <- vector()  # number of active loans for each day 


number.chargeoffs <- vector()  # number of chargeoffs committed on any given day


for (day in 1:maxday) {
    
    survivors <- seq(1,n)[which(finalday>=day)]  # takes all the active members, whose finalday hasn't occurred yet

    chargeoffs <- seq(1,n)[which(data[,2]==day)]    # find out who charges off on this day
    
    daily.chargeoff[day] <- length(chargeoffs)/length(survivors)    # calculate chargeoff rate for that day
    
    number.chargeoffs[day] <- length(chargeoffs)    # calculate the total number of chargeoffs for that day
    
    survivors.left[day] <- length(survivors)    # calculate the number of active loans remaining for that day
    
}


###
### Plot the results
###


plot(survivors.left, type="l", main = "Number of Active Loans Over Time", ylab="Number of Active Loans", xlab="Days Since Origination",
     cex.lab=1.5, cex.main=1.5)

    
plot(daily.chargeoff, main = "Chargeoff Rate Over Time", xlab = "Days Since Origination", 
     ylab="Proportion of Active Loans that Chargeoff (per day)",
     cex.lab=1.3, cex.main=1.5)
n <- length(daily.chargeoff)   # total number of days observed (=730)
abline(v=n/5, lty=2, col="blue")



#####
##### Quantile Comparisons #####
#####


n <- length(daily.chargeoff)   # total number of days observed (=730)


# Split data evenly into five quantiles:
    
data1 <- daily.chargeoff[1:(n/5)]
data2 <- daily.chargeoff[(n/5+1):(2*n/5)]
data3 <- daily.chargeoff[(2*n/5+1):(3*n/5)]
data4 <- daily.chargeoff[(3*n/5+1):(4*n/5)]
data5 <- daily.chargeoff[(4*n/5+1):n]



# visually compare the distributions within each quantile:

plot(data1,ylim=c(0,max(daily.chargeoff)), xlab = "Days 1-146 for each quantile", ylab = "Daily Chargeoff Rate", 
     main = "Comparing Chargeoff Distribution Within Each Quantile", 
     cex.main=1.5, cex.lab=1.3)
points(data2, col=2)
points(data3, col=3)
points(data4, col=4)
points(data5, col=6)
legend(x=0, y=.00275,c("0-20%","20-40%","40-60%","60-80%","80-100%"),col=c(1,2,3,4,6),pch=1, cex=1.2)



# Welch's t-test to compare the 0-20% quantile against the others

t.test(data1,data2)

t.test(data1,data3)

t.test(data1,data4)

t.test(data1,data5)



# Binomial Exact Test to compare 0-20% quantile against the others

length(which(data1>data2))   # 94
1-pbinom(94, 146, prob=.5)  # 0.00017  p-value


length(which(data1>data3))   # 101
1-pbinom(101, 146, prob=.5)  # 8.85e-07  p-value


length(which(data1>data4))   # 97
1-pbinom(97, 146, prob=.5)  # 2.13e-05  p-value


length(which(data1>data5))   # 112
1-pbinom(112, 146, prob=.5)  # 9.51e-12  p-value



# ANOVA to check whether further quantile separation is needed

anova.data <- matrix(c(daily.chargeoff,rep(0,n)), byrow=F, ncol=2)  # need to combine data with quantile as factor variable

for (row in 1:n) {
    
    if (row < n/5) {anova.data[row,2] <- 1}
    
    else if (row < 2*n/5) {anova.data[row,2] <- 2}
    
    else if (row < 3*n/5) {anova.data[row,2] <- 3}
    
    else if (row < 4*n/5) {anova.data[row,2] <- 4}
    
    else {anova.data[row,2] <- 5}
    
}

anova.data <- anova.data[(n/5):n,]  # remove the first quantile

lm.data <- aov(anova.data[,1] ~ as.factor(anova.data[,2]))

summary(lm.data)




#####
##### Calculate Total Chargeoff Probability #####
#####


mean.20 <- mean(data1)  # mean of the 0-20% quantile

mean.80 <- mean(daily.chargeoff[(n/5):n])   # pooled mean of the 20-80% quantile 


total.chargeoff <- 0  # running total chargeoff rate


for (person in 1:nrow(data)) {  # loop through each borrower in the data
    
    
    if(is.na(data[person,2])) {  # if they haven't *already* committed chargeoff
        
        
        if (data[person,1]<=146) {  # if they haven't made it through the first 20% of risky days yet
            
            
            p.chargeoff = (146 - data[person,1])*mean.20 + (1095-146)*mean.80   # extra risky until day 145, then normal risk
            
        }
        
        else {
            
            p.chargeoff = (1095 - data[person,1])*mean.80   # if they already surpassed day 145, assign normal risk for remaining days
            
        }
        
    }
    
    else {
        
        p.chargeoff = 1     # if the borrower has already committed chargeoff, they have risk = 100%
        
    }
    
    total.chargeoff <- total.chargeoff + p.chargeoff    # progressively add up the cumulative chargeoff risk over all borrowers
    
}


total.chargeoff






