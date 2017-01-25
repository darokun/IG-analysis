#-----
# Initial stuff
#-----

# setting up my working directory
setwd("/Users/daro/Documents/GitHub/IG-analysis") # change depending on where your xlsx file is.

# loading the package called "xlsx" to work with Excel spreadsheets
install.packages("xlsx")
library(xlsx)

# loading in the data into an object called "data"
# I am reading the file with a function called read.xlsx
# I am reading only the first spreadsheet, starting on row 2 and ending on row 32
data <- read.xlsx("RohblattInstagram.xlsx", sheetIndex = 1,
                  startRow = 2, endRow = 32)

#-----
# Data Exploration
#-----

# quickly explore the first and last 6 lines of the data
head(data)
tail(data)

# quickly explore the structure of the data
str(data)

# more data exploration
boxplot(data$Follower ~ data$Blogger)
data$Blogger
data$Follower[23]

mean(data$Follower)
hist(data$Follower, breaks = 15)
median(data$Follower)

# convert column names into objects I can easily remember
# the number in brackets points to column number on the spreadsheet
blog.name <- data[,1]
n.followers <- data[,2]
n.new.posts <- data[,3]
n.comments <- data[,4]
n.likes <- data[,5]
n.interactions <- data[,6]
media.value <- data[,7]

# collect new data for new followers:
#date: 14.01.2016 Saturday

(439+581+892+364+256+275)*5 #sherlinanym
(21+176+231+71+131+153)*5 #allaboutelisa
(2581+2118+1154+996+642+569)*5 #carodaur

(11+40+109+95+52+73)*5 #ninasuess
(8+21)*5 #annafrost
(531+646+764+435+658+331)*5 #debiflue

(107+89+84+73+82+49)*5 #pinkfoxy
(111+371+102+139+98+84)*5 #fashiioncarpet
(1749+2382+2799+3778+2507+1497)*5 #ohhcouture

(324+466+326+222+295+215)*5 #mvb
(7+74+272+256+144+8)*5 #matiamubysofia
(6+11+42+17+10+14)*5 #hoardoftrends

(162+53+125+90+165+37)*5 #marinathemoss
(5+41)*5 #fashionhippieloves
(78+62+25+68+92+57)*5 #mashasedgwick

(578+241+618+627+375+274)*5 #luanasilva
(143+159+169+57+30+63)*5 #cocos_wonderland
(222+210+111+15+30+9)*5 #constantly_k

(845+1163+1420+3570+2364+1500)*5 #caro_e_
(585+821+1215+851+611+286)*5 #xeniaoverdose
(66+124+31+82+35+19)*5 #majawyh

(23+12+26+50+62+11)*5 #journelles
(20+8+51+29+14)*5 #josieloves
(368+215+440+476+239+270)*5 #novalanalove

(20+24+4+14)*5 #bekleidet
(9+1171)*5 #linakottutz
(52+98+144+108+85+130)*5 #luisalion

(337+202+256+145+99+59)*5 #lenaterlutter
(55+100+26+50+106+33)*5 #phiaka
(602+312+635+395+252+169)*5 #milenalesecret

new.followers <- c(14035, 3915, 40300,
                   1900, 145, 16825, 
                   2420, 4525, 73560,
                   9240, 3805, 500,
                   3160, 230, 1910, 
                   13565, 3105, 2985,
                   54310, 21845, 1785,
                   920, 610, 10040, 
                   310, 5900, 3085,
                   5490, 1850, 11825)

# collect data for total posts: (date: 16.01.17)
n.total.posts <- c(796, 318, 1770, 1571, 3665, 849,
                   831, 2163, 1296, 1399, 1561, 1935, 
                   1997, 4256, 2269, 942, 1058, 917, 
                   1137, 1889, 3198, 3808, 4572, 3653,
                   2367, 2140, 2680, 4255, 788, 980)

# add the new.followers object into the database:
data[,8] <- new.followers
data[,9] <- n.total.posts
colnames(data)[8] <- "New Followers" # assign new variable name
colnames(data)[9] <- "Total Posts" # assign new variable name

# create objects with names I can easily remember
blog.name <- data[,1]        # blogger username on IG
n.followers <- data[,2]      # total number of followers
n.new.posts <- data[,3]      # number of new posts
n.comments <- data[,4]       # number of comments
n.likes <- data[,5]          # number of likes
n.interactions <- data[,6]   # number of interactions (comments + likes)
media.value <- data[,7]      # media value per post


# recode hashtag variable
n.hashtag <- NULL                                  # new empty vector: age.cat1
n.hashtag[hashtag <= 2] <- 1                       # First level: 0-2
n.hashtag[hashtag >= 2.01 & hashtag <= 4] <- 2      # Second level: 2.01-4
n.hashtag[hashtag >= 4.01 & hashtag <= 6] <- 3      # Third level: 4.01-6
n.hashtag <- as.factor(n.hashtag)                  # Convert age.cat1 into factor
#tab$age.cat1 <- age.cat1                          # Add age.cat1 to dataset
summary(n.hashtag)                                 # check variable

#---

# create a new excel file with the new columns (new followers and total posts)
write.xlsx(data, "RohblattInstagram2.xlsx")

# UNIVARIATE ANALYSIS (JUST ONE VARIABLE AT THE TIME)
# checking if variables are normally distributed:
hist(n.followers, breaks=15) # non-normal
hist(new.followers, breaks=15) # non-normal
hist(n.new.posts, breaks=15) # non-normal
hist(n.total.posts, breaks=15) # non-normal
hist(n.comments, breaks=15) # non-normal
hist(n.likes, breaks=15) # non-normal
hist(n.interactions, breaks=15) # non-normal
hist(media.value, breaks=15) # non-normal

# normality tests:
shapiro.test(n.followers)
shapiro.test(new.followers)
shapiro.test(n.new.posts) # middle-point p-value = 0.05262 # mean?
shapiro.test(n.total.posts)
shapiro.test(n.comments)
shapiro.test(n.likes)
shapiro.test(n.interactions)
shapiro.test(media.value)


# conclusion: use non-parametric statistics

# BIVARIATE ANALYSIS
# hypotheses:
# 1a. more total posts <-> more total followers
# 1b. more new posts <-> more new followers
# 2. more interactions <->  more media value
# 3. more new posts <->  more new likes
# 4. more interactions <->  more new followers
# 5. more comments <->  more likes


# scatterplots
# 1a
plot(n.total.posts, n.followers, pch = 20)
abline(lm(n.followers ~ n.total.posts), col="red") # nothing much

# 1b
plot(n.new.posts, new.followers, pch = 20)
abline(lm(new.followers ~ n.new.posts), col="red") # looks likely

# 2
plot(n.interactions, media.value, pch = 20)
abline(lm(media.value ~ n.interactions), col="red") # looks very likely

# 3
plot(n.new.posts, n.likes, pch = 20)
abline(lm(n.likes ~ n.new.posts), col="red") # looks somehow likely

# 4
plot(n.interactions, new.followers, pch = 20)
abline(lm(new.followers ~ n.interactions), col="red") # looks very likely

# 5
plot(n.comments, n.likes, pch = 20)
abline(lm(n.likes ~ n.comments), col="red") # looks very likely

# notes:
# 1a doesn't show much because we assume that these bloggers started from scratch, being unknown, with a faily low growth rate throughout their existence. However, 1b looks more likely because in the last month they are well established, post regularly, have a good number of followers already, are included in the recommendations algorithm from IG, etc, etc.


#------
# Data Analysis
#------
boxplot(n.followers)
# Table 1
# create table 1
# summary values (median, ...)
labels <- c("Variable", "Median", "IQR")
row1 <- cbind("Total Followers", median(n.followers), IQR(n.followers))
row2 <- cbind("New Followers", median(new.followers), IQR(new.followers))
row3 <- cbind("Total Posts", median(n.total.posts), IQR(n.total.posts))
row4 <- cbind("New Posts", median(n.new.posts), IQR(n.new.posts))
row5 <- cbind("Comments", median(n.comments), IQR(n.comments))
row6 <- cbind("Likes", median(n.likes), IQR(n.likes))
row7 <- cbind("Interactions", median(n.interactions), IQR(n.interactions))
row8 <- cbind("Media Value", median(media.value), IQR(media.value))
table1 <- rbind(labels, row1, row2, row3, row4, row5, row6, row7, row8)
table1
write.xlsx(table1, "table1.xlsx") # write it to a xlsx file

#---

# create table 2: matrix of correlations
# correlation matrix
df <- data.frame(n.followers, new.followers, n.total.posts, n.new.posts, n.comments, n.likes, n.interactions, media.value)
round(cor(df, method = "kendall"),2) # get correlations (returns matrix)
write.xlsx(round(cor(df, method = "kendall"),2), "cormatrix.xlsx") # writes matrix to a xlsx file

# correlation with n.followers
cor.test(new.followers, n.followers, method = "kendall") # 0.49 p = 7.096e-05 ***
cor.test(n.total.posts, n.followers, method = "kendall") # -0.14 p = 0.2871
cor.test(n.new.posts, n.followers, method = "kendall") # 0.24 p = 0.0634
cor.test(n.comments, n.followers, method = "kendall") # 0.54 p = 2.541e-05 ***
cor.test(n.likes, n.followers, method = "kendall") # 0.73 p = 2.184e-10 ***
cor.test(n.interactions, n.followers, method = "kendall") # 0.73 p = 2.184e-10 ***
cor.test(media.value, n.followers, method = "kendall") # 0.98 p = 1.11e-15 ***

# correlation with new.followers
cor.test(n.total.posts, new.followers, method = "kendall") # -0.31 p = 0.01433 ***
cor.test(n.new.posts, new.followers, method = "kendall") # 0.31 p = 0.0152 ***
cor.test(n.comments, new.followers, method = "kendall") # 0.59 p = 4.15e-06 ***
cor.test(n.likes, new.followers, method = "kendall") # 0.58 p = 1.705e-06***
cor.test(n.interactions, new.followers, method = "kendall") # 0.58 p = 1.705e-06 ***
cor.test(media.value, new.followers, method = "kendall") # 0.50 p = 4.996e-05 ***

# correlation with n.total.posts
cor.test(n.new.posts, n.total.posts, method = "kendall") # 0.06 p = 0.6426
cor.test(n.comments, n.total.posts, method = "kendall") # -0.30 p = 0.0185 ***
cor.test(n.likes, n.total.posts, method = "kendall") # -0.31 p = 0.01588 ***
cor.test(n.interactions, n.total.posts, method = "kendall") # -0.31 p = 0.01588 ***
cor.test(media.value, n.total.posts, method = "kendall") # -0.15 p = 0.256

# correlation with n.new.posts
cor.test(n.comments, n.new.posts, method = "kendall") # 0.18 p = 0.1584
cor.test(n.likes, n.new.posts, method = "kendall") # 0.23 p = 0.08025
cor.test(n.interactions, n.new.posts, method = "kendall") # 0.23 p = 0.08025
cor.test(media.value, n.new.posts, method = "kendall") # 0.24 p = 0.0634

# correlation with n.comments
cor.test(n.likes, n.comments, method = "kendall") # 0.70 p = 4.755e-08 ***
cor.test(n.interactions, n.comments, method = "kendall") # 0.70 p = 4.755e-08 ***
cor.test(media.value, n.comments, method = "kendall") # 0.56 p = 1.337e-05 ***

# correlation with n.likes
cor.test(n.interactions, n.likes, method = "kendall") # 1.00 p = 1.11e-15 ***
cor.test(media.value, n.likes, method = "kendall") # 0.75 p = 5.056e-11 ***

# correlation with n.interactions
cor.test(media.value, n.interactions, method = "kendall") # 0.75 p = 5.056e-11 ***

# Revise hypotheses
# hypotheses: (see "cormatrix.xlsx" file)
# 1a. more total posts <-> more total followers =======> NO, because correlation coef -0.14 and non-significant p = 0.2871
# 1b. more new posts <-> more new followers     =======> YES, slightly, bc cor coef 0.31 and significant p = 0.0152
# 2. more interactions <->  more media value    =======> YES, HIGH!, bc cor coef 0.75 and significant p = 5.056e-11
# 3. more new posts <->  more new likes         =======> YES, slightly, bc cor coef 0.31 and significant p = 0.0152
# 4. more interactions <->  more new followers  =======> YES, somehow, bc cor coef 0.58 and significant p = 1.705e-06
# 5. more comments <->  more likes              =======> YES, HIGH!, bc cor coef 0.70 and significant p = 4.755e-08

#----
# Table 3

# model1: what variables determine the increase of new.followers?
# create full model (don't include n.interactions because it is redundant with n.comments and n.likes)
full.model1 <- lm(new.followers ~ n.followers + n.total.posts + n.comments + n.likes + media.value)
summary(full.model1)

# model selection (using AIC = Akaike's Information Criterion and step-wise regression)
install.packages("MASS") # install package MASS
library(MASS) # load package MASS
stepAIC(full.model1) # says the best model should be new.followers ~ n.followers + n.likes + media.value

best.model1 <- lm(new.followers ~ n.followers + n.likes + media.value)
summary(best.model1) # display results

# better display results:
install.packages("broom") # install package broom
library(broom) # load package broom
table3a <- tidy(best.model1) # get coefficient table as a data frame
table3a <- table3a[,c(-3,-4)] # delete columns we don't care about
write.xlsx(table3a, "best.model1.xlsx") # write to a xlsx file
glance(best.model1) # get rest of stats as a data frame
AIC(best.model1)
#-----
# END OF SCRIPT
#-----

# to do:
# codebuch
# descriptive stats for new variables
# boxplots or barplots for individual variables
# bivariate tests?
# model with everything