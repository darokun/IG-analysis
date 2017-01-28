#-----
# Initial stuff
#-----

# setting up my working directory
setwd("/Users/daro/Documents/GitHub/IG-analysis") # change depending on where your xlsx file is.

# loading the package called "xlsx" to work with Excel spreadsheets
# install.packages("xlsx")
library(xlsx)

# loading in the data into an object called "data"
# I am reading the file with a function called read.xlsx
# I am reading only the first spreadsheet, starting on row 2 and ending on row 32
data <- read.xlsx("Instagram.Data.xlsx", sheetIndex = 1,
                  startRow = 2, endRow = 32, colIndex = 1:13)

#-----
# Data Exploration
#-----

# quickly explore the first and last 6 lines of the data
head(data)
tail(data)

# quickly explore the structure of the data
str(data)

# # more data exploration
# boxplot(data$Follower ~ data$Blogger)
# data$Blogger
# data$Follower[23]
# 
# mean(data$Follower)
# hist(data$Follower, breaks = 15)
# median(data$Follower)

# convert column names into objects I can easily remember
# the number in brackets points to column number on the spreadsheet
blog.name <- data[,1]
n.total.followers <- data[,2]
n.new.followers <- data[,3]
n.total.posts <- data[,4]
n.new.posts <- data[,5]
n.comments <- data[,6]
n.likes <- data[,7]
n.interactions <- data[,8]
media.value <- data[,9]
anford.labels <- c("nein", "ja")
caption.labels <- c("sachlich", "unterhaltend", "werbend")
place.labels <- c("langweilig", "interessant", "sehr interessant")
anford <- factor(data[,10], labels = anford.labels) # 0 = nein, 1 = ja
caption <- factor(data[,11], labels = caption.labels) # 1 = sachlich, 2 = unterhaltend, 3 = werbend
hashtag <- factor(data[,12],  labels = hashtag.labels) # 1= wenige (0-2), 2=mittel (2.01-4) 3= viele (4.01-6)
place <- factor(data[,13], labels = place.labels) # 1=langweilig, 2= interessant 3= sehr interessant
 
# collect new data for new followers:
#date: 14.01.2016 Saturday

# (439+581+892+364+256+275)*5 #sherlinanym
# (21+176+231+71+131+153)*5 #allaboutelisa
# (2581+2118+1154+996+642+569)*5 #carodaur
# 
# (11+40+109+95+52+73)*5 #ninasuess
# (8+21)*5 #annafrost
# (531+646+764+435+658+331)*5 #debiflue
# 
# (107+89+84+73+82+49)*5 #pinkfoxy
# (111+371+102+139+98+84)*5 #fashiioncarpet
# (1749+2382+2799+3778+2507+1497)*5 #ohhcouture
# 
# (324+466+326+222+295+215)*5 #mvb
# (7+74+272+256+144+8)*5 #matiamubysofia
# (6+11+42+17+10+14)*5 #hoardoftrends
# 
# (162+53+125+90+165+37)*5 #marinathemoss
# (5+41)*5 #fashionhippieloves
# (78+62+25+68+92+57)*5 #mashasedgwick
# 
# (578+241+618+627+375+274)*5 #luanasilva
# (143+159+169+57+30+63)*5 #cocos_wonderland
# (222+210+111+15+30+9)*5 #constantly_k
# 
# (845+1163+1420+3570+2364+1500)*5 #caro_e_
# (585+821+1215+851+611+286)*5 #xeniaoverdose
# (66+124+31+82+35+19)*5 #majawyh
# 
# (23+12+26+50+62+11)*5 #journelles
# (20+8+51+29+14)*5 #josieloves
# (368+215+440+476+239+270)*5 #novalanalove
# 
# (20+24+4+14)*5 #bekleidet
# (9+1171)*5 #linakottutz
# (52+98+144+108+85+130)*5 #luisalion
# 
# (337+202+256+145+99+59)*5 #lenaterlutter
# (55+100+26+50+106+33)*5 #phiaka
# (602+312+635+395+252+169)*5 #milenalesecret
# 
# new.followers <- c(14035, 3915, 40300,
#                    1900, 145, 16825, 
#                    2420, 4525, 73560,
#                    9240, 3805, 500,
#                    3160, 230, 1910, 
#                    13565, 3105, 2985,
#                    54310, 21845, 1785,
#                    920, 610, 10040, 
#                    310, 5900, 3085,
#                    5490, 1850, 11825)
# 
# # collect data for total posts: (date: 16.01.17)
# n.total.posts <- c(796, 318, 1770, 1571, 3665, 849,
#                    831, 2163, 1296, 1399, 1561, 1935, 
#                    1997, 4256, 2269, 942, 1058, 917, 
#                    1137, 1889, 3198, 3808, 4572, 3653,
#                    2367, 2140, 2680, 4255, 788, 980)

# # add the new.followers object into the database:
# data[,8] <- new.followers
# data[,9] <- n.total.posts
# colnames(data)[8] <- "New Followers" # assign new variable name
# colnames(data)[9] <- "Total Posts" # assign new variable name
# 
# # create objects with names I can easily remember
# blog.name <- data[,1]        # blogger username on IG
# n.followers <- data[,2]      # total number of followers
# n.new.posts <- data[,3]      # number of new posts
# n.comments <- data[,4]       # number of comments
# n.likes <- data[,5]          # number of likes
# n.interactions <- data[,6]   # number of interactions (comments + likes)
# media.value <- data[,7]      # media value per post

#---

# create a new excel file with the new columns (new followers and total posts)
# write.xlsx(data, "RohblattInstagram2.xlsx")

# UNIVARIATE ANALYSIS (JUST ONE VARIABLE AT THE TIME)
# checking if variables are normally distributed:
hist(n.total.followers, breaks=15) # non-normal
hist(n.new.followers, breaks=15) # non-normal
hist(n.total.posts, breaks=15) # non-normal
hist(n.new.posts, breaks=15) # borderline normal
hist(n.comments, breaks=15) # non-normal
hist(n.likes, breaks=15) # non-normal
hist(n.interactions, breaks=15) # non-normal
hist(media.value, breaks=15) # non-normal

# normality tests:
shapiro.test(n.total.followers)
shapiro.test(n.new.followers)
shapiro.test(n.total.posts) 
shapiro.test(n.new.posts) # middle-point p-value = 0.05262 # use mean?
shapiro.test(n.comments)
shapiro.test(n.likes)
shapiro.test(n.interactions)
shapiro.test(media.value)

# conclusion: use non-parametric statistics

#---
# Plots:
png("Boxplot.Total.Followers.png")
boxplot(n.total.followers, main = "Boxplot of Total Number of Followers", 
        ylab = "Frequency")
dev.off()

png("Boxplot.New.Followers.png")
boxplot(n.new.followers, main = "Boxplot of Number of New Followers", 
        ylab = "Frequency")
dev.off()

png("Boxplot.Total.Posts.png")
boxplot(n.total.posts, main = "Boxplot of Total Number of Posts", 
        ylab = "Frequency")
dev.off()

png("Boxplot.New.Posts.png")
boxplot(n.new.posts, main = "Boxplot of Number of New Posts", 
        ylab = "Frequency")
dev.off()

png("Boxplot.Comments.png")
boxplot(n.comments, main = "Boxplot of Number of Comments", 
        ylab = "Frequency")
dev.off()

png("Boxplot.Likes.png")
boxplot(n.likes, main = "Boxplot of Number of Likes", 
        ylab = "Frequency")
dev.off()

png("Boxplot.Interactions.png")
boxplot(n.interactions, main = "Boxplot of Interactions", 
        ylab = "Frequency")
dev.off()

png("Boxplot.MediaValue.png")
boxplot(media.value, main = "Boxplot of Media Value", 
        ylab = "US Dollars")
dev.off()

png("Barplot.Anforderungen.png")
plot(anford, main = "Anforderungen", ylab = "Frequency", ylim = c(0,20),
     col = c("turquoise", "coral"))
dev.off()

png("Barplot.Caption.png")
plot(caption, main = "Assessment of Picture Caption", ylab = "Frequency", ylim = c(0,15),
     col = c("turquoise", "coral", "pink"))
dev.off()

png("Barplot.Hashtag.png")
plot(hashtag, main = "Number of Hashtags", ylab = "Frequency", ylim = c(0,20),
     col = c("turquoise", "coral", "pink"))
dev.off()

png("Barplot.Place.png")
plot(place, main = "Assessment of Place", ylab = "Frequency", ylim = c(0,15),
     col = c("turquoise", "coral", "pink"))
dev.off()


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
plot(n.total.posts, n.total.followers, pch = 20)
abline(lm(n.total.followers ~ n.total.posts), col="red") # nothing much

# 1b
plot(n.new.posts, n.new.followers, pch = 20)
abline(lm(n.new.followers ~ n.new.posts), col="red") # looks likely

# 2
plot(n.interactions, media.value, pch = 20)
abline(lm(media.value ~ n.interactions), col="red") # looks very likely

# 3
plot(n.new.posts, n.likes, pch = 20)
abline(lm(n.likes ~ n.new.posts), col="red") # looks somehow likely

# 4
plot(n.interactions, n.new.followers, pch = 20)
abline(lm(n.new.followers ~ n.interactions), col="red") # looks very likely

# 5
plot(n.comments, n.likes, pch = 20)
abline(lm(n.likes ~ n.comments), col="red") # looks very likely

# notes:
# 1a doesn't show much because we assume that these bloggers started from scratch, being unknown, with a faily low growth rate throughout their existence. However, 1b looks more likely because in the last month they are well established, post regularly, have a good number of followers already, are included in the recommendations algorithm from IG, etc, etc.

# boxplots
# new hypotheses:
# 6. if place is more interesting <=> more interactions
# 7. if the caption is more into advertising <=> less [new] followers
# 8. if anford is yes <=> more interactions
# 9. if more hashtags <=> more likes

png("Boxplot.interactionsANDplace.png")
boxplot(n.interactions ~ place, 
        main = "Number of interactions according to assessment of place",
        ylab = "Frequency",
        col = c("turquoise", "coral", "pink"))
dev.off()

png("Boxplot.newFollowersANDcaption.png")
boxplot(n.new.followers ~ caption, 
        main = "Number of New Followers according to type of caption",
        ylab = "Frequency",
        col = c("turquoise", "coral", "pink"))
dev.off()

png("Boxplot.interactionsANDanford.png")
boxplot(n.interactions ~ anford, 
        main = "Number of interactions according to Anforderungen",
        ylab = "Frequency",
        col = c("turquoise", "coral"))
dev.off()

png("Boxplot.likesANDhashtags.png")
boxplot(n.likes ~ hashtag, 
        main = "Number of likes according to number of hashtags",
        ylab = "Frequency",
        col = c("turquoise", "coral", "pink"))
dev.off()


#------
# Data Analysis
#------

# Table 1
# create table 1a: numerical variables
# summary values (median, ...)
table1a.labels <- c("Variable", "Median", "IQR")
row1 <- cbind("Total Followers", median(n.total.followers), IQR(n.total.followers))
row2 <- cbind("New Followers", median(n.new.followers), IQR(n.new.followers))
row3 <- cbind("Total Posts", median(n.total.posts), IQR(n.total.posts))
row4 <- cbind("New Posts", median(n.new.posts), IQR(n.new.posts))
row5 <- cbind("Comments", median(n.comments), IQR(n.comments))
row6 <- cbind("Likes", median(n.likes), IQR(n.likes))
row7 <- cbind("Interactions", median(n.interactions), IQR(n.interactions))
row8 <- cbind("Media Value", median(media.value), IQR(media.value))
table1a <- rbind(table1a.labels, row1, row2, row3, row4, row5, row6, row7, row8)
table1a
#write.xlsx(table1a, "table1a.xlsx") # write it to a xlsx file

# create table 1b: categorical variables
# absolute and relative numbers:
table1b.labels <- c("Variable", "Frequency", "Percentage")
row9 <- cbind("Anforderungen", table(anford), round(prop.table(table(anford)),2)*100)
row10 <- cbind("Caption", table(caption), round(prop.table(table(caption)),2)*100)
row11 <- cbind("Hashtags", table(hashtag), round(prop.table(table(hashtag)),2)*100)
row12 <- cbind("Place", table(place), round(prop.table(table(place)),2)*100)
table1b <- rbind(table1b.labels, row9, row10, row11, row12)
table1b
#write.xlsx(table1b, "table1b.xlsx") # write it to a xlsx file and fix it later

#---

# create table 2: matrix of correlations
# correlation matrix
df <- data.frame(n.total.followers, n.new.followers, n.total.posts, n.new.posts, n.comments, n.likes, n.interactions, media.value)
round(cor(df, method = "kendall"),2) # get correlations (returns matrix)
#write.xlsx(round(cor(df, method = "kendall"),2), "cormatrix.xlsx") # writes matrix to a xlsx file

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
# 3. more new posts <->  more new likes         =======> No. The correlation is just low-medium, and non-significant (r = 0.23, p-value = 0.08).
# 4. more interactions <->  more new followers  =======> YES, somehow, bc cor coef 0.58 and significant p = 1.705e-06
# 5. more comments <->  more likes              =======> YES, HIGH!, bc cor coef 0.70 and significant p = 4.755e-08

# according to our data, the number of new posts and the number of new followers seem to follow a positive and significant correlation (r = 0.31, p-value = 0.0152).

# according to our data, we would reject null hypotheses number 1b to 5 in favor of the alternative due to positive and significant correlation (see Table 4). However, there is not enough evidence to reject the null hypothesis number 1a (r = -0.14, p-value = 0.2871)

#----

# install.packages("psych")
library(psych)

describeBy(n.interactions, place) 
# median interaction by place: 
# langweilig = 4168, interessant = 5030.5, sehr interessant = 11159.5

describeBy(n.new.followers, caption) 
# median new followers by caption: 
# sachlich = 4502.5, unterhaltend = 1850, werbend = 3915

describeBy(n.interactions, anford) 
# median interaction by anford: 
# nein = 9445, ja = 5360

describeBy(n.likes, hashtag) 
# median interaction by anford: 
# wenige = 5750, mittel = 3595, viele = 23086

# tests of proportions

# new hypotheses:
# 6. if place is more interesting <=> more interactions
# 7. if the caption is more into advertising <=> less [new] followers
# 8. if anford is yes <=> more interactions
# 9. if more hashtags <=> more likes

# choose hypothesis test depending on type of variables:
# 6. ordinal and numerical -> kruskal wallis
# 7. ordinal and numerical -> kruskal wallis
# 8. nominal and numerical -> wilcoxon
# 9. ordinal and numerical -> kruskal wallis

# run tests:
kruskal.test(place, n.interactions) # non-significant
kruskal.test(caption, n.new.followers) # non-significant
wilcox.test(n.interactions ~ anford) # non-significant
kruskal.test(hashtag, n.likes) # non-significant
wilcox.test(n.likes ~ hashtag) # non-significant




# TO DO: Table of this

#----
# Table 3a

# model1: what variables determine the increase of new.followers?
# create full model (don't include n.interactions because it is redundant with n.comments and n.likes)
full.model1 <- lm(n.new.followers ~ n.total.followers + n.total.posts + n.comments + n.likes + media.value + place + caption + anford + hashtag)
summary(full.model1)

# model selection (using AIC = Akaike's Information Criterion and step-wise regression)
# install.packages("MASS") # install package MASS
library(MASS) # load package MASS
stepAIC(full.model1) # says the best model should be n.new.followers ~ n.comments + n.likes + place + anford + hashtag

best.model1 <- lm(n.new.followers ~ n.comments + n.likes + place + anford + hashtag)
summary(best.model1) # display results

# better display results:
# install.packages("broom") # install package broom
library(broom) # load package broom
table3a <- tidy(best.model1) # get coefficient table as a data frame
table3a <- table3a[,c(-3,-4)] # delete columns we don't care about
# write.xlsx(table3a, "best.model1.xlsx") # write to a xlsx file
glance(best.model1) # get rest of stats as a data frame
AIC(best.model1)

#---

# Table 3b

# model2: what variables determine the increase of n.interactions?
# create full model (don't include n.interactions because it is redundant with n.comments and n.likes)
full.model2 <- lm(n.interactions ~ n.total.followers + n.new.followers + n.total.posts + media.value + place + caption + anford + hashtag)
summary(full.model2)

# model selection (using AIC = Akaike's Information Criterion and step-wise regression)
# install.packages("MASS") # install package MASS
stepAIC(full.model2) # says the best model should be n.new.followers ~ n.comments + n.likes + place + anford + hashtag

best.model2 <- lm(n.interactions ~ n.total.followers + n.new.followers + 
                    n.total.posts + media.value)
summary(best.model2) # display results

# better display results:
table3b <- tidy(best.model2) # get coefficient table as a data frame
table3b <- table3b[,c(-3,-4)] # delete columns we don't care about
# write.xlsx(table3b, "best.model2.xlsx") # write to a xlsx file
glance(best.model2) # get rest of stats as a data frame
AIC(best.model2)


#-----
# END OF SCRIPT
#-----

# to do:
# tables aesthetics DONE
# check levels of hashtag



