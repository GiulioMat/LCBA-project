library(ggplot2)
library(vcd)  # data visualization for categorical variables

library("ggthemes")
library("grid")
library(ggpubr)


setwd("D:/software/R Files/Business and Customer Analytics/Final Projects")
# Demogra_data <- read.csv('Demographic_data.csv')
Demogra_data <- read.csv('Laptop_Research_Survey.csv')

Demogra_data$Age <- factor(Demogra_data$Age, levels = c("18", "18-20", "20-23", "24-26", "27-30", ">30"))
Demogra_data$Gender <- factor(Demogra_data$Gender, levels = c("male", "female", "prefer not to say"))
Demogra_data$Education <- factor(Demogra_data$Education)
Demogra_data$Work. <- factor(Demogra_data$Work)

summary(Demogra_data)

# ------------- Relation between Age and rating -------------------
ggplot(data=Demogra_data,aes(x=Age,y=rating)) +geom_boxplot()
mosaic( ~ Age + rating, data = Demogra_data,
        labeling_args = list(set_varnames = c(rating = "Response to Computers", Age = "Age level")),
        highlighting = "Age", highlighting_fill=rainbow)

# ------------- Relation between Gender and rating -------------------
ggplot(data=Demogra_data,aes(x=Gender,y=rating)) +geom_boxplot()
# examine the frequency table for education
with(Demogra_data,table(rating, Gender))
with(Demogra_data,prop.table(table(rating, Gender), margin=1))
with(Demogra_data,chisq.test(table(rating, Gender)))
mosaic( ~ Gender + rating, data = Demogra_data,
        labeling_args = list(set_varnames = c(rating = "Response to Computers", Age = "Gender level")),
        highlighting = "Gender", highlighting_fill=rainbow)

# ------------- Relation between Education and rating -------------------
ggplot(data=Demogra_data,aes(x=Education,y=rating)) +geom_boxplot()
# examine the frequency table for education
with(Demogra_data,table(rating, Education))
with(Demogra_data,prop.table(table(rating, Education), margin=1))
with(Demogra_data,chisq.test(table(rating, Education)))

mosaic( ~ Education + rating, data = Demogra_data,
        labeling_args = list(set_varnames = c(rating = "Response to Computers", Age = "Education level")),
        highlighting = "Education", highlighting_fill=rainbow)

# ------------- Relation between Work. and rating -------------------
ggplot(data=Demogra_data,aes(x=Work.,y=rating)) +geom_boxplot()

with(Demogra_data,table(rating, Work.))
with(Demogra_data,prop.table(table(rating, Work.), margin=1))
with(Demogra_data,chisq.test(table(rating, Work.)))

mosaic( ~ Work. + rating, data = Demogra_data, 
        labeling_args = list(set_varnames = c(rating = "Response to Computers", Age = "Work level")),
        highlighting_fill=rainbow)

# -----------------------------------------------
# Focus on Gender, attributes of computer and rating
# -----------------------------------------------
num_male <- nrow(Demogra_data[Demogra_data$Gender=="male",])/9
num_female <- nrow(Demogra_data[Demogra_data$Gender=="female",])/9
num_not_say <- nrow(Demogra_data[Demogra_data$Gender=="prefer not to say",])/9
# we see that the number of female participants is twice as larger than number of male
# the number of participants who do not choose their gender is small, which is not 
# convincing enough to make a conclusion

# -----------  Relation between Gender, RAM and rating  --------------
pic <- Demogra_data[,c(2,4,11)]
pic <- aggregate(pic$rating, by=list(type=pic$Gender, pic$RAM),mean)
names(pic) <-c ("Gender","RAM", "Average_rating")
pic$RAM <- factor(pic$RAM)

ggplot(data=pic, mapping=aes(x = RAM, y = Average_rating,fill=Gender))+
        geom_bar(stat="identity",position=position_dodge(0.75))
# almost everyone prefer larger RAM, but there is no big difference between 8GB and 16GB

# -----------  Relation between Gender, Screen_size and rating  --------------
pic <- Demogra_data[,c(2,6,11)]
pic <- aggregate(pic$rating, by=list(type=pic$Gender, pic$screen_size),mean)
names(pic) <-c ("Gender","Screen_size", "Average_rating")
pic$Screen_size <- factor(pic$Screen_size)

ggplot(data=pic, mapping=aes(x = Screen_size, y = Average_rating,fill=Gender))+
        geom_bar(stat="identity",position=position_dodge(0.75))
# male students prefer larger screen size, while females prefer smaller ones

# -----------  Relation between Gender, battery and rating  --------------
pic <- Demogra_data[,c(2,7,11)]
pic <- aggregate(pic$rating, by=list(type=pic$Gender, pic$battery),mean)
names(pic) <-c ("Gender","battery", "Average_rating")
pic$battery <- factor(pic$battery)

ggplot(data=pic, mapping=aes(x = battery, y = Average_rating,fill=Gender))+
        geom_bar(stat="identity",position=position_dodge(0.75))
# Battery is not a crucial influencer in decision-making of male, 
# but female prefer a computer with longer standby time

# -----------  Relation between Gender, operating_system and rating  --------------
pic <- Demogra_data[,c(2,3,11)]
pic <- aggregate(pic$rating, by=list(type=pic$Gender, pic$operating_system),mean)
names(pic) <-c ("Gender","operating_system", "Average_rating")
pic$operating_system <- factor(pic$operating_system)

ggplot(data=pic, mapping=aes(x = operating_system, y = Average_rating,fill=Gender))+
        geom_bar(stat="identity",position=position_dodge(0.75))
# most users prefer MacOS and Windows than Linux

# -----------  Relation between Gender, price and rating  --------------
pic <- Demogra_data[,c(2,8,11)]
pic <- aggregate(pic$rating, by=list(type=pic$Gender, pic$price),mean)
names(pic) <-c ("Gender","price", "Average_rating")
pic$price <- factor(pic$price)

ggplot(data=pic, mapping=aes(x = price, y = Average_rating,fill=Gender))+
        geom_bar(stat="identity",position=position_dodge(0.75))
# it is no surprise that people prefer cheaper product with better equality; 
# male students do not think there is a big difference between 500 and 1000


# -----------------------------------------------
# Focus on Education, attributes of computer and rating
# -----------------------------------------------
num_Bachelor <- nrow(Demogra_data[Demogra_data$Education=="Bachelor's degree",])/9
num_Mas_Sci <- nrow(Demogra_data[Demogra_data$Education=="Master of Science",])/9
num_Dr_Phi <- nrow(Demogra_data[Demogra_data$Education=="Doctor of Philosophy",])/9
num_not_stu <- nrow(Demogra_data[Demogra_data$Education=="Not a student",])/9
num_not_say <- nrow(Demogra_data[Demogra_data$Education=="prefer not to say",])/9
# Bachelor=54, Master of Science = 143, Dr of Philosophy = 4, not a student = 5,
# prefer not say = 7
# Again, we see that the numbers of PhD students and people who are working and 
# those who prefer not to say are small, which are not convincing enough to make 
# a concrete conclusion, but here we are trying to interpret the meaning

# -----------  Relation between Education, RAM and rating  --------------
pic <- Demogra_data[,c(2,4,12)]
pic <- aggregate(pic$rating, by=list(type=pic$Education, pic$RAM),mean)
names(pic) <-c ("Education","RAM", "Average_rating")
pic$RAM <- factor(pic$RAM)

ggplot(data=pic, mapping=aes(x = RAM, y = Average_rating,fill=Education))+
        geom_bar(stat="identity",position=position_dodge(0.75))
# in general, people prefer larger RAM

# -----------  Relation between Education, Screen_size and rating  --------------
pic <- Demogra_data[,c(2,6,12)]
pic <- aggregate(pic$rating, by=list(type=pic$Education, pic$screen_size),mean)
names(pic) <-c ("Education","Screen_size", "Average_rating")
pic$Screen_size <- factor(pic$Screen_size)

ggplot(data=pic, mapping=aes(x = Screen_size, y = Average_rating,fill=Education))+
        geom_bar(stat="identity",position=position_dodge(0.75))
# bachelor students have no special requirement of screen size,
# Doctor of philosophy prefer 15.6''
#people who are working prefer larger screen size

# -----------  Relation between Education, battery and rating  --------------
pic <- Demogra_data[,c(2,7,12)]
pic <- aggregate(pic$rating, by=list(type=pic$Education, pic$battery),mean)
names(pic) <-c ("Education","battery", "Average_rating")
pic$battery <- factor(pic$battery)

ggplot(data=pic, mapping=aes(x = battery, y = Average_rating,fill=Education))+
        geom_bar(stat="identity",position=position_dodge(0.75))
# People at work prefer longer standby battery,
# Others do not have special prefrerence

# -----------  Relation between Education, operating_system and rating  --------------
pic <- Demogra_data[,c(2,3,12)]
pic <- aggregate(pic$rating, by=list(type=pic$Education, pic$operating_system),mean)
names(pic) <-c ("Education","operating_system", "Average_rating")
pic$operating_system <- factor(pic$operating_system)

ggplot(data=pic, mapping=aes(x = operating_system, y = Average_rating,fill=Education))+
        geom_bar(stat="identity",position=position_dodge(0.75))
# People who are not students prefer MacOS
# all the students prefer Windows than MacOS, and prefer MacOS than Linux

# -----------  Relation between Education, price and rating  --------------
pic <- Demogra_data[,c(2,8,12)]
pic <- aggregate(pic$rating, by=list(type=pic$Education, pic$price),mean)
names(pic) <-c ("Education","price", "Average_rating")
pic$price <- factor(pic$price)

ggplot(data=pic, mapping=aes(x = price, y = Average_rating,fill=Education))+
        geom_bar(stat="identity",position=position_dodge(0.75))
# in general, people prefer cheaper price, but from PhD students' point of view,
# higher price is not always comes the second 

# -----------------------------------------------
# Focus on Work., attributes of computer and rating
# -----------------------------------------------
num_work_full <- nrow(Demogra_data[Demogra_data$Work=="Yes, full-time",])/9
num_work_part <- nrow(Demogra_data[Demogra_data$Work=="Yes, part-time",])/9
num_work_no <- nrow(Demogra_data[Demogra_data$Work=="No",])/9

# num_work_full=22, num_work_part = 164, No = 27
# Again, we see that the numbers of PhD students and people who are working and 
# those who prefer not to say are small, which are not convincing enough to make 
# a concrete conclusion, but here we are trying to interpret the meaning

# -----------  Relation between Work., RAM and rating  --------------
pic <- Demogra_data[,c(2,4,13)]
pic <- aggregate(pic$rating, by=list(type=pic$Work, pic$RAM),mean)
names(pic) <-c ("Work.","RAM", "Average_rating")
pic$RAM <- factor(pic$RAM)

ggplot(data=pic, mapping=aes(x = RAM, y = Average_rating,fill=Work.))+
        geom_bar(stat="identity",position=position_dodge(0.75))
# Students do not work and have part-time work are preferring larger RAM,
# students have full-time job prefer 8GB

# -----------  Relation between Education, Screen_size and rating  --------------
pic <- Demogra_data[,c(2,6,13)]
pic <- aggregate(pic$rating, by=list(type=pic$Work, pic$screen_size),mean)
names(pic) <-c ("Work.","Screen_size", "Average_rating")
pic$Screen_size <- factor(pic$Screen_size)

ggplot(data=pic, mapping=aes(x = Screen_size, y = Average_rating,fill=Work.))+
        geom_bar(stat="identity",position=position_dodge(0.75))
# screen size has little influence in terms of work

# -----------  Relation between Work., battery and rating  --------------
pic <- Demogra_data[,c(2,7,13)]
pic <- aggregate(pic$rating, by=list(type=pic$Work, pic$battery),mean)
names(pic) <-c ("Work.","battery", "Average_rating")
pic$battery <- factor(pic$battery)

ggplot(data=pic, mapping=aes(x = battery, y = Average_rating,fill=Work.))+
        geom_bar(stat="identity",position=position_dodge(0.75))
# Students do not work and have part-time work are preferring longer standby battery,
# students have full-time job prefer 4-hour standby battery 

# -----------  Relation between Work., operating_system and rating  --------------
pic <- Demogra_data[,c(2,3,13)]
pic <- aggregate(pic$rating, by=list(type=pic$Work, pic$operating_system),mean)
names(pic) <-c ("Work.","operating_system", "Average_rating")
pic$operating_system <- factor(pic$operating_system)

ggplot(data=pic, mapping=aes(x = operating_system, y = Average_rating,fill=Work.))+
        geom_bar(stat="identity",position=position_dodge(0.75))
# Linux is the least prefered operating system
# students who have full-time job are the ones show the strongest interest to MacOS

# -----------  Relation between Work., price and rating  --------------
pic <- Demogra_data[,c(2,8,13)]
pic <- aggregate(pic$rating, by=list(type=pic$Work, pic$price),mean)
names(pic) <-c ("Work.","price", "Average_rating")
pic$price <- factor(pic$price)

ggplot(data=pic, mapping=aes(x = price, y = Average_rating,fill=Work.))+
        geom_bar(stat="identity",position=position_dodge(0.75))
# full-time working students prefer 1000$ computer
# students do not work and have part-time job prefer cheaper computer


# -----------------------------------------------
# Focus on Age, attributes of computer and rating
# -----------------------------------------------
Demogra_data$Age[Demogra_data$Age=="&lt;18"] <- 18
num_18 <- nrow(Demogra_data[Demogra_data$Age=="18",])/9
num_18_20 <- nrow(Demogra_data[Demogra_data$Age=="18-20",])/9
num_20_23 <- nrow(Demogra_data[Demogra_data$Age=="20-23",])/9
num_24_26 <- nrow(Demogra_data[Demogra_data$Age=="24-26",])/9
num_27_30 <- nrow(Demogra_data[Demogra_data$Age=="27-30",])/9
num_30 <- nrow(Demogra_data[Demogra_data$Age==">30",])/9
# 18=3, 18-20=12, 20-23=137, 24-26=52, 27-30=6, >30=3

# -----------  Relation between Age, RAM and rating  --------------
pic <- Demogra_data[,c(2,4,10)]
pic <- aggregate(pic$rating, by=list(type=pic$Age, pic$RAM),mean)
names(pic) <-c ("Age","RAM", "Average_rating")
pic$RAM <- factor(pic$RAM)

ggplot(data=pic, mapping=aes(x = RAM, y = Average_rating,fill=Age))+
        geom_bar(stat="identity",position=position_dodge(0.75))
# No clear conclusion

# -----------  Relation between Age, Screen_size and rating  --------------
pic <- Demogra_data[,c(2,6,10)]
pic <- aggregate(pic$rating, by=list(type=pic$Age, pic$screen_size),mean)
names(pic) <-c ("Age","Screen_size", "Average_rating")
pic$Screen_size <- factor(pic$Screen_size)

ggplot(data=pic, mapping=aes(x = Screen_size, y = Average_rating,fill=Age))+
        geom_bar(stat="identity",position=position_dodge(0.75))
# No clear conclusion

# -----------  Relation between Age, battery and rating  --------------
pic <- Demogra_data[,c(2,7,10)]
pic <- aggregate(pic$rating, by=list(type=pic$Age, pic$battery),mean)
names(pic) <-c ("Age","battery", "Average_rating")
pic$battery <- factor(pic$battery)

ggplot(data=pic, mapping=aes(x = battery, y = Average_rating,fill=Age))+
        geom_bar(stat="identity",position=position_dodge(0.75))
# No clear conclusion

# -----------  Relation between Age, operating_system and rating  --------------
pic <- Demogra_data[,c(2,3,10)]
pic <- aggregate(pic$rating, by=list(type=pic$Age, pic$operating_system),mean)
names(pic) <-c ("Age","operating_system", "Average_rating")
pic$operating_system <- factor(pic$operating_system)

ggplot(data=pic, mapping=aes(x = operating_system, y = Average_rating,fill=Age))+
        geom_bar(stat="identity",position=position_dodge(0.75))
# Linux is the least preferred

# -----------  Relation between Age, price and rating  --------------
pic <- Demogra_data[,c(2,8,10)]
pic <- aggregate(pic$rating, by=list(type=pic$Age, pic$price),mean)
names(pic) <-c ("Age","price", "Average_rating")
pic$price <- factor(pic$price)

ggplot(data=pic, mapping=aes(x = price, y = Average_rating,fill=Age))+
        geom_bar(stat="identity",position=position_dodge(0.75))
# students prefer chpear computers except those under 18
# but the number of people under 18 is not convincing enough

