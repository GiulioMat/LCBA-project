library(ggplot2)

setwd("D:/software/R Files/Business and Customer Analytics/Final Projects")
Demogra_data <- read.csv('Demographic_data.csv')

Demogra_data$Age <- factor(Demogra_data$Age, levels = c("18", "18-20", "20-23", "24-26", "27-30", ">30"))
Demogra_data$Gender <- factor(Demogra_data$Gender, levels = c("male", "female", "prefer not to say"))
Demogra_data$Education <- factor(Demogra_data$Education)
Demogra_data$Work. <- factor(Demogra_data$Work.)

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

# ----------------------------------
# fit logistic regression model 
# predicted response using 0.5 cut-off
# ----------------------------------
mean(as.numeric(as.character(Demogra_data$rating)))

Demogra_data$predicted_response <- 0
Demogra_data$predicted_response[Demogra_data$rating <= 5] <- "Negative"
Demogra_data$predicted_response[Demogra_data$rating > 5] <- "Positive"

with(Demogra_data, table(Demogra_data$rating, Demogra_data$predicted_response))  # check definition 

Demogra_data$predicted_response <- factor(Demogra_data$predicted_response)
Demogra_fit <- glm(predicted_response ~ Age + Gender + Education + Work., 
                   data = Demogra_data, family=binomial)

summary(Demogra_fit)
anova(Demogra_fit, test="Chisq")
