library(lme4)
library(ggpubr)
library(sjPlot)
library(lattice)
source("spine_chart_utility.R")


##########################################
# step1. consider the data including     #
# both the Italian and Chinese market    #
##########################################

# load the data
df <- read.csv("Laptop_Research_Survey.csv")
df <- data.frame(df)
head(df)
str(df)
summary(df)

# convert the variable to factors 
for (i in c('operating_system',"RAM","price","storage","screen_size","battery","nationality","Education")){
  df[[i]] <- as.factor(df[[i]])
}

# fit linear regression
lin_reg <- lm(rating ~ operating_system + RAM + storage + screen_size + battery + price, data=df)
summary(lin_reg)
AIC(lin_reg) # 7720.239

#############################################
# step2. try to compute the part-worths and #
# plot the spine chart of student customers #
#############################################

# save key list elements of the fitted model as needed for conjoint measures

conjoint.results <-
  lin_reg[c("contrasts","xlevels","coefficients")]


conjoint.results$attributes <- names(conjoint.results$contrasts)
part.worths <- conjoint.results$xlevels  # list of same structure as xlevels
end.index.for.coefficient <- 1  # intitialize skipping the intercept
part.worth.vector <- NULL # used for accumulation of part worths

for(index.for.attribute in seq(along=conjoint.results$contrasts)) {
  nlevels <- length(unlist(conjoint.results$xlevels[index.for.attribute]))
  begin.index.for.coefficient <- end.index.for.coefficient + 1
  end.index.for.coefficient <- begin.index.for.coefficient + nlevels -2
  last.part.worth <- -sum(conjoint.results$coefficients[
    begin.index.for.coefficient:end.index.for.coefficient])
  part.worths[index.for.attribute] <-
    list(as.numeric(c(conjoint.results$coefficients[
      begin.index.for.coefficient:end.index.for.coefficient],
      last.part.worth)))
  part.worth.vector <-
    c(part.worth.vector,unlist(part.worths[index.for.attribute]))
} 

conjoint.results$part.worths <- part.worths

standardize <- function(x) {(x - mean(x)) / sd(x)}
conjoint.results$standardized.part.worths <-
  lapply(conjoint.results$part.worths,standardize)


# compute and store part-worth ranges for each attribute
part.worth.ranges <- conjoint.results$contrasts
for (index.for.attribute in seq(along=conjoint.results$contrasts))
  part.worth.ranges[index.for.attribute] <-
  dist(range(conjoint.results$part.worths[index.for.attribute]))
conjoint.results$part.worth.ranges <- part.worth.ranges

sum.part.worth.ranges <- sum(as.numeric(conjoint.results$part.worth.ranges))
# compute and store importance values for each attribute
attribute.importance <- conjoint.results$contrasts
for(index.for.attribute in seq(along=conjoint.results$contrasts))
  attribute.importance[index.for.attribute] <-
  (dist(range(conjoint.results$part.worths[index.for.attribute]))/
     sum.part.worth.ranges) * 100
conjoint.results$attribute.importance <- attribute.importance

# data frame for ordering attribute names
attribute.name <- names(conjoint.results$contrasts)
attribute.importance <- as.numeric(attribute.importance)
temp.frame <- data.frame(attribute.name,attribute.importance)
conjoint.results$ordered.attributes <-
  as.character(temp.frame[sort.list(
    temp.frame$attribute.importance,decreasing = TRUE),"attribute.name"])

# respondent internal consistency added to list structure
conjoint.results$internal.consistency <- summary(lin_reg)$r.squared

print.digits = 2

# user-defined function for printing conjoint measures
if (print.digits == 2)
  pretty.print <- function(x) {sprintf("%1.2f",round(x,digits = 2))}
if (print.digits == 3)
  pretty.print <- function(x) {sprintf("%1.3f",round(x,digits = 3))}


# report conjoint measures to console
# use pretty.print to provide nicely formated output
for (k in seq(along=conjoint.results$ordered.attributes)) {
  cat("\n","\n")
  cat(conjoint.results$ordered.attributes[k],"Levels: ",
      unlist(conjoint.results$xlevels[conjoint.results$ordered.attributes[k]]))
  
  cat("\n"," Part-Worths:  ")
  cat(pretty.print(unlist(conjoint.results$part.worths
                          [conjoint.results$ordered.attributes[k]])))
  
  cat("\n"," Standardized Part-Worths:  ")
  cat(pretty.print(unlist(conjoint.results$standardized.part.worths
                          [conjoint.results$ordered.attributes[k]])))
  
  cat("\n"," Attribute Importance:  ")
  cat(pretty.print(unlist(conjoint.results$attribute.importance
                          [conjoint.results$ordered.attributes[k]])))
}


pdf(file = "laptop_preference_results_new.pdf", width=8.5, height=11)
spine.chart(conjoint.results)
dev.off()  # close the graphics output device

############################################################################
# step3. multilevel linear model on rating and respondent level (2 levels )#
############################################################################

#### consider the rating as Level 1, respondent as Level 2
#### random intercept model
multilin_RI <- lmer(rating ~ operating_system + RAM + storage + screen_size + battery + price + (1 | resp_id), data=df)
summary(multilin_RI) # a lot of variance in how respondent use rating scale
vcov(multilin_RI) # compute the covariance matrix
lme4::VarCorr(multilin_RI) %>% print(comp = c("Variance", "Std.Dev"),digits = 2)
AIC(multilin_RI)  ## 7451.259

# confidence interval
tab_model(multilin_RI)
set.seed(10)
confint(multilin_RI, method="boot", boot.type="perc")
# fixed effects for intercept
fixef(multilin_RI)
# random effects of each respondent for intercept
ranef(multilin_RI)
dotplot(ranef(multilin_RI)) #dotplot
coef(multilin_RI)

# draw the kernel density and box plot
interc <- unlist(coef(multilin_RI)$resp_id[1]) # get intercept values
par(mfrow=c(1,2))
plot(density(interc), main="Kernel density") # kernel density
grid()
boxplot(interc, main="Box plot") # box plot
grid()
par(mfrow=c(1,1))

# compute the ICC
ICC=1.447/(1.447+3.494) # 0.2929
ICC*100 #29.29

# compute the level-1 ER-squared
NullModel <- lmer(rating ~ 1 + (1 | resp_id), data = df)
summary(NullModel)
R2_1 <- 1-(1.447+3.494)/(1.341+4.444)
R2_1 # 0.1458946


# access the normality of residual
qqnorm(resid(multilin_RI))  # sigma^2
qqline(resid(multilin_RI))

# access the normality of random effects of intercept
qqnorm(ranef(multilin_RI)$resp_id[[1]])
qqline(ranef(multilin_RI)$resp_id[[1]])

#### random slope model
### level 2 are the respondents
mlmRS <- lmer(rating ~ operating_system + RAM + storage + screen_size + battery + price + 
                (operating_system + RAM + storage + screen_size + battery + price | resp_id), 
              data=df, control = lmerControl(check.nobs.vs.nRE = "ignore"))

summary(mlmRS)
tab_model(mlmRS) 
AIC(mlmRS)  # 7344.585
head(ranef(mlmRS)$resp_id) # check the random effect of each respondent 

# plot random effects
dotplot(ranef(mlmRS), cex=0.4) # heterogeneity in operating_system, RAM, battery, price


### visualization of the distribution of different attributes
# visualizing RAM
RAM8 <- coef(mlmRS)$resp_id$RAM8
RAM16 <- coef(mlmRS)$resp_id$RAM16
par(mfrow=c(1,2))
hist(RAM8, xlim=c(-4,4), ylim=c(0,50), main="RAM 8GB vs RAM 4GB", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()
hist(RAM16, xlim=c(-4,4), ylim=c(0,50), main="RAM 16GB vs RAM 4GB", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()

# visualizing battery 
battery8 <- coef(mlmRS)$resp_id$battery8
battery12 <- coef(mlmRS)$resp_id$battery12
par(mfrow=c(1,2))
hist(battery8, xlim=c(-4,4), ylim=c(0,50), main="Battery 8h vs Battery 4h", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()
hist(battery12, xlim=c(-4,4), ylim=c(0,50), main="Battery 12h vs Battery 4h", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()

# visualizing price 
par(mfrow=c(1,2))
price1000 <- coef(mlmRS)$resp_id$price1000
price1500 <- coef(mlmRS)$resp_id$price1500
hist(price1000, xlim=c(-4,4), ylim=c(0,50), main="1000Euro vs 500Euro", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()
hist(price1500, xlim=c(-2,2), ylim=c(0,50), main="1500Euro vs 500Euro", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()




# visualizing operating system 
windows <- coef(mlmRS)$resp_id$operating_systemWindows
macos <- coef(mlmRS)$resp_id$operating_systemMacOS
par(mfrow=c(1,2))
hist(windows, xlim=c(-4,4), ylim=c(0,50), main="Windows vs Linux", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()
hist(macos, xlim=c(-4,4), ylim=c(0,50), main="MacOS vs Linux", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()

# visualizing storage ### this is a point
par(mfrow=c(1,2))
storage512 <- coef(mlmRS)$resp_id$storage512  ### 256 is little more popular than 512GB
storage1024 <- coef(mlmRS)$resp_id$storage1024
hist(storage512, xlim=c(-4,4), ylim=c(0,50), main="storage 512GB vs storage 256GB ", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()
hist(storage1024, xlim=c(-2,2), ylim=c(0,50), main="storage 1024GB vs storage 256GB ", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()

# visualizing price ## point
par(mfrow=c(1,2))
price1000 <- coef(mlmRS)$resp_id$price1000
price1500 <- coef(mlmRS)$resp_id$price1500
hist(price1000, xlim=c(-4,4), ylim=c(0,50), main="price 1000Euro vs price 500Euro", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()

hist(price1500, xlim=c(-4,4), ylim=c(0,50), main="price 1500Euro vs price 500Euro", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()  # people can accept the price 1000Euro, but obvious be negative to price 1500Euro



##########################################################################################
# step4. multilevel linear model on rating, respondent and nationality level (3 levels ) #
##########################################################################################

#### random intercept model
### level 3 are nationalities
multilin_RI_2 <- lmer(rating ~ operating_system + RAM + storage + screen_size + battery + price + (1 | nationality/resp_id), data=df)
summary(multilin_RI_2)
vcov(multilin_RI_2)

lme4::VarCorr(multilin_RI_2) %>% print(comp = c("Variance", "Std.Dev"),digits = 3)
AIC(multilin_RI_2) ## 7404.505

# compute the ICC
ICC_2 = (1.005+1.514)/(1.005+1.514+3.494)
ICC_2*100

# compute the level-1 ER-squared
NullModel_1 <- lmer(rating ~ 1 + (1 | nationality/resp_id), data = df)
summary(NullModel_1)
R2_1_1 <- 1- (1.005+1.514+3.494)/(0.8993+1.5139+4.4444)
R2_1_1 # 0.1232


# access the normality of residual
qqnorm(resid(multilin_RI_2))  # sigma^2
qqline(resid(multilin_RI_2))

# access the normality of random effects of intercept
qqnorm(ranef(multilin_RI_2)$resp_id[[1]])
qqline(ranef(multilin_RI_2)$resp_id[[1]])


#### random slope model
mlmRS_2 <- lmer(rating ~ operating_system + RAM + storage + screen_size + battery + price + 
                  (operating_system + RAM + storage + screen_size + battery + price | nationality/resp_id), 
                data=df, control = lmerControl(check.nobs.vs.nRE = "ignore"))
summary(mlmRS_2)
AIC(mlmRS_2) # 7477.759
tab_model(mlmRS_2)
head(ranef(mlmRS_2)$resp_id) # check the random effect of each respondent 

# plot random effects
dotplot(ranef(mlmRS_2), cex=0.4)

## histgram # price
par(mfrow=c(1,2))
price1000_2 <- coef(mlmRS_2)$resp_id$price1000
price1500_2 <- coef(mlmRS_2)$resp_id$price1500
hist(price1000_2, xlim=c(-4,4), ylim=c(0,50), main="1000Euro vs 500Euro", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()
hist(price1500_2, xlim=c(-2,2), ylim=c(0,50), main="1500Euro vs 500Euro", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()

# storage
par(mfrow=c(1,2))
storage512_2 <- coef(mlmRS_2)$resp_id$storage512  ### 256 is little more popular than 512GB
storage1024_2 <- coef(mlmRS_2)$resp_id$storage1024
hist(storage512_2, xlim=c(-4,4), ylim=c(0,50), main="storage 512GB vs storage 256GB ", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()
hist(storage1024_2, xlim=c(-2,2), ylim=c(0,50), main="storage 1024GB vs storage 256GB ", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()



##########################################################################################
# step5. multilevel linear model on rating, respondent and education level (3 levels ) #
##########################################################################################


#### random intercept model
### level 3 are education
multilin_RI_3 <- lmer(rating ~ operating_system + RAM + storage + screen_size + battery + price + (1 | Education/resp_id), data=df)
summary(multilin_RI_3)
vcov(multilin_RI_3)

lme4::VarCorr(multilin_RI_3) %>% print(comp = c("Variance", "Std.Dev"),digits = 3)
AIC(multilin_RI_3) ## 7447.435

# compute the ICC
ICC_3 = (1.3482+0.3177)/(1.3482+0.3177+3.4936)
ICC_3*100

# compute the level-1 ER-squared
NullModel_2 <- lmer(rating ~ 1 + (1 | Education/resp_id), data = df)
summary(NullModel_2)
R2_1_2 <- 1-(1.3482+0.3177+3.4936)/(1.2425+0.3177+4.4444)
R2_1_2 # 0.1407

#### random slope model
mlmRS_3 <- lmer(rating ~ operating_system + RAM + storage + screen_size + battery + price + 
                  (operating_system + RAM + storage + screen_size + battery + price | Education/resp_id), 
                data=df, control = lmerControl(check.nobs.vs.nRE = "ignore"))
summary(mlmRS_3)
AIC(mlmRS_3) 
tab_model(mlmRS_3)
head(ranef(mlmRS_3)$resp_id) # check the random effect of each respondent 

# plot random effects
dotplot(ranef(mlmRS_3), cex=0.4)

## histgram # price
par(mfrow=c(1,2))
price1000_3 <- coef(mlmRS_3)$resp_id$price1000
price1500_3 <- coef(mlmRS_3)$resp_id$price1500
hist(price1000_3, xlim=c(-4,4), ylim=c(0,50), main="1000Euro vs 500Euro", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()
hist(price1500_3, xlim=c(-2,2), ylim=c(0,50), main="1500Euro vs 500Euro", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()

# storage
par(mfrow=c(1,2))
storage512_3 <- coef(mlmRS_3)$resp_id$storage512  ### 256 is little more popular than 512GB
storage1024_3 <- coef(mlmRS_3)$resp_id$storage1024
hist(storage512_3, xlim=c(-4,4), ylim=c(0,50), main="storage 512GB vs storage 256GB ", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()
hist(storage1024_3, xlim=c(-2,2), ylim=c(0,50), main="storage 1024GB vs storage 256GB ", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()














