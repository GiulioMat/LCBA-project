library(lme4)
library(sjPlot)
library(ggplot2)

# load data
df <- read.csv("Laptop_Research_Survey.csv")

# convert variables as qualitative
for(i in c('RAM', 'storage', 'screen_size', 'battery', 'price')){
  df[[i]] <- as.factor(df[[i]])
}

str(df)
summary(df)

# fit linear regression model
lin_reg <- lm(rating ~ operating_system + RAM + storage + screen_size + battery + price, data=df)
summary(lin_reg) # prefer everything big and MacOS, don't like 1500 as price

# function to plot part-worths
plot_pw <- function(model){
  part_worths <- data.frame(type=character(), level=character(), pw=double())
  
  index = 1
  for(i in names(model$xlevels)){
    index <- index+1
    x1 <- model$coefficients[index]
    index <- index+1
    x2 <- model$coefficients[index]
    mean <- mean(c(0, x1, x2))
    sd <- sd(c(0, x1, x2))
    x0 <- (0-mean)/sd
    x1 <- (x1-mean)/sd
    x2 <- (x2-mean)/sd
    part_worths <- rbind(data.frame(type=i, level=paste0(i,"_",model$xlevels[[i]][1]), pw=x0), part_worths)
    part_worths <- rbind(data.frame(type=i, level=paste0(i,"_",model$xlevels[[i]][2]), pw=x1), part_worths)
    part_worths <- rbind(data.frame(type=i, level=paste0(i,"_",model$xlevels[[i]][3]), pw=x2), part_worths)
  }
  row.names(part_worths) <- NULL
  
  ggdotchart(pw, x="level", y="pw", color="type", rotate = TRUE, 
             add = "segments", group = "type", sorting = "ascending", dot.size=3, size=1) + 
    geom_hline(yintercept = 0, color = "lightgray") +
    labs(title="Spine chart", x ="Levels", y = "Standardized Part-Worths", color = "Attributes") +
    theme(plot.title = element_text(hjust = 0.5))
  
}

# fit random intercept multilevel linear model
multilin_RI <- lmer(rating ~ operating_system + RAM + storage + screen_size + battery + price + (1 | resp_id), data=df)
summary(multilin_RI) # a lot of variance in how respondent use rating scale

plot_model(multilin_RI, vline.color = "red", show.values = TRUE)

multilin_RI_2 <- lmer(rating ~ operating_system + RAM + screen_size + battery + price + (1 | resp_id), data=df)
anova(multilin_RI, multilin_RI_2) # the full model is better than the restricted one

# visualize the distibution of the differing respondent intercepts
interc <- unlist(coef(multilin_RI)$resp_id[1]) # get intercept values
par(mfrow=c(1,2))
plot(density(interc), main="Kernel density") # kernel density
grid()
boxplot(interc, main="Box plot") # box plot
grid()
par(mfrow=c(1,1))

# compute the intraclass correlation coefficient
ICC <- (1.994/(1.994+3.375))*100 # 37% of variance is explained by different use of rating scale

# compute the Level-1 R-squared
NullModel <- lmer(rating ~ 1 + (1 | resp_id), data=df)
summary(NullModel)
R2_1 <- 1-((3.375+1.994)/(4.201+1.902)) # 0.12 (?)

# fit random slope multilevel linear model
mlmRS <- lmer(rating ~ operating_system + RAM + storage + screen_size + battery + price + 
                (operating_system + storage + battery  | resp_id), data=df)
summary(mlmRS)
dotplot(ranef(mlmRS), cex=0.4)

windows <- coef(mlmRS)$resp_id$operating_systemWindows
macos <- coef(mlmRS)$resp_id$operating_systemMacOS
par(mfrow=c(1,2))
hist(windows, xlim=c(-4,4), ylim=c(0,50), main="Windows vs Linux", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()
hist(macos, xlim=c(-4,4), ylim=c(0,50), main="MacOS vs Linux", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()
par(mfrow=c(1,1))

