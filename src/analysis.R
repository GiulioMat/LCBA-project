library(lme4)
library(ggpubr)
library(sjPlot)
library(lattice)
library(gridExtra)

# load data
df <- read.csv("Laptop_Research_Survey.csv")

# convert variables as qualitative
for(i in c('RAM', 'storage', 'screen_size', 'battery', 'price')){
  df[[i]] <- as.factor(df[[i]])
}

str(df)
summary(df)



# set options to obtain sum contrast
backup_options <- options()
options(contrasts=c("contr.sum","contr.poly"))

# fit linear regression model
lin_reg <- lm(rating ~ operating_system + RAM + storage + screen_size + battery + price, data=df)
summary(lin_reg)

# function to plot part-worths
plot_pw <- function(model){
  part_worths <- data.frame(type=character(), level=character(), pw=double(), std_pw=double())
  
  # calculate part-worths
  index = 1
  for(i in names(model$xlevels)){
    index <- index+1
    x1 <- model$coefficients[index]
    index <- index+1
    x2 <- model$coefficients[index]
    x3 <- -sum(x1, x2)
    
    mean <- mean(c(x1, x2, x3))
    sd <- sd(c(x1, x2, x3))
    std_x1 <- (x1-mean)/sd
    std_x2 <- (x2-mean)/sd
    std_x3 <- (x3-mean)/sd
    
    part_worths <- rbind(data.frame(type=i, level=paste0(i,"_",model$xlevels[[i]][1]), pw=x1, std_pw=std_x1), part_worths)
    part_worths <- rbind(data.frame(type=i, level=paste0(i,"_",model$xlevels[[i]][2]), pw=x2, std_pw=std_x2), part_worths)
    part_worths <- rbind(data.frame(type=i, level=paste0(i,"_",model$xlevels[[i]][3]), pw=x3, std_pw=std_x3), part_worths)
  }
  row.names(part_worths) <- NULL
  
  # calculate importance of attributes
  importance <- aggregate(abs(part_worths$pw), by=list(attributes=part_worths$type), FUN=sum)
  importance$x <- round((importance$x/sum(importance$x))*100, 3)
  importance <- importance[order(importance$x, decreasing = TRUE),]
  importance$x <- paste(importance$x, '%')
  names(importance)[2] <- "importance"
  
  # plot standardized part-worths and importances
  plot <- ggdotchart(part_worths, x="level", y="std_pw", color="type", rotate = TRUE, 
                     add = "segments", group = "type", sorting = "ascending", dot.size=3, size=1) + 
    geom_hline(yintercept = 0, color = "lightgray") +
    labs(title="Spine chart", x ="Levels", y = "Standardized Part-Worths", color = "Attributes") +
    theme(plot.title = element_text(hjust = 0.5))
  
  grid.arrange(tableGrob(importance,rows=NULL, theme = ttheme_default(base_size = 20)), plot, ncol=2)
}

plot_pw(lin_reg) # generally prefer everything big and MacOS, don't like 1500 as price


# set options back to normal
options(backup_options)

# fit random intercept multilevel linear model
multilin_RI <- lmer(rating ~ operating_system + RAM + storage + screen_size + battery + price + (1 | resp_id), data=df)
summary(multilin_RI) # a lot of variance in how respondent use rating scale
tab_model(multilin_RI)

multilin_RI_2 <- lmer(rating ~ operating_system + RAM + storage + screen_size + battery + price + (1 | nationality/resp_id), data=df)
summary(multilin_RI_2)
tab_model(multilin_RI_2)

AIC(lin_reg)
AIC(multilin_RI) 
AIC(multilin_RI_2) # a bit better model with also nationality

# plot_model(multilin_RI, vline.color = "red", show.values = TRUE)

# visualize the distibution of the differing respondent intercepts
interc <- unlist(coef(multilin_RI_2)$resp_id[1]) # get intercept values
par(mfrow=c(1,2))
plot(density(interc), main="Kernel density") # kernel density
grid()
boxplot(interc, main="Box plot") # box plot
grid()
par(mfrow=c(1,1))

# compute the intraclass correlation coefficient
ICC <- (1.994/(1.994+3.375))*100 # 37% of variance explained by model with resp_id
ICC_2 <- ((1.571+1.57)/(1.571+1.571+3.375))*100 # 48% of variance explained by model with nationality/resp_id



# fit random slope multilevel linear model
mlmRS <- lmer(rating ~ operating_system + RAM + storage + screen_size + battery + price + 
                (operating_system + RAM + storage + screen_size + battery + price | resp_id), 
              data=df, control = lmerControl(check.nobs.vs.nRE = "ignore"))
summary(mlmRS)
tab_model(mlmRS) # high variance in operating_system, RAM, battery, price

mlmRS_2 <- lmer(rating ~ operating_system + RAM + storage + screen_size + battery + price + 
                (operating_system + RAM + storage + screen_size + battery + price | nationality/resp_id), 
              data=df, control = lmerControl(check.nobs.vs.nRE = "ignore"))
summary(mlmRS_2)
tab_model(mlmRS_2)

AIC(mlmRS) # better model with just resp_id
AIC(mlmRS_2)

# plot random effects
dotplot(ranef(mlmRS), cex=0.4) # heterogeneity in operating_system, RAM, battery, price

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
price1000 <- coef(mlmRS)$resp_id$price1000
price1500 <- coef(mlmRS)$resp_id$price1500
par(mfrow=c(1,2))
hist(price1000, xlim=c(-4,4), ylim=c(0,50), main="1000$ vs 500$", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()
hist(price1500, xlim=c(-4,4), ylim=c(0,50), main="1500$ vs 500$", ylab="Respondents frequency", 
     xlab="Difference in rating")
grid()

par(mfrow=c(1,1))


