library("reshape2")
setwd("D:/software/R Files/Business and Customer Analytics/Final Projects")
###################
# Italian survey
###################

# load survey data
res <- read.csv('Laptop_Research_Survey_ita.csv', sep=';')
res_data <- res[, c(1, 6:14)] # get important columns
res_data <- melt(res_data, id.vars="Num") # format from wide to long

# load alternatives data
alt <- read.csv('alternatives.csv')
alt <- alt[alt$BLOCK==1,][,5:10] # get alternatives of block 1
n_resp <- max(res$Num)
alt_rep <- alt[rep(1:9, each = n_resp),] # repeat each alternative n_resp times

# merge two dataframes
df_res_1 <- cbind(res_data, alt_rep) 
row.names(df_res_1) <- NULL # reset row index number
df_res_1 <- df_res_1[,-2] # remove laptop variables

# rename columns 
names(df_res_1)[1] <- "resp_id"
names(df_res_1)[2] <- "rating"

# Add Demographic information to the table
add_info_1 <- res[, c(1, 2:5)]
# add_info_1_rep <- for i in 9
temp_1 <- rbind(add_info_1, add_info_1) 
temp_1 <- rbind(temp_1, temp_1)
temp_1 <- rbind(temp_1, temp_1)
temp_1 <- rbind(temp_1, add_info_1)
df_res_1 <- cbind(df_res_1,temp_1)
df_res_1 <- df_res_1[,-9]

###################
# Chinese survey
###################

# load survey data
res <- read.csv('Laptop_Research_Survey_chn.csv', sep=';')
res_data <- res[, c(1, 6:14)] # get important columns
res_data <- melt(res_data, id.vars="Num") # format from wide to long

# load alternatives data
alt <- read.csv('alternatives.csv')
alt <- alt[alt$BLOCK==2,][,5:10] # get alternatives of block 2
n_resp <- max(res$Num)
alt_rep <- alt[rep(1:9, each = n_resp),] # repeat each alternative n_resp times

# merge two dataframes
df_res_2 <- cbind(res_data, alt_rep)
row.names(df_res_2) <- NULL # reset row index number
df_res_2 <- df_res_2[,-2] # remove question variables

# rename columns 
names(df_res_2)[1] <- "resp_id"
names(df_res_2)[2] <- "rating"

# Add Demographic information to the table
add_info_2 <- res[, c(1, 2:5)]
# add_info_1_rep <- for i in 9
temp_2 <- rbind(add_info_2, add_info_2) 
temp_2 <- rbind(temp_2, temp_2)
temp_2 <- rbind(temp_2, temp_2)
temp_2 <- rbind(temp_2, add_info_2)
df_res_2 <- cbind(df_res_2,temp_2)
df_res_2 <- df_res_2[,-9]



# scale resp.id by num respondents of italian survey
df_res_2$resp_id <- df_res_2$resp_id + max(df_res_1$resp_id) 


###################
# Merge surveys
###################

df_res <- rbind(df_res_1, df_res_2)

df_res$Age[df_res$Age=="&lt;18"] <- 18

# save final dataframe as .csv
write.csv(df_res, file="Demographic_data.csv", row.names=FALSE)

