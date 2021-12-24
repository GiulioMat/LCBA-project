# Fraction factorial (orthogonal) design (for traditional conjoint survey)

library(support.CEs) #package containing functions to implement
# attrib <- list(color = c("black", "white", "silver"), #list of attributes with levels 
#                RAM = c("-8", "8", "16", "32", "32+"),
#                operating_system = c("Windows", "Linux", "MacOS"),
#                screen_size = c("13.3", "14", "15.6"),
#                storage = c("128", "256", "526", "1TB", "1TB+"),
#                weight = c("-2", "2", "2-3", "3+"),
#                battery = c("3-4", "4-6", "6-8", "8+"))

# attrib <- list(operating_system = c("Windows", "Linux", "MacOS"),
#                RAM = c("4GB", "8GB", "16GB"),
#                storage = c("256GB", "526GB", "1TB"),
#                screen_size = c("13.3\"", "14\"", "15.6\""),
#                battery = c("4h", "8h", "12h"),
#                price= c("500$", "1000$", "1500$"))

attrib <- list(operating_system = c("Windows", "Linux", "MacOS"),
               RAM = c("4", "8", "16"),
               storage = c("256", "526", "1000"),
               screen_size = c("13.3", "14", "15.6"),
               battery = c("4", "8", "12"),
               price= c("500", "1000", "1500"))

#We can use the Lma.design() function and set nalternatives=1  
FFtradSurvey <- Lma.design(attribute.names=attrib, #specification of the attributes
                           nalternatives=1, #number of alternatives per question
                           nblocks=2, #number of subsets of question
                           seed=999) #to specify the random number seed 
FFtradSurvey 

questionnaire(FFtradSurvey) #create the questionnaire format

# The questionnaire can be send to an external file, such as a txt,
# using the function sinc()
setwd("...") #set the destination directory
sink("questions_for_FF_trad_Survey.txt")  # send survey to external text file
questionnaire(FFtradSurvey)
sink() # send output back to the screen

# Organizing the questionnaire in a dataframe format
FFtradSurveyDF <- FFtradSurvey$alternatives[[1]]
FFtradSurveyDF
write.csv(FFtradSurveyDF, file="alternatives.csv")
