# Fraction factorial (orthogonal) design (for traditional conjoint survey)

library(support.CEs) #package containing functions to implement

# attrib <- list(operating_system = c("Windows", "Linux", "MacOS"),
#                RAM = c("4GB", "8GB", "16GB"),
#                storage = c("256GB", "512GB", "1024GB"),
#                screen_size = c("13.3\"", "14\"", "15.6\""),
#                battery = c("4h", "8h", "12h"),
#                price= c("500$", "1000$", "1500$"))

attrib <- list(operating_system = c("Windows", "Linux", "MacOS"),
               RAM = c("4", "8", "16"),
               storage = c("256", "512", "1024"),
               screen_size = c("13.3", "14", "15.6"),
               battery = c("4", "8", "12"),
               price= c("500", "1000", "1500"))

# use the Lma.design() function and set nalternatives=1  
FFtradSurvey <- Lma.design(attribute.names=attrib, # specification of the attributes
                           nalternatives=1, # number of alternatives per question
                           nblocks=2, # number of subsets of question
                           seed=999) # specify the random number seed 
FFtradSurvey 

questionnaire(FFtradSurvey) #create the questionnaire format

setwd("...") #set the destination directory
sink("questions_for_FF_trad_Survey.txt")  # send survey to external text file
questionnaire(FFtradSurvey)
sink() # send output back to the screen

# organizing the questionnaire in a dataframe format
FFtradSurveyDF <- FFtradSurvey$alternatives[[1]]
FFtradSurveyDF
write.csv(FFtradSurveyDF, file="alternatives.csv")
