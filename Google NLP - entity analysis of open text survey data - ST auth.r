##
## Google Natural Language Processing
##

# DESCRIPTION: The following code uses Google NLP to analyse open text responses from a survey dataset

# For info on R google auth see https://cran.r-project.org/web/packages/googleAuthR/vignettes/google-authentication-types.html
# Google Natural Language API https://cran.rstudio.com/web/packages/googleLanguageR/vignettes/nlp.html
# Natural Language API Basics: https://cloud.google.com/natural-language/docs/basics#sentiment-analysis-values
# Google NLP for R http://code.markedmondson.me/googleLanguageR/
# http://code.markedmondson.me/googleLanguageR/


## The google language R wrap around function is structured like so:
#gl_nlp(string,
#       nlp_type = c("annotateText", "analyzeEntities",
#                    "analyzeSentiment", "analyzeSyntax", 
#                    "analyzeEntitySentiment"),
#       type = c("PLAIN_TEXT", "HTML"),
#       language = c("en", "zh", "zh-Hant", "fr","de", "it", "ja", "ko", "pt", "es"), 
#       encodingType = c("UTF8", "UTF16","UTF32", "NONE"))


# 1. Authenticate - You need Google Cloud, NLP API enabled, and a client secret certificate from your account
#install.packages('googleLanguageR')
library(googleLanguageR)
gl_auth('SimonT-45cc1417ef7a.json')


# 2. Load libraries
library(tidyr)
library(dplyr)
library(data.table)
# library(googleAuthR)
# library(googleAnalyticsR)
# library(searchConsoleR)


# 3. Get dataset
inquiryone <- read.csv('inquiry1.csv')


# 4. Combine text - here I combine columns with open text answers together - so text from each respondent is located in one cell
inquiryone$wholetext <- paste(inquiryone[ , 5])
for (i in 6:52) {
  
  inquiryone$wholetext <- paste(inquiryone$wholetext, inquiryone[ , i], sep = ". ")
  
}

#Check the text for the first repondent
inquiryone$wholetext[1]

#Get the column number for the 'whole text'
grep("wholetext", colnames(inquiryone))


# 5. create an 'Entity Function' to analyse entities in each response
thematithizer <- function(dataset, columnnumber, salience_level = 0.01) {
  
  #Set up dataframe
  Responses <- data.frame()
  
  #Begin loop
  for (i in 1:nrow(dataset)) {
    
    #Get text of question
    text <- dataset[i, columnnumber] %>% as.character()
    
    #Analyze string
    if(nchar(text) > 100) {
      entities <- gl_nlp(text,
                         nlp_type = c("analyzeEntitySentiment"),
                         type = c("PLAIN_TEXT"))
      
      #Get only dataset of entities
      entities <- as.data.frame(entities$entities)
      
      #Remove NAs
      entities <- entities[which(entities$type != '<NA>'), ]
      entities <- entities[which(entities$salience > salience_level), ] #Can change salience, the higher the less characters are included
      
      if (nrow(entities) > 0) { 
        
        #Add Response number
        entities$Respondent.Name <- i
        
        #Form dataframe
        Responses <- rbind(Responses, entities)
        
      }
      
    } else {
      
      next
      
    }
    
  }  
  
  return(Responses)
}



# 6. Use the function
Outputdataset <- thematithizer(inquiryone , 96, salience_level = 0.01)


# If you want, add the respondent name from the original survey dataset into the output dataset
RespondentName <- inquiryone
RespondentName$Respondent.Name <- 1 #Create a new column with the respondent number
for (i in 1: nrow(RespondentName)) {RespondentName$Respondent.Name[i] <- i} #Put in all the respondent numbers
RespondentName <- RespondentName[ , c("School.Name", "Respondent.Name")] #Keep only the name and number columns
Outputdataset <- merge(RespondentName, Outputdataset, by = "Respondent.Name", all = TRUE) #Merged entities with School names


# 7. Write data to csv
write.csv(Outputdataset, 'responsetest.csv')


## NOTE: The output doesn't deal with capitalisation or pluralisation on the entities identified
## The follow code comes from the text mining package in R
## It puts every thing in lower case, stems the terms, then completes them using a dictionary of terms


# R text mining libraries
library(tm)
library(SnowballC)


# NOTE: The following function is wiping some of the terms, so you receive 'NA' instead of the term
# I'm working on trying to fix this


# 8. Create a stemming function
stem_entities <- function(dataset, column_header) {
  
  #Get dictionary for stem completion
  dictionary = as.vector(dataset[, column_header]) #Doesn't have to be lower case, except some terms remain capitalised otherwise
  
  for (i in 1:nrow(dataset)) {
    
    # 1. Get 'corpus' - i.e. columns with text in a dataframe (only input the relevant columns)
    docs <- dataset[i, column_header] #Gets text from a dataframe
    
    # 2.Clean data
    docs <- tolower(docs) # Convert to lower case
    docs <- wordStem(docs, language = "en") #Stem document
    docs <- stemCompletion(docs, dictionary, type = "first") #Recomplete stems into words
    
    # 3. Add word back into dataset
    dataset[i, column_header] <- docs
    
  }
  
  #return dataset
  return(dataset)
  
}


# 9. Stem the entity column
Outputdataset2 <- stem_entities(Outputdataset, "name")

# 10. Write data to file
write.csv(Outputdataset2, 'responsetest2.csv')


