##### Purpose of code: #####
  # Understand, prep, & clean data to prepare for modeling

##### Setting up environment #####

# loading libraries
library(tidyverse) 

# set working directory
setwd("C:\\Users\\jessa\\OneDrive\\Documents\\MASTERclasses\\GitHub\\NHANES_13-14\\")

#__________________________________________________________________________________________________________

# reading in datasets:
  # read_csv is a more thorough function for reading in csv data
  # it is part of the tidyverse we loaded above
  # in order to figure out what type of data each column is
  # read_csv reads in 1000 rows to make a decision
  # sometimes this isn't enough rows to give a full picture of our data
  # and in that case we can adjust this to the length we'd like with guess_max
  # Since none of our datasets are too long - I have provided the full length of the dataset
  # in order get the best picture of our data

# 1. demographics
demo <- read_csv("demographic.csv", guess_max = 10175)

# 2. labs
labs <- read_csv("labs.csv", guess_max = 9813)

# 3. questionnaire
quest <- read_csv("questionnaire.csv", guess_max = 10175)

# Note: this data comes from NHANES & there is additional data available 
  # However for the scope of this project 
  # only demographic data and self-reported data were used to predict b12

#__________________________________________________________________________________________________________

##### Reducing Columns #####

# There is A LOT of data in these datasets
  # However, upon inspection of the datasets - there are columns which contain similar information
  # or information which is only available for a subset of respondents
  # (Ex: education level for only children k-12, with a different education marker for adults)
  # For this data especially it's really important we perform the most important part of data analysis: 
  # understand your data!!!
  # I've gone ahead and looked through demographic data and questionnaire data to identify the most useful columns
  # For more complete information on which variables where chosen see the provided data dictionary


# Here we'll be reducing the number of columns before joining everything together

# reducing columns in data important variables for demographic data
demouse <- demo %>% select("SEQN", "RIAGENDR", "RIDAGEYR", "DMDBORN4", "RIDRETH3", 
                                 "DMQMILIZ", "DMDMARTL", "RIDEXPRG", "DMDHHSZA", "DMDHHSZB","INDHHIN2")

# reducing columns in data important variables for questionaire data                          
questuse <- quest %>% select("SEQN", "ALQ120U", "ALQ130", "BPQ020", "BPQ080",
                             "HSD010", "DIQ010", "DBD910", "DLQ010", "DLQ020",
                                 "DLQ040", "DLQ050", "DLQ060", "DLQ080", "FSD032A",
                                 "HIQ011", "HUQ090", "HOQ065", "MCQ220", "MCQ160M",
                                 "MCQ160E", "MCQ160C", "OHQ030", "RXQ510", "SLD010H",
                                 "SMD650", "SMD470", "WHD010", "WHD020")

#__________________________________________________________________________________________________________

##### Joining Datasets #####


# join 1 - demographic data & questionnaire - this will be our potential predictor variables set
full <- demouse %>% 
  inner_join(questuse, by = "SEQN")

# join 2 - full & target variable LBDB12 & LBD12SI (B12 levels) from lab data
full <- full %>% 
  inner_join(select(labs, "LBDB12", "SEQN"), by = "SEQN")

# 9813 observations - okay it looks like on our last join we lost a few individuals,
  # (respondents who filled out demographic and questionnaire information
  # but didn't participate in lab work) - that's okay!

#__________________________________________________________________________________________________________

##### Creating Target Variable #####

# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/VITB12_H.htm

# We have 9813 records - wooo!
  # However, in order to predict our target variable B12 - we can ONLY use records
  # to build our model which contain our variable of interest.
  # Therefore we'll have to reduce our dataset down.

# removing missing values
b12data <- full %>% drop_na(LBDB12)

# okay great we have 5316 observations with this variable available!

# In order to identify if an individual is low/deficient in B12 we will create a binary marker
  # Information on b12 level cutoffs were determined from the following websites
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2781043/
  # https://www.webmd.com/a-to-z-guides/vitamin-b12-test#2
  # Below 200 is considered deficient with values between 200 - 300 being considered borderline
  # For the purposes of this model we will consider borderline & low individuals as "low"
  # Coded as 1 =low/borderline
  #          0 = within or above normal range

# Creating binary target & removing continous b12 variable
b12data <- b12data %>% 
  mutate(b12low = case_when(LBDB12 < 300 ~ 1,
                            LBDB12 >= 300 ~ 0)) %>% 
  select(-c(LBDB12))

#__________________________________________________________________________________________________________

##### Investigating Missing Data #####

# Now that we've identified variables we would like to use,
  # It's important to know what our data looks like.
  # We'll do some basic descriptive stats & investigate missing values
  # To start we'll compile information on how many missing values for each category

# For some models (like a decision tree) missing data is no problem!
  # For others (like a logistic regression) this can become an issue. 
  # Logistic regression will remove observations including with any predictor variable missing
    # this can start to add up and reduce your data quite a bit
  # BUT depending on *how much* data is missing we do have options:
    # - we can impute data (replace with "most" common value)
    # - or transform data into categorical variables (including missing)


# In order to identify HOW much of our data is missing for each variable we can create a for loop
  # This for loop goes through all of the columns of data & counts the missing values for that column
  # It then outputs this into a table which we can calculate % missing

# For loop to find count of missing values for all the variables 
for (i in 1:ncol(b12data)){
  if ( i == 1){
    results <- data.frame(missing = 0, stringsAsFactors = FALSE)
  } else {
    missvar <- sum(is.na(b12data[,i]))
    results <- rbind(results,missvar)
    i <- i + 1
  }
}

# Creating data frame with all the variable names
col <- as.data.frame(colnames(b12data))

# Merging column names with missing values
results <- cbind(col, results)

# Dividing the number missing by the number of observations gives us percent missing
results$percent_missing <- results$missing / nrow(b12data)

# Now we can use this percent missing to sort our data
varsbymiss <- results %>% arrange(percent_missing)

#________________________________________________________________________________________________________

##### Imputing, Binning, & Recoding certain variables ######

# We can see in our list of percent missing that there are a few variables which need additional attention

# Pregnancy _____________________________________________________________________________________________
  # Particularly variable RIDEXPRG, which represents if the participant is currently pregnant
  # Pregnancy can affect levels of B12 and could be very important in our analysis
  # ~70% are missing - but we would expect that ~50% would be missing (due to gender)
  # There are still some missing which we don't expect - but let's see what we can do to
  # with this variable to give the most accurate information.

# Recode:
  # If male, then pregnant = not applicable
  # If female & pregnant = pregnant
  # If female & not pregnant = not pregnant
  # If female & unknown = unknown
  # If female & missing = unknown
  # & remove old variable for pregnancy

b12data <- b12data %>%
  mutate(pregnant = case_when(RIAGENDR == 1 ~ "not applicable",
                              RIAGENDR == 2 & RIDEXPRG == 1 ~ "pregnant",
                              RIAGENDR == 2 & RIDEXPRG == 2 ~ "not pregnant",
                              RIAGENDR == 2 & RIDEXPRG == 3 ~ "unknown",
                              RIAGENDR == 2 & is.na(RIDEXPRG) ~ "unknown")) %>% 
  select(-c(RIDEXPRG))

# Smoking ______________________________________________________________________________________________
  # There are a lot of missing values for the variable SMD650 (# of cigarettes a day)
  # This is because this question is only asked of individuals who indicated that they are current smokers
  # If an individual's response is missing - this indicates they are a non-smoker
  # In addition, this response was recorded as numeric value
  # However, the impact of 10 vs. 11 cigarettes is somewhat trivial
  # In order to best account for non-smokers and get most impactful view of smoking
  # it could be valuable to bin this variable into:
  #     - nonsmoker (nonresponse aka 0 cigarettes)
  #     - light (1-5 cigarettes)
  #     - moderate (6-10 cigarettes)
  #     - heavy (11-20 cigarettes)
  #     - very heavy (21+ cigarettes)

b12data <- b12data %>%
  mutate(smoking = case_when(is.na(SMD650) ~"nonsmoker",
                             SMD650 <= 5 ~ "light",
                             SMD650 >= 6 & SMD650 <= 10 ~ "moderate",
                             SMD650 >= 11 & SMD650 <= 20 ~ "heavy",
                             SMD650 >= 21 ~ "very heavy")) 


# Second smoke exposure _______________________________________________________________________________
  # Second hand smoke can also have damaging effects to health
  # In order to understand if someone is being exposed to sustained second hand smoke at home
  # the variable SMD470 (how many people smoke at home indoors) was chosen. 
  
  # The levels of this variable are:
  #     - 0 = no one smokes in the house
  #     - 1 = one person smokes in the house
  #     - 2 = two people smoke in the house
  #     - 3 = three + people smoke in the house


# Re-coded below as follows:
  # Nonsmoker and 0 persons smokes inside = no indoor secondhand exposure
  # Smoker but 1 person smokes inside = no indoor secondhand exposure (they are the smoker)
  # Smoker and 0 persons smoke inside = no indoor secondhand exposure
  # Nonsmoker and 1, 2, 3+ people smoke = indoor secondhand exposure
  # Smoker and 2 persons smoke inside = indoor secondhand exposure
  # Smoker and 3+ persons smoke inside = indoor secondhand exposure

# In addition the two columns used to create the new column are dropped in the select clause
b12data <- b12data %>% 
  mutate(secondhand_smoke = case_when(is.na(SMD470) ~ "unknown",
                                      smoking == "nonsmoker" & SMD470 == 0 ~ "no exposure",
                                      smoking != "nonsmoker" & SMD470 == 1 ~ "no exposure",
                                      smoking != "nonsmoker" & SMD470 == 0 ~ "no exposure",
                                      smoking == "nonsmoker" & SMD470 != 0 ~ "exposure",
                                      smoking != "nonsmoker" & SMD470 == 2 ~ "exposure",
                                      smoking != "nonsmoker" & SMD470 == 3 ~ "exposure")) %>% 
  select(-c(SMD650, SMD470))


# Average Number of Drinks _____________________________________________________________________________
  # Similar to number of cigarettes smoked, number of drinks is can be binned
  # Meaning that the difference in 7 vs 8 drinks is less impactful than 7 vs. 20 drinks
  # Because of this we will bin below:

b12data <- b12data %>%
  mutate(ALQavgdrinks = case_when(is.na(ALQ130) ~"not drinker",
                             ALQ130 <= 2 ~ "light",
                             ALQ130 >= 3 & ALQ130 <= 5 ~ "moderate",
                             ALQ130 >= 6 & ALQ130 <= 10 ~ "heavy",
                             ALQ130 >= 11 & ALQ130 <= 25 ~ "very heavy",
                             ALQ130 == 999 ~ "unknown")) %>% 
  filter(ALQavgdrinks != "unknown") %>% 
  select(-c(ALQ130))

# Original column dropped
# Also there were 3 individuals who answered as unknown - 
# these individuals were dropped to ensure data quality


# Frozen pizza/meals  ____________________________________________________________________________________
# This is currently a numeric value but as above could be better served a categorical
b12data <- b12data %>%
  mutate(monthlyfrozen = case_when(DBD910 == 0  ~ "never",
                                  DBD910 >= 1 & DBD910 <= 3 ~ "rarely",
                                  DBD910 >= 4 & DBD910 <= 7 ~ "sometimes",
                                  DBD910 >= 8 & DBD910 <= 15 ~ "frequently",
                                  DBD910 >= 16 & DBD910 <= 180 ~ "very frequently",
                                  DBD910 == 9999 ~ "unknown")) %>% 
  filter(monthlyfrozen != "unknown") %>% 
  select(-c(DBD910))

# Original column dropped
# Also there were 11 individuals who answered as unknown - that were dropped


# Number of hours of sleep  _______________________________________________________________________________
# This is currently a numeric value but as above could be better served a categorical
b12data <- b12data %>%
  mutate(sleep_binned = case_when(is.na(SLD010H) ~ "unknown",
                                   SLD010H >= 2 & SLD010H <= 5 ~ "2-5 hours",
                                   SLD010H >= 6 & SLD010H <= 7 ~ "6-7 hours",
                                   SLD010H >= 8 & SLD010H <= 9 ~ "8-9 hours",
                                   SLD010H >= 10 & SLD010H <= 12 ~ "10-12 hours",
                                   SLD010H == 99 ~ "unknown")) %>% 
  filter(sleep_binned != "unknown") %>% 
  select(-c(SLD010H))

# Original column dropped
# Also there were 8 individuals who answered as unknown - that were dropped

# BMI _____________________________________________________________________________________________________
# height and weight are best understood in conjunction
# while BMI does have some issues it is the commonly accepted metric of health

# let's first remove respondents without either of these (height, weight) measures 
# (or extreme outliers which appear to be incorrectly recorded data)
b12data <- b12data %>%  filter(WHD010 < 82, WHD010 != "", WHD020 < 500, WHD020 != "")

# this removed 124 respondents - which is a lot however
  # I tend to prefer removing individuals without information (or correct information)
  # as opposed to imputing with the most common value, since there is no way of knowning
  # if that is actually anywhere near the correct value

# creating BMI variable using BMI formula
b12data$BMI <- (703*b12data$WHD020/b12data$WHD010**2)

# removing old height & weight variables
b12data <- b12data %>% select(-c(WHD010, WHD020))

#__________________________________________________________________________________________________________

##### Doing final tweaks #####
  # In this portion, final adjustments were made
  # Including removing individual rows with poor data quality & binning unknown information
  # More about these adjustments can be found in the data dictionary

b12data <- b12data %>% 
  filter(DMDBORN4 != 77, DMQMILIZ != 7, DMDMARTL != 77, BPQ020 != 9, DIQ010 != 9, 
         DLQ010 !=9, DLQ020 != 9, DLQ040 < 7, DLQ050 != 9, DLQ080 != 9, HIQ011 != 9, 
         HUQ090 !=9, MCQ160M !=9, MCQ160E != 9, MCQ160C != 9, OHQ030 < 8) %>% 
  mutate(INDHHIN2 = case_when(is.na(INDHHIN2) ~ "99", 
                              INDHHIN2 == "77" ~ "99", 
                              T ~ as.character(INDHHIN2))) %>% 
  mutate(HSD010 = case_when(is.na(HSD010) ~ "9", 
                              T ~ as.character(HSD010))) %>% 
  mutate(FSD032A = case_when(is.na(FSD032A) ~ "9", 
                             FSD032A == "7" ~ "9", 
                              T ~ as.character(FSD032A))) %>% 
  mutate(HOQ065 = case_when(is.na(HOQ065) ~ "9", 
                            HOQ065 == "7" ~ "9", 
                             T ~ as.character(HOQ065))) %>% 
  mutate(RXQ510 = case_when(is.na(RXQ510) ~ "not applicable", 
                            RXQ510 == "7" ~ "9", 
                            T ~ as.character(HOQ065))) %>% 
  mutate(ALQ120U = case_when(is.na(ALQ120U) ~ "0", 
                            T ~ as.character(ALQ120U))) %>% 
  select(-c(SEQN))

# In total this removed 44 observations
# The unique ID of SEQN was also removed

#__________________________________________________________________________________________________________

##### Rerunning our missing loop #####

# checking to see that all missing values have been handled - this will save us headaches later
for (i in 1:ncol(b12data)){
  if ( i == 1){
    results <- data.frame(missing = 0, stringsAsFactors = FALSE)
  } else {
    missvar <- sum(is.na(b12data[,i]))
    results <- rbind(results,missvar)
    i <- i + 1
  }
}

# Creating data frame with all the variable names
col <- as.data.frame(colnames(b12data))

# Merging column names with missing values
# This gives us a dataframe with column name & number of missing values
results <- cbind(col, results)

# Dividing the number missing by the number of observations gives us percent missing
results$percent_missing <- results$missing / nrow(b12data)

# Now we can use this percent missing to filter our data
# We will start by sorting the data by percent missing
varsbymiss <- results %>% arrange(percent_missing)

# All good! No missing values

# getting list of column names
cols <- colnames(b12data)

# Changing variable types ________________________________________________________________________________
# While we have a lot numeric values within our data, 
# a lot of these  (all but BMI & age) of are actually categorical markers, aka data with factors
# This is important to have identified correctly for modeling later
# So we'll go ahead and take care of it now before splitting 

# removing the 2 continuous variables from our list
cols <- cols[cols %in% c("RIDAGEYR", "BMI") == FALSE] 

# converting the rest of the list (categorical variables & target variable) to character
for (i in cols){
  b12data[[i]] <- as.factor(b12data[[i]]) 
}

#__________________________________________________________________________________________________________

##### Splitting data #####

# an important part of model building is splitting your data
# we will split our data into 3 parts: train, validation, & test

# splitting data
# set.seed just allows this random sampling to be replicated
  # (8888 has no meaning other than 8 is my favorite number!)
set.seed(8888)
assignment <- sample(1:3, size = nrow(b12data), prob = c(0.7, 0.2, 0.1), replace = TRUE)

# Create a train, validation and tests from the original data frame 
b12_train <- b12data[assignment == 1, ]  
b12_valid <- b12data[assignment == 2, ]  
b12_test <- b12data[assignment == 3, ] 

# We are now ready to do some modeling!!!!

# Let's save these data sets for future use

# Since we'll be working in R we can just save it as Rdata
# It will bundle it all up together in one file - easy!
save(b12_train, b12_valid, b12_test, file = "b12split.Rdata")


# But for ease of access for others who may not be using R
# We'll also write it to CSVs

write.csv(b12_train, "b12train.csv")
write.csv(b12_valid, "b12valid.csv")
write.csv(b12_test, "b12test.csv")
