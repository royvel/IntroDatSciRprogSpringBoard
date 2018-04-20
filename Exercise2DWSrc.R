# Load the datafile to datastrucutre mytitanic and replace blank or spaces to NULL(NA) values
mytitanic <- read.csv("REKHA\\DataScience\\SpringBoard\\DataWrangling\\Exercise2\\titanic_original.csv",na.strings = c("", " ", "NA"))
# Replace null values (NA) in embarked column with value S
mytitanic <- mytitanic %>% replace_na(list(embarked = "S"))
# Replace null values in age column with mean of age : we could use min or max age but mean is between min $ max so a better representation 
mytitanic <- mytitanic %>% replace_na(list(age = mean(mytitanic$age, na.rm = TRUE)))
# Fill the empty slots in boat column with a dummy value e.g. the string 'None' or 'NA'
# already did when reading the file.
# Add a new column has_cabin_number which has 1 if there is a cabin number, and 0 otherwise.
mytitanic <- mytitanic %>% mutate(has_cabin_number = ifelse(is.na(mytitanic$cabin), 0, 1))
# write the data from dataframe mytitanic to output csv file refine_titanic
write.csv(mytitanic, "REKHA\\DataScience\\SpringBoard\\DataWrangling\\Exercise2\\titanic_clean.csv")