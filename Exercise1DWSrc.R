# Load the datafile to datastrucutre mydata
mydata <- read.csv("C:\\Users\\Rekha\\Documents\\REKHA\\DataScience\\SpringBoard\\DataWrangling\\refine_original.csv")
# Cleanup brandnames - correct misspellings, Convert the company column values to lowercase
mydata$company <- ifelse (mydata$company == "fillips", "phillips", ifelse(mydata$company == "philips", "phillips",ifelse(mydata$company == "phllips", "phillips", ifelse(mydata$company == "phillps", "phillips", ifelse(mydata$company == "phlips", "phillips", ifelse (mydata$company == "unilver", "unilever", ifelse(mydata$company == "akz0", "akzo", ifelse (mydata$company == "ak zo", "akzo", mydata$company))))))))
mydata$company <- tolower(mydata$company)
# Separate productcode and number
mydata <- separate(mydata, Product.code...number, c("productcode", "number"), sep="-")
# Add product categories
mydata <- mydata %>% mutate(productcategory = ifelse(mydata$productcode == "p", "Smartphone", ifelse(mydata$productcode == "v", "TV", ifelse(mydata$productcode == "x", "Laptop", ifelse(mydata$productcode == "q", "Tablet", "NA")))))
# Add full address for geocoding
mydata <- mydata %>% mutate(full_address = paste(mydata$address, mydata$city, mydata$country, sep=","))
# Create dummy variables for company and product category
mydata <- dummy_cols(mydata, select_columns = c("company", "productcategory"))
# Save the dataframe to csv file refine_clean
write.csv(mydata, "C:\\Users\\Rekha\\Documents\\REKHA\\DataScience\\SpringBoard\\DataWrangling\\refine_clean_new.csv")