## Only run "install.packages()" once when you run the code very first time.
## Once you downloaded the package once, you don't need to download again in the future.
install.packages("readxl")
install.packages("writexl")
install.packages("stringr")
install.packages("plyr")

#loading the packages
library(readxl)
library(writexl)
library(stringr)
library(plyr)

#loading the functions
source("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/R Scripts/Home Safe Reporting Functions.R")

#loading the dataset
hsaps = read_xlsx("//cdss/feed/Central Office/HHCRB/HHB/Housing Programs/Home Safe/Data and Evaluation/Data Reports to date/Cleaning In Progress/^hsaps_cleaned_2023-09-27.xlsx")

##getting the numbers (make sure to have the most recent cleaned hsaps data)
# examples
hsaps_enrolled_num(hsaps,"2023-06-30","2023-09-30")
hsaps_race_ethnicity(hsaps,"2023-06-30","2023-09-30")
hsaps_homelessness(hsaps,"2023-06-30","2023-09-30")
hsaps_enrollment_duration(hsaps,"2023-06-30","2023-09-30")
hsaps_exit(hsaps,"2023-06-30","2023-09-30")
hsaps_perm_housed(hsaps,"2023-06-30","2023-09-30")

# more examples
hsaps_enrolled_num(hsaps,"2018-06-30","2023-06-30")
hsaps_exit(hsaps,"2021-06-30","2022-06-30")
