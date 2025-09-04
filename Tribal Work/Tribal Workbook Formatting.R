library(openxlsx)
file = "C:/Users/sypark/Desktop/Data Processing/Tribal Work/Tribal Grantee Workbook.xlsx"
wb = loadWorkbook(file)

#defining drop downs
raceneth = c("American Indian, Alaska Native, or Indigenous",
             "Asian or Asian American","Black, African American, or African",
             "Hispanic/Latina/e/o","Middle Eastern or North African","Native Hawaiian or Pacific Islander")

#implementing drop downs in columns
dataValidation(wb,sheet="Report",cols=82,rows=3:500,type="list",value="'Sheet2'!$A$2:$A$11")

#saving the workbook
saveWorkbook(wb, "Tribal Work/test.xlsx", overwrite = TRUE)
