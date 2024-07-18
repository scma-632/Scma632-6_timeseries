#https://www.r-bloggers.com/2021/12/vector-error-correction-model-vecm-using-r/
setwd('D:\\SCMA 2024\\Data')
getwd()
library(readxl)
df = read_excel('macronew.xlsx')
sheet = excel_sheets('macronew.xlsx')
sheet

df1= read_excel('macronew.xlsx', sheet='deficit')
df2= read_excel('macronew.xlsx', sheet='CREDIT')
df3= read_excel('macronew.xlsx', sheet='GDP')
head(df3)


# Load necessary libraries
library(readxl)
library(dplyr)

# Specify the file path
file_path <- "macronew.xlsx"

# List of sheet names
sheets <- c("deficit", "CREDIT", "GDP", "EXIM", "WPI", "IIP", "IAP", "FDI", "EXRATE", "BOP", "EMPLOY", "M", "RAIN", "oil","GDP_W")

# Initialize an empty list to store dataframes
data_frames <- list()

# Loop through each sheet name and read the data into a dataframe
for (sheet in sheets) {
  data_frames[[sheet]] <- read_excel(file_path, sheet = sheet)
}

# Merge all dataframes using "deficit" as the base for merging by "year"
combined_data <- data_frames[["deficit"]]

for (sheet in sheets[-1]) {
  combined_data <- combined_data %>%
    left_join(data_frames[[sheet]], by = "Year")
}

# View the combined data
print(combined_data)

dim(combined_data)
names(combined_data)

macro = combined_data[,c(1,4,14,15,24,36,37,63,65,73,83,84,85,86)]
head(macro)
tail(macro)
dput(names(macro))

library(janitor)
macro = clean_names(macro)
dput(names(macro))

df1 = macro[,c(4,6,8,10)]
names(df1)

vecm.model <- ca.jo(
    df1, ecdet = 'const', type  = 'eigen', K = 2, spec = 'transitory', dumvar = NULL)
 
summary(vecm.model)