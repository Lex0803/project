
onderwijsniveau_regio2023 <- read.csv("C:/Users/Veerman/Downloads/Hoogst_behaald_onderwijsniveau__regio_05062025_113647.csv", sep=";")
werkeloosheid_provincie <- read.csv2("C:/Users/Veerman/Downloads/Werklozename (% van beroepsbevolking).csv")
getwd()

# 1. Load required packages
library(dplyr)
library(readr)
library(stringr)

# 2. Load the dataset 
df <- read.csv("Werkloosheidpercentage.csv", sep = ";", stringsAsFactors = FALSE)

# 3. Rename the first column
colnames(df)[1] <- "Kwartaal"

# 4. Extract the year from the 'Kwartaal' column
df$Jaar <- str_extract(df$Kwartaal, "\\d{4}")

# 5. Replace commas with dots in the numeric columns and convert to numeric
# (exclude 'Kwartaal' and 'Jaar' columns)
numeric_cols <- setdiff(colnames(df), c("Kwartaal", "Jaar"))

df[numeric_cols] <- lapply(df[numeric_cols], function(x) {
  as.numeric(str_replace_all(x, ",", "."))
})

# 6. Group by year and calculate the mean per column
df_yearly <- df %>%
  group_by(Jaar) %>%
  summarise(across(all_of(numeric_cols), ~round(mean(.x, na.rm = TRUE), 2)))

# 7. View the result
print(df_yearly)

# 8. Optional: Export to CSV
write.csv(df_yearly, "Werkloosheid_per_jaar.csv", row.names = FALSE)
