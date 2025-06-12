


getwd()

# 1. Load required packages
library(dplyr)
library(readr)
library(stringr)

# 2. Load the dataset 
df <- read.csv2("Werkloosheidpercentage.csv")

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


Df2015=read.csv2("2015_onderwijsniveau.csv")
Df2016=read.csv2("2016_onderwijsniveau.csv")
Df2017=read.csv2("2017_onderwijsniveau.csv")
Df2018=read.csv2("2018_onderwijsniveau.csv")
Df2019=read.csv2("2019_onderwijsniveau.csv")
Df2020=read.csv2("2020_onderwijsniveau.csv")
Df2021=read.csv2("2021_onderwijsniveau.csv")
Df2022=read.csv2("2022_onderwijsniveau.csv")
Df2023=read.csv2("2023_onderwijsniveau.csv")
Combined_Df=rbind(Df2015, Df2016, Df2017, Df2018, Df2019, Df2020, Df2021,Df2022,Df2023)
write.csv(Combined_Df,"Totaal_onderwijsniveau.csv", row.names = FALSE)

Rename_Combined_Df <- Combined_Df %>% 
  rename(
    Basisonderwijs = Onderwijsniveau.5.categorieën.11.Basisonderwijs....,
    VMBO_HAVO_VWO_Onderbouw_MBO_1 = Onderwijsniveau.5.categorieën.12.Vmbo..havo...vwo.onderbouw..mbo1....,
    HAVO_VWO_MBO_2_4 = Onderwijsniveau.5.categorieën.21.Havo..vwo..mbo2.4....,
    HBO_WO_Bachelor = Onderwijsniveau.5.categorieën.31.Hbo...wo.bachelor....,
    HBO_WO_Master_Doctor = Onderwijsniveau.5.categorieën.32.Hbo...wo.master..doctor....
  )

library(dplyr)
library(tidyverse)
Rename_Combined_Df <- Combined_Df %>% 
  rename(
    Basisonderwijs = Onderwijsniveau.5.categorieën.11.Basisonderwijs....,
    VMBO_HAVO_VWO_Onderbouw_MBO_1 = Onderwijsniveau.5.categorieën.12.Vmbo..havo...vwo.onderbouw..mbo1....,
    HAVO_VWO_MBO_2_4 = Onderwijsniveau.5.categorieën.21.Havo..vwo..mbo2.4....,
    HBO_WO_Bachelor = Onderwijsniveau.5.categorieën.31.Hbo...wo.bachelor....,
    HBO_WO_Master_Doctor = Onderwijsniveau.5.categorieën.32.Hbo...wo.master..doctor....
  )



Low_and_High_Education <- Rename_Combined_Df %>%
  mutate(
    Low_Education_rate =
      Basisonderwijs +
      VMBO_HAVO_VWO_Onderbouw_MBO_1,
    High_Education_rate = 
      HAVO_VWO_MBO_2_4 +
      HBO_WO_Bachelor +
      HBO_WO_Master_Doctor
  )
Low_and_High_Education <- Low_and_High_Education[, -c(6, 7, 8, 9, 10)]