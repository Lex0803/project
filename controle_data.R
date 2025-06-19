


getwd()

# 1. Load required packages
library(dplyr)
library(readr)
library(stringr)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(tidyverse)

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

Low_and_High_Education <- Rename_Combined_Df %>%
  mutate(
    Low_Education_rate =
      Basisonderwijs +
      VMBO_HAVO_VWO_Onderbouw_MBO_1 +
      HAVO_VWO_MBO_2_4,
    High_Education_rate = 
      HBO_WO_Bachelor +
      HBO_WO_Master_Doctor
  )
Low_and_High_Education <- Low_and_High_Education[, -c(6, 7, 8, 9, 10)]
Low_and_High_Education <- Low_and_High_Education %>% 
  rename(
    Jaar = Perioden
  )

Low_and_High_Education_Netherlands <- filter(Low_and_High_Education, Regio.s == "Nederland")
Low_and_High_Education_Netherlands <- Low_and_High_Education_Netherlands[-c(9), ]


df_yearly <- df_yearly[-c(9, 10, 11), ]

Education_Employment <- df_yearly %>%
  right_join(Low_and_High_Education_Netherlands, by = "Jaar") %>%
  mutate(new_column = Nederland / Low_Education_rate,
         Jaar = as.numeric(Jaar))
  
ggplot(Education_Employment, aes(x = Jaar, y = new_column)) +
  geom_line() +
  labs(x = "\nYear", y = "Unemployment \nby Low Educated\n")+
  scale_x_continuous(breaks = seq(2015, 2022, by = 1), lim = c(2015, 2022))

ggplot(Education_Employment, aes(x = Jaar, y = new_column)) +
  geom_line() +
  geom_vline(xintercept = 2020, 
             color = "red", 
             linetype = "dashed") +
  labs(x = "\nYear", y = "Unemployment \nby Low Educated\n")+
  scale_x_continuous(breaks = seq(2015, 2022, by = 1), lim = c(2015, 2022))

Low_and_High_Education_Provinces_2022 <- filter(Low_and_High_Education, Jaar == "2022") %>%
  filter(grepl("\\(PV\\)", Regio.s))
Low_and_High_Education_Provinces_2022 <- Low_and_High_Education_Provinces_2022 %>%
  mutate(Regio.s = gsub(" \\(PV\\)", "", Regio.s))
Low_and_High_Education_Provinces_2022$Regio.s <- ifelse(Low_and_High_Education_Provinces_2022$Regio.s == "Fryslân", "Friesland", Low_and_High_Education_Provinces_2022$Regio.s)
Low_and_High_Education_Provinces_2022 <- Low_and_High_Education_Provinces_2022[, -c(1, 2, 5, 7)]


df_yearly_transposed <- df_yearly %>%
  pivot_longer(
    cols = -Jaar
  )
df_yearly_transposed <- filter(df_yearly_transposed, Jaar == "2022")
df_yearly_transposed <- filter(df_yearly_transposed, !name == "Nederland")
df_yearly_transposed$name <- ifelse(df_yearly_transposed$name == "Noord.Holland", "Noord-Holland", df_yearly_transposed$name)
df_yearly_transposed$name <- ifelse(df_yearly_transposed$name == "Zuid.Holland", "Zuid-Holland", df_yearly_transposed$name)
df_yearly_transposed$name <- ifelse(df_yearly_transposed$name == "Noord.Brabant", "Noord-Brabant", df_yearly_transposed$name)
df_yearly_transposed <- df_yearly_transposed %>%
  rename(Regio.s = name,
         Unemployment = value)
df_yearly_transposed <- df_yearly_transposed %>%
  right_join(Low_and_High_Education_Provinces_2022, by = "Regio.s") %>%
  mutate(new_column = Unemployment / Low_Education_rate)


ratio_data <- df_yearly_transposed[, -c(1, 3, 4, 5)]


Dutch_provinces <- ne_states(country = "Netherlands", returnclass = "sf") %>%
  filter(!name %in% c("St. Eustatius", "Saba"))

map_data <- Dutch_provinces %>%
  inner_join(ratio_data, by = c("name" = "Regio.s"))

ggplot(map_data, aes(fill = new_column)) +
  geom_sf(color = "white", size = 0.2) +
  scale_fill_gradient(low = "blue", high = "red",
                      name = "Werkloosheid per\nlaagopgeleide\nin 2022")

Low_and_High_Education_Randstad <- filter(Low_and_High_Education, grepl("\\(PV\\)", Regio.s))
Low_and_High_Education_Randstad <- Low_and_High_Education_Randstad %>%
mutate(Regio.s = gsub(" \\(PV\\)", "", Regio.s))
Low_and_High_Education_Randstad <- filter(Low_and_High_Education_Randstad, Regio.s == "Noord-Holland" | Regio.s == "Zuid-Holland" | Regio.s == "Utrecht")
Low_and_High_Education_Randstad <- filter(Low_and_High_Education_Randstad, !Jaar == "2023*")
Low_and_High_Education_Randstad <- Low_and_High_Education_Randstad[, -c(1, 2, 5, 7)]

df_yearly_transposed_Randstad<- df_yearly %>%
  pivot_longer(
    cols = -Jaar
  )
df_yearly_transposed_Randstad$name <- ifelse(df_yearly_transposed_Randstad$name == "Noord.Holland", "Noord-Holland", df_yearly_transposed_provinces$name)
df_yearly_transposed_Randstad$name <- ifelse(df_yearly_transposed_Randstad$name == "Zuid.Holland", "Zuid-Holland", df_yearly_transposed_provinces$name)
df_yearly_transposed_Randstad <- filter(df_yearly_transposed_Randstad, name == "Noord-Holland" | name == "Zuid-Holland" | name == "Utrecht")
df_yearly_transposed_Randstad <- df_yearly_transposed_Randstad %>%
  rename(Regio.s = name,
         Unemployment = value)
df_yearly_transposed_Randstad <- right_join(
  df_yearly_transposed_Randstad,
  Low_and_High_Education_Randstad,
  by = c("Regio.s", "Jaar")
)
df_yearly_transposed_Randstad <- df_yearly_transposed_Randstad %>%
  mutate(new_column = Unemployment / Low_Education_rate)

mean_by_Randstad <- df_yearly_transposed_Randstad %>%
  group_by(Regio.s) %>%           
  summarise(
    Mean_Value = mean(new_column) 
  )

Low_and_High_Education_Noord_Nederland <- filter(Low_and_High_Education, grepl("\\(PV\\)", Regio.s))
Low_and_High_Education_Noord_Nederland <- Low_and_High_Education_Noord_Nederland %>%
  mutate(Regio.s = gsub(" \\(PV\\)", "", Regio.s))
Low_and_High_Education_Noord_Nederland$Regio.s <- ifelse(Low_and_High_Education_Noord_Nederland$Regio.s == "Fryslân", "Friesland", Low_and_High_Education_Noord_Nederland$Regio.s)
Low_and_High_Education_Noord_Nederland <- filter(Low_and_High_Education_Noord_Nederland, Regio.s == "Groningen" | Regio.s == "Friesland" | Regio.s == "Drenthe")
Low_and_High_Education_Noord_Nederland <- filter(Low_and_High_Education_Noord_Nederland, !Jaar == "2023*")
Low_and_High_Education_Noord_Nederland <- Low_and_High_Education_Noord_Nederland[, -c(1, 2, 5, 7)]

df_yearly_transposed_Noord_Nederland <- df_yearly %>%
  pivot_longer(
    cols = -Jaar
  )
df_yearly_transposed_Noord_Nederland <- filter(df_yearly_transposed_Noord_Nederland, name == "Drenthe" | name == "Friesland" | name == "Groningen")
df_yearly_transposed_Noord_Nederland <- df_yearly_transposed_Noord_Nederland %>%
  rename(Regio.s = name,
         Unemployment = value)
df_yearly_transposed_Noord_Nederland <- right_join(
  df_yearly_transposed_Noord_Nederland,
  Low_and_High_Education_Noord_Nederland,
  by = c("Regio.s", "Jaar")
)
df_yearly_transposed_Noord_Nederland <- df_yearly_transposed_Noord_Nederland %>%
  mutate(new_column = Unemployment / Low_Education_rate)

mean_by_Noord_Nederland <- df_yearly_transposed_Noord_Nederland %>%
  group_by(Regio.s) %>%           
  summarise(
    Mean_Value = mean(new_column) 
  )
