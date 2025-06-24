#Load required packages 
library(dplyr) 
library(readr) 
library(stringr) 
library(sf) 
library(rnaturalearth) 
library(ggplot2) 
library(tidyverse) 


#Load the dataset
Unemployment <- read.csv2("Werkloosheidpercentage.csv") 

#Rename the first column and extract the year from the 'Quarter' column 
colnames(Unemployment)[1] <- "Quarter" 
Unemployment$Year <- str_extract(Unemployment$Quarter, "\\d{4}") 

#Replace commas with dots in the numeric columns and convert to numeric 
#(exclude 'Quarter' and 'Jaar' columns) 
numeric_cols <- setdiff(colnames(Unemployment), c("Quarter", "Year")) 
Unemployment[numeric_cols] <- lapply(Unemployment[numeric_cols], function(x) { 
  as.numeric(str_replace_all(x, ",", ".")) 
}) 

#Group by year and calculate the mean per column 
Unemployment_yearly <- Unemployment %>% 
  group_by(Year) %>% 
  summarise(across(all_of(numeric_cols), ~round(mean(.x, na.rm = TRUE), 2))) 

#Load in the education level by year datasets
Educationlevel2015=read.csv2("2015_onderwijsniveau.csv", fileEncoding = "UTF-8") 
Educationlevel2016=read.csv2("2016_onderwijsniveau.csv", fileEncoding = "UTF-8") 
Educationlevel2017=read.csv2("2017_onderwijsniveau.csv", fileEncoding = "UTF-8") 
Educationlevel2018=read.csv2("2018_onderwijsniveau.csv", fileEncoding = "UTF-8") 
Educationlevel2019=read.csv2("2019_onderwijsniveau.csv", fileEncoding = "UTF-8") 
Educationlevel2020=read.csv2("2020_onderwijsniveau.csv", fileEncoding = "UTF-8") 
Educationlevel2021=read.csv2("2021_onderwijsniveau.csv", fileEncoding = "UTF-8") 
Educationlevel2022=read.csv2("2022_onderwijsniveau.csv", fileEncoding = "UTF-8") 
Educationlevel2023=read.csv2("2023_onderwijsniveau.csv", fileEncoding = "UTF-8") 
Educationlevel_by_year=rbind(Educationlevel2015, Educationlevel2016, Educationlevel2017, Educationlevel2018, Educationlevel2019, Educationlevel2020, Educationlevel2021,Educationlevel2022,Educationlevel2023) 

#Renamed all the column names to an easier accessible name or to an English word
Educationlevel_by_year <- Educationlevel_by_year %>%  
  rename( 
    Basisonderwijs = Onderwijsniveau.5.categorien.11.Basisonderwijs...., 
    VMBO_HAVO_VWO_Onderbouw_MBO_1 = Onderwijsniveau.5.categorien.12.Vmbo..havo...vwo.onderbouw..mbo1...., 
    HAVO_VWO_MBO_2_4 = Onderwijsniveau.5.categorien.21.Havo..vwo..mbo2.4...., 
    HBO_WO_Bachelor = Onderwijsniveau.5.categorien.31.Hbo...wo.bachelor...., 
    HBO_WO_Master_Doctor = Onderwijsniveau.5.categorien.32.Hbo...wo.master..doctor....,
    Gender = Geslacht,
    Age = Leeftijd,
    Year = Perioden,
    Region = Regio.s,
    Population = Bevolking..aantal.
  )

#Made the variables for low and high education by combining certain columns
Low_and_High_Education <- Educationlevel_by_year %>% 
  mutate( 
    Low_Education_rate = 
      Basisonderwijs + 
      VMBO_HAVO_VWO_Onderbouw_MBO_1 + 
      HAVO_VWO_MBO_2_4, 
    High_Education_rate =  
      HBO_WO_Bachelor + 
      HBO_WO_Master_Doctor 
  ) 

#Removed certain columns from the dataset Low_and_High_Education to make it more readable
Low_and_High_Education <- Low_and_High_Education[, -c(6, 7, 8, 9, 10)] 

#Filtered the Netherlands and removed the row for 2023, because used data is from 2015 to 2022
Low_and_High_Education_Netherlands <- filter(Low_and_High_Education, Region == "Nederland") 
Low_and_High_Education_Netherlands <- Low_and_High_Education_Netherlands[-c(9), ] 

#Removed rows for 2023, 2024 and 2025, because used data is from 2015 to 2022
Unemployment_yearly <- Unemployment_yearly[-c(9, 10, 11), ] 

#Merged the the datasets for Dutch unemployment and education level
#Also made sure that the cells in the column Year are numeric
Education_Employment <- Unemployment_yearly %>% 
  right_join(Low_and_High_Education_Netherlands, by = "Year") %>% 
  mutate(Unemployment_by_Low_Education = Nederland / Low_Education_rate, 
         Year = as.numeric(Year)) 

#Made the graph for temporal visualisation
ggplot(Education_Employment, aes(x = Year, y = Unemployment_by_Low_Education)) + 
  geom_line() + 
  labs(x = "\nYear", y = "Unemployment \nper Low Educated\n")+ 
  scale_x_continuous(breaks = seq(2015, 2022, by = 1), lim = c(2015, 2022)) 

#Made the graph for event analysis
ggplot(Education_Employment, aes(x = Year, y = Unemployment_by_Low_Education)) + 
  geom_line() + 
  geom_vline(xintercept = 2020,  
             color = "red",  
             linetype = "dashed") + 
  labs(x = "\nYear", y = "Unemployment \nper Low Educated\n")+ 
  scale_x_continuous(breaks = seq(2015, 2022, by = 1), lim = c(2015, 2022)) 

#Filtered the provinces by the year 2022, and removed the (PV) after the provinces in the cells
#Also changed Fryslan into Friesland and removed columns for readability
Low_and_High_Education_Provinces_2022 <- filter(Low_and_High_Education, Year == "2022") %>% 
  filter(grepl("\\(PV\\)", Region)) 
Low_and_High_Education_Provinces_2022 <- Low_and_High_Education_Provinces_2022 %>% 
  mutate(Region = gsub(" \\(PV\\)", "", Region)) 
Low_and_High_Education_Provinces_2022$Region <- ifelse(Low_and_High_Education_Provinces_2022$Region == "Fryslan", "Friesland", Low_and_High_Education_Provinces_2022$Region) 
Low_and_High_Education_Provinces_2022 <- Low_and_High_Education_Provinces_2022[, -c(1, 2, 5, 7)] 

#Transposed the dataset Unemployment_yearly to merge with Low_and_High_Education_Provinces_2022
#Also changed some names of the provinces the match with Low_and_High_Education_Provinces_2022
Unemployment_yearly_transposed <- Unemployment_yearly %>% 
  pivot_longer( 
      cols = -Year 
    ) 
Unemployment_yearly_transposed <- filter(Unemployment_yearly_transposed, Year == "2022") 
Unemployment_yearly_transposed <- filter(Unemployment_yearly_transposed, !name == "Nederland") 
Unemployment_yearly_transposed$name <- ifelse(Unemployment_yearly_transposed$name == "Noord.Holland", "Noord-Holland", Unemployment_yearly_transposed$name) 
Unemployment_yearly_transposed$name <- ifelse(Unemployment_yearly_transposed$name == "Zuid.Holland", "Zuid-Holland", Unemployment_yearly_transposed$name) 
Unemployment_yearly_transposed$name <- ifelse(Unemployment_yearly_transposed$name == "Noord.Brabant", "Noord-Brabant", Unemployment_yearly_transposed$name) 
Unemployment_yearly_transposed <- Unemployment_yearly_transposed %>% 
  rename(Region = name, 
         Unemployment = value) 

#Merged the datasets Unemployment_yearly_transposed and Low_and_High_Education_Provinces_2022
Education_Unemployment_yearly_transposed <- Unemployment_yearly_transposed %>% 
  right_join(Low_and_High_Education_Provinces_2022, by = "Region") %>% 
  mutate(Unemployment_by_Low_Education = Unemployment / Low_Education_rate) 

#Cleaned the data from Education_Unemployment_yearly_transposed to make it more readable
Education_Unemployment_ratio_data <- Education_Unemployment_yearly_transposed[, -c(1, 3, 4, 5)] 

#Chosen which map to choose and joined the used data from the datasets
Dutch_provinces <- ne_states(country = "Netherlands", returnclass = "sf") %>% 
  filter(!name %in% c("St. Eustatius", "Saba")) 
map_data <- Dutch_provinces %>% 
  inner_join(Education_Unemployment_ratio_data, by = c("name" = "Region")) 

#Made the map
ggplot(map_data, aes(fill = Unemployment_by_Low_Education)) + 
  geom_sf(color = "white", size = 0.2) + 
  scale_fill_gradient(low = "blue", high = "red", 
                      name = "Unemployed per\nLow Educated\nin 2022") 

#Filtered the used provinces for every year except 2023
Low_and_High_Education_provinces <- filter(Low_and_High_Education, grepl("\\(PV\\)", Region)) 
Low_and_High_Education_provinces <- Low_and_High_Education_provinces %>% 
  mutate(Region = gsub(" \\(PV\\)", "", Region)) 
Low_and_High_Education_provinces$Region <- ifelse(Low_and_High_Education_provinces$Region == "Fryslan", "Friesland", Low_and_High_Education_provinces$Region) 
Low_and_High_Education_provinces <- filter(Low_and_High_Education_provinces, Region == "Noord-Holland" | Region == "Zuid-Holland" | Region == "Utrecht" | Region == "Friesland" | Region == "Drenthe" | Region == "Groningen") 
Low_and_High_Education_provinces <- filter(Low_and_High_Education_provinces, !Year == "2023*") 
Low_and_High_Education_provinces <- Low_and_High_Education_provinces[, -c(1, 2, 5, 7)] 

#Transposed and filtered the used provinces for every year
Unemployment_yearly_transposed_provinces<- Unemployment_yearly %>% 
  pivot_longer( 
    cols = -Year 
  ) 
Unemployment_yearly_transposed_provinces$name <- ifelse(Unemployment_yearly_transposed_provinces$name == "Noord.Holland", "Noord-Holland", Unemployment_yearly_transposed_provinces$name) 
Unemployment_yearly_transposed_provinces$name <- ifelse(Unemployment_yearly_transposed_provinces$name == "Zuid.Holland", "Zuid-Holland", Unemployment_yearly_transposed_provinces$name) 
Unemployment_yearly_transposed_provinces <- filter(Unemployment_yearly_transposed_provinces, name == "Noord-Holland" | name == "Zuid-Holland" | name == "Utrecht" | name == "Drenthe" | name == "Groningen" | name =="Friesland") 
Unemployment_yearly_transposed_provinces <- Unemployment_yearly_transposed_provinces %>% 
  rename(Region = name, 
         Unemployment = value) 

#Merged the two datasets Education_Unemployment_yearly_transposed_provinces and Low_and_High_Education_provinces
Education_Unemployment_yearly_transposed_provinces <- right_join( 
  Unemployment_yearly_transposed_provinces, 
  Low_and_High_Education_provinces, 
  by = c("Region", "Year")
  ) 

#Added the Unemployment_by_Low_Education column
Education_Unemployment_yearly_transposed_provinces <- Education_Unemployment_yearly_transposed_provinces %>% 
  mutate(Unemployment_by_Low_Education = Unemployment / Low_Education_rate) 

#Added the mean for each province 
mean_by_provinces <- Education_Unemployment_yearly_transposed_provinces %>% 
  group_by(Region) %>%            
  summarise( 
    Mean_Unemployment_by_Low_Education = mean(Unemployment_by_Low_Education)  
  ) 

#Added region groups next to the provinces belonging to the region group 
mean_by_provinces$Region_Group <- c(rep("Noord-Nederland", 3), rep("Randstad", 3)) 

#Plotting the boxplot 
ggplot(data = mean_by_provinces, mapping = aes(x = Region_Group, y = Mean_Unemployment_by_Low_Education)) + 
  geom_boxplot() + 
  labs(x = "\nRegion", y = "Unemployment per \nLow Educated\n")
