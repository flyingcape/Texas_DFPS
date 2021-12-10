library(dplyr) 
library(tidyr) 
library(ggplot2) 
library(readr)
library(usmap)
library(stringr)

Children_DFPS <-read.csv("CData/Children_DFPS.csv")
Adopt_Need <-read.csv("CData/Adopt_Need.csv")
Homes <-read.csv("CData/Homes.csv")
PEI_Families_Served <-read.csv("CData/PEI_Families_Served.csv")
map_data <- read.csv("CData/map_data_cont.csv")

# changing order of factors for plotting
Children_DFPS$Age_Group = as.factor(Children_DFPS$Age_Group)
Children_DFPS$Age_Group <- factor(Children_DFPS$Age_Group, levels = c("0 to 5 Years", "6 to 12 Years", "13 to 18 Years"))

# renaming and changing order of factors for plotting
Homes$Type[which(Homes$Type == "Adoptive Homes")] <- "Adoptive"
Homes$Type[which(Homes$Type == "Foster/Adoptive Homes")] <- "Foster/Adoptive"
Homes$Type[which(Homes$Type == "Foster Homes")] <- "Foster"
Homes$Type = as.factor(Homes$Type)
Homes$Type <- factor(Homes$Type, levels = c("Foster/Adoptive", "Adoptive", "Foster"))

# renaming and changing order of factors for plotting
Adopt_Need <- Adopt_Need %>% rename(`Placement Intention` = Current.Placement.Status)
Adopt_Need$`Placement Intention`[which(Adopt_Need$`Placement Intention` == "Placement Not Intended to be Permanent")] <- "Not Permanent"
Adopt_Need$`Placement Intention`[which(Adopt_Need$`Placement Intention` == "Placement Intended to be Permanent")] <- "Permanent"
Adopt_Need$`Placement Intention` = as.factor(Adopt_Need$`Placement Intention`)
Adopt_Need$`Placement Intention` <- factor(Adopt_Need$`Placement Intention`, levels = c("Permanent", "Not Permanent"))

map_data_year <- filter(map_data, Year == "2020")

data <- map_data_year
var1 = data$Adopt_Need_Total_County
var2 = data$Homes_Total_County

data$Adopt_Need_Total_County[is.na(data$Adopt_Need_Total_County)] <- 0
data$Homes_Total_County[is.na(data$Homes_Total_County)] <- 0
var1[is.na(var1)] <- 0
var2[is.na(var2)] <- 0

# create 3 buckets for texas_homes
quantiles_var1 <- quantile(var1, 
                           probs = c(0,0.9632,0.9914,1), na.rm = TRUE)
# create 3 buckets for adoption need
quantiles_var2 <- quantile(var2, 
                           probs = c(0,0.90,0.9933,1), na.rm = TRUE)

# create color scale that encodes two variables
# red for homes and blue for adopt_need 
# the special notation with gather is due to readibility reasons
bivariate_color_scale <- tibble(
  "3 - 3" = "#3F2949", # high inequality, high income
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1", # low inequality, high income
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A", # medium inequality, medium income
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E", # high inequality, low income
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0" # low inequality, low income
) %>%
  gather("group", "fill")

# cut into groups defined above and join fill
bar <- data %>%
  mutate(
    var1_quantiles = cut(
      var1,
      breaks = quantiles_var1,
      include.lowest = TRUE
    ),
    var2_quantiles = cut(
      var2,
      breaks = quantiles_var2,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group = paste(
      as.numeric(var1_quantiles), "-",
      as.numeric(var2_quantiles)
    )
  ) %>%
  # we now join the actual hex values per "group"
  # so each municipality knows its hex value based on the his gini and avg
  # income value
  left_join(bivariate_color_scale, by = "group")

foo <- bar %>% select(fips,fill)

# the special notation with gather is due to readibility reasons
colours = c("#3F2949","#435786","#4885C1","#77324C","#806A8A","#89A1C8","#AE3A4E","#BC7C8F","#CABED0")

group = c("3 - 3", "2 - 3", "1 - 3", "3 - 2", "2 - 2", "1 - 2", "3 - 1","2 - 1", "1 - 1")
fill = c("#3F2949","#435786","#4885C1","#77324C","#806A8A","#89A1C8","#AE3A4E","#BC7C8F","#CABED0")

leg_data <- data.frame(group, fill)

# separate the groups
leg_data %<>%
  separate(group, into = c("var1", "var2"), sep = " - ") %>%
  mutate(var1 = as.integer(var1),
         var2 = as.integer(var2))
