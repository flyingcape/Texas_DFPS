library(dplyr) 
library(tidyr) 
library(ggplot2) 
library(readr)
library(usmap)
library(stringr)

Children_DFPS <-read.csv("CData/Children_DFPS.csv")
Adopt_Need <-read.csv("CData/Adopt_Need.csv")
Homes <-read.csv("CData/Homes.csv")
Removals <-read.csv("CData/Removals.csv")
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

color_map = c("#3b4994","#8c62aa","#be64ac",
              "#5698b9","#a5add3","#dfb0d6",
              "#5ac8c8","#ace4e4","#e8e8e8")

x_text = "More Removals ->"
y_text = "More Homes ->"

# x data 
data$Removals_Total_County_per1K = (data$Removals_Total_County/data$Child_Population)*1000
data$Removals_Total_County_per1K[is.na(data$Removals_Total_County_per1K)] <- 0
ex4 <- filter(data, Removals_Total_County_per1K != 0)
x = data$Removals_Total_County_per1K

# y data
data$Homes_Total_County_per1K = (data$Homes_Total_County/data$Total_Population)*100000
data$Homes_Total_County_per1K[is.na(data$Homes_Total_County_per1K)] <- 0
ex2 <- filter(data, Homes_Total_County_per1K != 0)
y = data$Homes_Total_County_per1K

x_brk_points = c(quantile(ex4$Removals_Total_County_per1K, 0.30),
                 quantile(ex4$Removals_Total_County_per1K, 0.60))
y_brk_points = c(quantile(ex2$Homes_Total_County_per1K, 0.30),
                 quantile(ex2$Homes_Total_County_per1K, 0.60))

# ---------------------------
quantiles_x <- quantile(x, 
                        probs = c(0,ecdf(x)(x_brk_points[1]),
                                  ecdf(x)(x_brk_points[2]),1), na.rm = TRUE)
# create 3 buckets for variable 2
quantiles_y <- quantile(y, 
                        probs = c(0,ecdf(y)(y_brk_points[1]),
                                  ecdf(y)(y_brk_points[2]),1), na.rm = TRUE)

# create color scale that encodes two variables
bivariate_color_scale <- tibble(
  "3 - 3" = color_map[1], # high X, high Y
  "2 - 3" = color_map[2],
  "1 - 3" = color_map[3], # low X, high Y
  "3 - 2" = color_map[4],
  "2 - 2" = color_map[5], # medium X, medium Y
  "1 - 2" = color_map[6],
  "3 - 1" = color_map[7], # high X, low Y
  "2 - 1" = color_map[8],
  "1 - 1" = color_map[9] # low X, low Y
) %>%
  gather("group", "fill")

# cut into groups defined above and join fill
bar <- data %>%
  mutate(
    x_quantiles = cut(
      x,
      breaks = quantiles_x,
      include.lowest = TRUE
    ),
    y_quantiles = cut(
      y,
      breaks = quantiles_y,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group = paste(
      as.numeric(x_quantiles), "-",
      as.numeric(y_quantiles)
    )
  ) %>%
  # we now join the actual hex values per "group"
  # so each county knows its hex value based on x and y
  left_join(bivariate_color_scale, by = "group")

foo = bar %>% select(fips,fill)
colours = color_map

# CREATE THE LEGEND
group = c("3 - 3", "2 - 3", "1 - 3", 
          "3 - 2", "2 - 2", "1 - 2", 
          "3 - 1","2 - 1", "1 - 1")
fill = color_map
leg_data <- data.frame(group, fill)

# Create the legend
# separate the groups
leg_data %<>%
  separate(group, into = c("x", "y"), sep = " - ") %>%
  mutate(x = as.integer(x),
         y = as.integer(y))

legend <- ggplot() +
  geom_tile(
    data = leg_data,
    mapping = aes(
      x = x,
      y = y,
      fill = color_map)
  ) +
  scale_fill_identity() +
  ylab(y_text) +
  xlab(x_text) +
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), axis.text.y=element_blank(), 
        axis.title = element_text(size = 6)) + 
  # theme(axis.text.x = element_text(color="black", size=12, angle=0)) +
  # quadratic tiles
  coord_fixed() + 
  theme(plot.background = element_rect(color = "#6d9eeb", fill = "#6d9eeb"))
