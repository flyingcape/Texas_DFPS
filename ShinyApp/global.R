library(shiny)
library(dplyr) 
library(tidyr) 
library(ggplot2) 
library(readr)
library(maps)
library(usmap)
library(gridExtra)
library(biscale)
library(cowplot)
library(sf)
library(shinythemes)

# ------------------------
# import data
# ------------------------
Children_TX <-read.csv("CData/Children_TX.csv")
Homes <-read.csv("CData/Homes.csv")
Removals <-read.csv("CData/Removals.csv")
map_data <- read.csv("CData/Shiny_map_data.csv")
states <- read.csv("CData/states.csv") # used to create outline of Texas panhandle on maps

# Merging Data to Create Splash Page Plot #1
Removals_Year <- Removals %>% group_by(Year, Removal.Stage) %>% summarise(Total_Removals = sum(Removals))
Child_Population_Year <- map_data %>% group_by(Year) %>% summarise(Child_Pop = sum(Child_Population))
Removals_Population_Year <- Removals_Year %>% merge(Child_Population_Year,by=c("Year")) 
Removals_Population_Year$RemovalsPer1K = (Removals_Population_Year$Total_Removals/Child_Population_Year$Child_Pop)*1000

# Merging Data to Create Splash Page Plot #2 
Homes_Year <- Homes %>% filter(CBC == "DFPS") %>% group_by(Year, Type) %>% summarise(Total_Homes = sum(Count))
Population_Year <- map_data %>% filter(CBC == "DFPS") %>% group_by(Year) %>% summarise(Total_Pop = sum(Total_Population))
Homes_Population_Year <- Homes_Year %>% merge(Population_Year,by=c("Year")) 
Homes_Population_Year$HomesPer100K = (Homes_Population_Year$Total_Homes/Homes_Population_Year$Total_Pop)*100000

# ------------------------
# Functions to create the Figures
# ------------------------
create_homes_removals_plot <- function(map_data, year, states) {
  
  color_map = c("#da4f58","#eda8a9","#e8e7e6",
                "#a44c55","#b09ca5","#a4d5e1",
                "#5a3f49","#5b7d8f","#43aac0")
  
  # Filter for Chosen Year
  map_data_year <- filter(map_data, Year == year)
  
  fips_list = (map_data_year$fips[map_data_year$CBC=="CBC"])
  
  # -----------------------------------------
  # The breaks:
  x_brk_points = c(3, 5)   # removals
  y_brk_points = c(6, 11)  # homes
  # -----------------------------------------
  
  # Building the parts of the dataframe we will work with
  mdata <- map_data_year %>% select("Year", "County", 
                                    "Region", "fips", "lon", "lat","CBC")
  
  # create dataframe we will work with to plot
  var1 = map_data_year$Removals_Total_County_per1K
  var2 = map_data_year$Homes_Total_County_per100K
  data <- cbind(mdata,var1,var2)  
  
  x = as.numeric(unlist(data[8]))
  y = as.numeric(unlist(data[9]))
  
  # create 3 buckets for variable 1
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
      group = paste(
        as.numeric(x_quantiles), "-",
        as.numeric(y_quantiles)
      )
    ) %>%
    # we now join the actual hex values per "group"
    # so each county has a hex value based on x and y
    left_join(bivariate_color_scale, by = "group")
  
  foo = bar %>% select(fips,fill)
  colours = color_map
  
  ## ----------------------------------------------------------------
  bivar_plot <- plot_usmap(data = foo, values = "fill", include = c("TX"), exclude = fips_list, color = "black", ) +  
    scale_fill_manual(
      values = colours,
      breaks = colours,
      drop = FALSE, na.value = "white") + theme(legend.position = "none") +
    theme(plot.background = element_rect(color = "white", fill = "white"))  +
    geom_path(data = states, 
              aes(x = long.1,
                  y = lat.1),
              color = "black") 
  
  return(bivar_plot)
} # END OF FUNCTION # 1

create_homes_plot_simple <- function(map_data, year, states) {
  
  legend_text = "FAD Homes per 100K residents"
  
  # Filter for Chosen Year
  map_data_year <- filter(map_data, Year == year)
  
  # Modifications to map plot
  #states <- map_data("state") %>% filter(region == "texas") %>% filter(order > 13025) %>% usmap_transform()
  
  fips_list = (map_data_year$fips[map_data_year$CBC=="CBC"])
  
  # Building the parts of the dataframe we will work with
  mdata <- map_data_year %>% select("Year", "County", 
                                    "Region", "fips", "lon", "lat", "CBC")
  
  # create dataframe we will work with to plot
  var1 = map_data_year$Homes_Total_County_per100K
  data <- cbind(mdata,var1)  
  
  data$var1[data$var1==0] <- NA 
  
  # --------------------------------
  # Compute scale
  # --------------------------------
  colours = c("#a9a9a9","#3f2e84","#1032a1","#25999b","#007c7f","#015c5d")
  labels <- c()
  brks <- c(0,10, 25, 50, 75, 100, 125)
  for(idx in 1:length(brks)){
    labels <- c(labels,round(brks[idx + 1], 2))
  }
  labels <- labels[1:length(labels)-1]
  # define a new variable on the data set just as above
  data$brks <- cut(data$var1, 
                   breaks = brks, 
                   include.lowest = TRUE, 
                   labels = labels)
  brks_scale <- levels(data$brks)
  labels_scale <- (brks_scale)
  
  labels_scale = append(labels_scale, "ZERO")
  # --------------------------------
  
  univar_plot <-plot_usmap(data = data, values = "brks", include = c("TX"), exclude = fips_list, color = "black") + 
    theme_map() +
    scale_fill_manual(
      values = colours,
      name = legend_text,
      drop = FALSE, 
      labels = labels_scale,
      guide = guide_legend(
        direction = "horizontal",
        keyheight = unit(6, units = "mm"),
        keywidth = unit(100 / length(labels), units = "mm"),
        title.position = 'top',
        title.hjust = 0,
        label.hjust = 1,
        nrow = 1,
        byrow = T,
        reverse = F,
        label.position = "bottom"), 
      na.translate = TRUE,  na.value = "yellow") + 
    theme(plot.background = element_rect(color = "white", fill = "white")) +
    theme(legend.text=element_text(size=14)) + 
    theme(legend.background = element_rect(color = "white", fill = "white")) + 
    theme(legend.position="top")  +
    geom_path(data = states, 
              aes(x = long.1,
                  y = lat.1),
              color = "black") 
  
  return(univar_plot)
} # END OF FUNCTION # 2

create_removals_plot_simple <- function(map_data, year) {
  
  legend_text = "Removals per 1K children"
  
  # Filter for Chosen Year
  map_data_year <- filter(map_data, Year == year)
  
  # Building the parts of the dataframe we will work with
  mdata <- map_data_year %>% select("Year", "County", 
                                    "Region", "fips", "lon", "lat")
  
  # create dataframe we will work with to plot
  var1 = map_data_year$Removals_Total_County_per1K
  data <- cbind(mdata,var1)  
  
  data$var1[data$var1==0] <- NA 
  
  # --------------------------------
  # Compute scale
  # --------------------------------
  colours = c("#a9a9a9","#f07d03","#f55702","#cc202a","#9c2449", "#6e2a67")
  labels <- c()
  brks <- c(0,5, 10, 15, 20, 30, 50)
  for(idx in 1:length(brks)){
    labels <- c(labels,round(brks[idx + 1], 2))
  }
  labels <- labels[1:length(labels)-1]
  # define a new variable on the data set just as above
  data$brks <- cut(data$var1, 
                   breaks = brks, 
                   include.lowest = TRUE, 
                   labels = labels)
  brks_scale <- levels(data$brks)
  labels_scale <- (brks_scale)
  
  labels_scale = append(labels_scale, "ZERO")
  # --------------------------------
  
  univar_plot <-plot_usmap(data = data, values = "brks", include = c("TX"), color = "black") + 
    theme_map() +
    scale_fill_manual(
      values = colours,
      name = legend_text,
      drop = FALSE, 
      labels = labels_scale,
      guide = guide_legend(
        direction = "horizontal",
        keyheight = unit(6, units = "mm"),
        keywidth = unit(100 / length(labels), units = "mm"),
        title.position = 'top',
        title.hjust = 0,
        label.hjust = 1,
        nrow = 1,
        byrow = T,
        reverse = F,
        label.position = "bottom"),
      na.translate = TRUE, na.value = "yellow") + 
    theme(plot.background = element_rect(color = "white", fill = "white")) +
    theme(legend.text=element_text(size=14)) + 
    theme(legend.background = element_rect(color = "white", fill = "white")) + 
    theme(legend.position="top")
  
  return(univar_plot)
} # END OF FUNCTION # 3

extract_priority_list <- function(map_data, year) {
  
  color_map = c("#da4f58","#eda8a9","#e8e7e6",
                "#a44c55","#b09ca5","#a4d5e1",
                "#5a3f49","#5b7d8f","#43aac0")
  
  # Filter for Chosen Year & and only DFPS counties (i.e. no CBC)
  map_data_year <- filter(map_data, (Year == year)&(CBC== "DFPS")) 
  
  # -----------------------------------------
  # The breaks:
  x_brk_points = c(3, 5)   # removals
  y_brk_points = c(6, 11)  # homes
  # -----------------------------------------
  
  # Building the parts of the dataframe we will work with
  mdata <- map_data_year %>% select("Year", "County", 
                                    "Region", "fips", "lon", "lat")
  
  # create dataframe we will work with to plot
  var1 = map_data_year$Removals_Total_County_per1K
  var2 = map_data_year$Homes_Total_County_per100K
  data <- cbind(mdata,var1,var2)  
  
  x = as.numeric(unlist(data[7]))
  y = as.numeric(unlist(data[8]))
  
  # create 3 buckets for variable 1
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
      group = paste(
        as.numeric(x_quantiles), "-",
        as.numeric(y_quantiles)
      )
    ) %>%
    # we now join the actual hex values per "group"
    # so each county has a hex value based on x and y
    left_join(bivariate_color_scale, by = "group")
  
  foo = bar %>% select(fips,fill)
  colours = color_map
  
  # --------------------------------
  the_list = toString(sort(bar$County[bar$fips %in% (foo$fips[foo$fill=="#5a3f49"])]))
  
  return(the_list)
} # END OF FUNCTION # 5

create_highneed_bar_chart <- function(map_data, year) {
  
  color_map = c("#da4f58","#eda8a9","#e8e7e6",
                "#a44c55","#b09ca5","#a4d5e1",
                "#5a3f49","#5b7d8f","#43aac0")

  # Filter for Chosen Year & and only DFPS counties (i.e. no CBC)
  map_data_year <- filter(map_data, (Year == year) & (CBC== "DFPS"))
  
  # -----------------------------------------
  # The breaks:
  x_brk_points = c(3, 5)   # removals
  y_brk_points = c(6, 11)  # homes
  # -----------------------------------------
  
  # Building the parts of the dataframe we will work with
  mdata <- map_data_year %>% select("Year", "County", 
                                    "DFPS_Region", "fips", "lon", "lat")
  
  # create dataframe we will work with to plot
  var1 = map_data_year$Removals_Total_County_per1K
  var2 = map_data_year$Homes_Total_County_per100K
  data <- cbind(mdata,var1,var2)  
  
  x = as.numeric(unlist(data[7]))
  y = as.numeric(unlist(data[8]))
  
  # create 3 buckets for variable 1
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
      group = paste(
        as.numeric(x_quantiles), "-",
        as.numeric(y_quantiles)
      )
    ) %>%
    # we now join the actual hex values per "group"
    # so each county has a hex value based on x and y
    left_join(bivariate_color_scale, by = "group")
  
  foo = bar %>% select(fips,fill)
  colours = color_map
  
  ## ----------------------------------------------------------------
  map <- plot_usmap(data = foo, values = "fill", include = c("TX"), color = "black", ) +  
    scale_fill_manual(
      values = colours,
      breaks = colours,
      drop = FALSE, na.value = "white") + theme(legend.position = "none")
  
  # CREATE THE LEGEND
  group = c("3 - 3", "2 - 3", "1 - 3", 
            "3 - 2", "2 - 2", "1 - 2", 
            "3 - 1","2 - 1", "1 - 1")
  fill = color_map
  leg_data <- data.frame(group, fill)
  
  # Create bar plot
  high_need_df <- filter(bar, fill=="#5a3f49") %>% select("Year", "County", 
                                                          "DFPS_Region")
  table(high_need_df$DFPS_Region)
  high_need_df <- within(high_need_df, 
                         DFPS_Region<- factor(DFPS_Region, 
                                         levels=names(sort(table(DFPS_Region), 
                                                           decreasing=FALSE))))
  bar_plot <- ggplot(high_need_df, aes(y = DFPS_Region)) +
    geom_bar(fill="#5a3f49") + theme(axis.text.x = element_text(color="black", size=12, angle=0)) + theme(axis.text.y = element_text(color="black", size=12, angle=0)) + labs(y = "DFPS\n Region", x = "High-need County Count") + theme(text = element_text(size = 14)) + theme(axis.title.y=element_text(angle=0,vjust=0.5)) + scale_x_continuous(breaks = c(0,5,10,15)) 
  
  return(bar_plot)
}
