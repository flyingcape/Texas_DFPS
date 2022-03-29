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
TX_counties <-read.csv("CData/texas0.csv")
Children_TX <-read.csv("CData/Children_TX.csv")
Adopt_Need <-read.csv("CData/Adopt_Need.csv")
Homes <-read.csv("CData/Homes.csv")
Removals <-read.csv("CData/Removals.csv")
map_data <- read.csv("CData/Shiny_map_data.csv")

# ------------------------
# Functions to create the Figures
# ------------------------
create_homes_removals_plot <- function(map_data, year) {
  
  color_map = c("#da4f58","#eda8a9","#e8e7e6",
                "#a44c55","#b09ca5","#a4d5e1",
                "#5a3f49","#5b7d8f","#43aac0")
  x_text = "More Removals ->"
  y_text = "More Homes ->"  
  
  # Filter for Chosen Year
  map_data_year <- filter(map_data, Year == year)
  
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
    coord_fixed() + 
    theme(plot.background = element_rect(color = "white", fill = "white"))
  
  bivar_plot <- ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    #draw_plot(legend, 0.05, 0.05, 0.3, 0.3) + 
    theme(plot.background = element_rect(color = "white", fill = "white"))
  
  return(bivar_plot)
} # END OF FUNCTION # 1

create_homes_plot_simple <- function(map_data, year) {
  
  legend_text = "Homes per 100K residents"
  
  # Filter for Chosen Year
  map_data_year <- filter(map_data, Year == year)
  
  # Building the parts of the dataframe we will work with
  mdata <- map_data_year %>% select("Year", "County", 
                                    "Region", "fips", "lon", "lat")
  
  # create dataframe we will work with to plot
  var1 = map_data_year$Homes_Total_County_per100K
  data <- cbind(mdata,var1)  
  
  data$var1[data$var1==0] <- NA 
  
  # --------------------------------
  # Compute scale
  # --------------------------------
  colours = c("#a9a9a9","#BE0D73","#F25E74","#FF8884","#34A1C7","#026178")
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
        na.translate = FALSE) + 
    theme(plot.background = element_rect(color = "white", fill = "white")) +
    theme(legend.text=element_text(size=14)) + 
    theme(legend.background = element_rect(color = "white", fill = "white")) + 
    theme(legend.position="bottom")
  
  return(univar_plot)
} # END OF FUNCTION # 2

create_removals_plot_simple <- function(map_data, year) {
  
  legend_text = "Removals per 1K Children"
  
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
  colours = c("#a9a9a9","#eac259","#f39f62","#f67300","#e52835","#810301")
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
      na.translate = FALSE) + 
    theme(plot.background = element_rect(color = "white", fill = "white")) +
    theme(legend.text=element_text(size=14)) + 
    theme(legend.background = element_rect(color = "white", fill = "white")) + 
    theme(legend.position="bottom")
  
  return(univar_plot)
} # END OF FUNCTION # 3

# The tiny county plots
create_homes_plot_yourcounty <- function(map_data, year, county) {
  
  # Filter for Chosen Year
  map_data_year <- filter(map_data, Year == year)
  
  # Building the parts of the dataframe we will work with
  mdata <- map_data_year %>% select("Year", "County", 
                                    "Region", "fips", "lon", "lat")
  
  # create dataframe we will work with to plot
  var1 = map_data_year$Homes_Total_County_per100K
  data <- cbind(mdata,var1)  
  
  data$var1[data$var1==0] <- NA 
  
  # --------------------------------
  # Compute scale
  # --------------------------------
  colours = c("#a9a9a9","#BE0D73","#F25E74","#FF8884","#34A1C7","#026178")
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
  # --------------------------------
  indx = which(labels_scale == data$brks[data$County == county])

  if (is.na(data$brks[data$County == county])){
    tiny_plot <-plot_usmap("counties", fill = "white", include = c(data$fips[data$County == county]), color = "black") +
      theme_map() +
      theme(plot.background = element_rect(color = "white", fill = "white")) 
  } else {
    tiny_plot <-plot_usmap("counties", fill = colours[indx], include = c(data$fips[data$County == county]), color = "black") +
      theme_map() +
      theme(plot.background = element_rect(color = "white", fill = "white")) 
  }

  return(tiny_plot)
} # END OF FUNCTION # 4

create_removals_plot_yourcounty <- function(map_data, year, county) {
  
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
  colours = c("#a9a9a9","#eac259","#f39f62","#f67300","#e52835","#810301")
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
  # --------------------------------
  indx = which(labels_scale == data$brks[data$County == county])

  if (is.na(data$brks[data$County == county])){
    tiny_plot <-plot_usmap("counties", fill = "white", include = c(data$fips[data$County == county]), color = "black") +
      theme_map() +
      theme(plot.background = element_rect(color = "white", fill = "white")) 
  } else {
      tiny_plot <-plot_usmap("counties", fill = colours[indx], include = c(data$fips[data$County == county]), color = "black") +
        theme_map() +
        theme(plot.background = element_rect(color = "white", fill = "white")) 
  }
  
  return(tiny_plot)
} # END OF FUNCTION # 5

create_bivariate_plot_yourcounty <- function(map_data, year, county) {
  
  color_map = c("#da4f58","#eda8a9","#e8e7e6",
                "#a44c55","#b09ca5","#a4d5e1",
                "#5a3f49","#5b7d8f","#43aac0")

  # Filter for Chosen Year
  map_data_year <- filter(map_data, Year == year)
  
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
  county_fill = foo$fill[foo$fips == (data$fips[data$County == county])]

  if (is.na(data$var1[data$County == county])){
    tiny_plot <-plot_usmap("counties", fill = "white", include = c(data$fips[data$County == county]), color = "black") +
      theme_map() +
      theme(plot.background = element_rect(color = "white", fill = "white")) 
  } else {
    tiny_plot <-plot_usmap("counties", fill = county_fill, include = c(data$fips[data$County == county]), color = "black") +
      theme_map() +
      theme(plot.background = element_rect(color = "white", fill = "white")) 
  }

  return(tiny_plot)
} # END OF FUNCTION # 6

extract_priority_list <- function(map_data, year) {
  
  color_map = c("#da4f58","#eda8a9","#e8e7e6",
                "#a44c55","#b09ca5","#a4d5e1",
                "#5a3f49","#5b7d8f","#43aac0")
  
  # Filter for Chosen Year
  map_data_year <- filter(map_data, Year == year)
  
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
} # END OF FUNCTION # 7