
# Define server logic required to create a plot
shinyServer(function(input, output) {
    output$Homes_text0 <- renderText({ 
        paste(input$YourCounty, " County ( in ", input$TheYear, ")")
    })   
    output$Homes_text1 <- renderText({ 
        paste("The county had ", round(map_data$Homes_Total_County[map_data$Year == input$TheYear & map_data$County == input$YourCounty][1],0)," FAD homes.")
    })    
    output$Homes_text2 <- renderText({ 
        paste("For ",round(map_data$Total_Population[map_data$Year == input$TheYear & map_data$County == input$YourCounty][1],0), " residents, this is ", round(map_data$Homes_Total_County_per100K[map_data$Year == input$TheYear & map_data$County == input$YourCounty][1],1), " homes per 100K residents.")
    })
    
    output$Homes_plot3 <-renderPlot(
        
        Homes %>% filter(Year == input$TheYear) %>% 
            filter(County == input$YourCounty) %>% group_by(Type) %>% summarise(Total = sum(Count)) %>% 
            ggplot(aes(x =  Type, y = Total, fill = Type)) + 
            geom_bar(stat = 'identity') + 
            theme(axis.text.x = element_text(color="black", size=9, angle=0)) + 
            scale_fill_manual("Home Type",values=c("blue", "#028140", "#2e4873")) +
            ylab("Number\nof FAD\n Homes") + xlab(" ") +
            theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
            theme(legend.position = "right") + 
            theme(axis.ticks.x = element_blank(), 
                  axis.text.x = element_blank()) +
            theme(plot.background = element_rect(fill = "gray")) + 
            ggtitle(" ") 
    )
    
    output$Homes_text3 <- renderText({ 
        paste("In ", input$TheYear,", ",input$YourCounty, " County had", round(map_data$Homes_Total_County[map_data$Year == input$TheYear & map_data$County == input$YourCounty][1],0), "FAD homes.")
    }) 

    output$Homes_text4 <- renderText({ 
        paste("That is ", round(map_data$Homes_Total_County_per100K[map_data$Year == input$TheYear & map_data$County == input$YourCounty][1],1), " homes per 100K residents.")
    }) 

    output$County_text0 <- renderText({ 
        paste(input$YourCounty, " County")
    }) 
        
    output$Removals_text1 <- renderText({ 
        paste("The county had ", round(map_data$Removals_Total_County[map_data$Year == input$TheYear & map_data$County == input$YourCounty][1],0)," removals.")
    })    
    output$Removals_text2 <- renderText({ 
        paste("For ", round(map_data$Child_Population[map_data$Year == input$TheYear & map_data$County == input$YourCounty][1],0), " children residents, this is ", round(map_data$Removals_Total_County_per1K[map_data$Year == input$TheYear & map_data$County == input$YourCounty][1],1), " removals per 1K children.")
    })
    
    output$Removals_Plot2 <-renderPlot(
        
        Removals %>% filter(Year == input$TheYear) %>% 
            filter(County == input$YourCounty) %>% group_by(`Removal.Stage`) %>% summarise(Total = sum(Removals)) %>% 
            ggplot(aes(x =  `Removal.Stage`, y = Total, fill = `Removal.Stage`)) + 
            geom_bar(stat = 'identity') + 
            theme(axis.text.x = element_text(color="black", size=9, angle=0)) + 
            scale_fill_manual("Removal Stage", values=c("blue", "#028140", "#2e4873")) +
            theme(axis.title.y = element_blank()) + 
            theme(legend.position = "right") + 
            theme(axis.ticks.x = element_blank(), 
                  axis.text.x = element_blank()) +
            ylab("Number of\n Removals") + xlab(" ") +
            theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
            theme(plot.background = element_rect(fill = "gray")) + 
            ggtitle(" ") 
    )
    
    output$Removals_text3 <- renderText({ 
        paste("In ", input$TheYear,", ",input$YourCounty, " County had", round(map_data$Removals_Total_County[map_data$Year == input$TheYear & map_data$County == input$YourCounty][1],0), " children removed.")
    }) 
    
    output$Removals_text4 <- renderText({ 
        paste("That is ", round(map_data$Removals_Total_County_per1K[map_data$Year == input$TheYear & map_data$County == input$YourCounty][1],1), " removals per 1K children.")
    }) 
    
    output$Adopt_text1 <- renderText({ 
        paste("The county had ", round(map_data$Adopt_Need_Total_County[map_data$Year == input$TheYear & map_data$County == input$YourCounty][1],0)," children awaiting adoption.")
    })    

    output$AdoptNeed_Plot1 <-renderPlot(
        
        Adopt_Need %>% filter(Year == input$TheYear) %>% 
            filter(County == input$YourCounty) %>% group_by(`Placement.Intention`) %>% summarise(Total = sum(Count)) %>% 
            ggplot(aes(x =  `Placement.Intention`, y = Total, fill = `Placement.Intention`)) + 
            geom_bar(stat = 'identity') + 
            theme(axis.text.x = element_text(color="black", size=9, angle=0)) + 
            scale_fill_manual("Placement\n Intention", values=c("blue", "#028140", "#2e4873")) +
            theme(axis.title.y = element_blank()) + 
            theme(legend.position = "right") + 
            theme(axis.ticks.x = element_blank(), 
                  axis.text.x = element_blank()) +
            ylab("Number of\n Children\n Awaiting\n Adoption") + xlab(" ") +
            theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
            theme(plot.background = element_rect(fill = "gray")) +
            ggtitle(" ") 
    )
    
    # Homes across the years (state-wide data)
    output$Homes_plot1 <-renderPlot(
        
        Homes %>% group_by(Year, Type) %>% summarise(Total = sum(Count)) %>% 
            ggplot(aes(x =  Year, y = Total, fill = Type)) + 
            geom_bar(stat = 'identity') + 
            ggtitle("") +
            scale_x_continuous(name="Year", limits=c(2010.5, 2020.5), breaks = c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)) + 
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(axis.text.x = element_text(color="black", size=12, angle=30)) + 
            theme(axis.text.y = element_text(color="black", size=14, angle=0)) + 
            theme(axis.title = element_text(size = 16)) +
            theme(legend.text = element_text(size = 14)) +
            theme(legend.title = element_text(size = 14))  +
            theme(plot.title = element_text(size = 14)) + 
            scale_fill_manual(values = c("red", "green", "blue"))
    )
    
    # Removals across the years (state-wide data)
    output$Removals_Plot1 <-renderPlot(
        
        Removals %>% group_by(Year, Removal.Stage) %>% summarise(Total = sum(Removals)) %>% ggplot(aes(x =  Year, y = Total, fill = Removal.Stage)) + 
            geom_bar(stat = 'identity') + 
            theme(axis.text.x = element_text(color="black", size=12, angle=45)) + 
            theme(axis.text.y = element_text(color="black", size=12, angle=0)) + 
            theme(axis.title = element_text(size = 16)) +
            theme(legend.text = element_text(size = 14)) +
            theme(legend.title = element_text(size = 14))  +
            theme(plot.title = element_text(size = 14)) +
            ggtitle("") +
            scale_x_continuous(name="Year", limits=c(2010.5, 2020.5), breaks = c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)) +
            scale_fill_manual(values = c("orange","purple"))
    )

    output$map1 <-renderPlot({
        create_homes_plot_simple(map_data, input$TheYear)
    })
    output$map1_again <-renderPlot({
        create_homes_plot_simple(map_data, input$TheYear)
    })
    
    output$map2 <-renderPlot({
        create_removals_plot_simple(map_data, input$TheYear)
    })

    output$map2_again <-renderPlot({
        create_removals_plot_simple(map_data, input$TheYear)
    })
    
    output$map3 <-renderPlot({
        create_homes_removals_plot(map_data, input$TheYear)
    })
    
    output$map3_again <-renderPlot({
        create_homes_removals_plot(map_data, input$TheYear)
    })
    
    output$map_tiny1 <-renderPlot({
        create_homes_plot_yourcounty(map_data, input$TheYear, input$YourCounty)
    })
    
    output$map_tiny2 <-renderPlot({
        create_removals_plot_yourcounty(map_data, input$TheYear, input$YourCounty)
    })    
    
    output$map_tiny3 <-renderPlot({
        create_bivariate_plot_yourcounty(map_data, input$TheYear, input$YourCounty)
    })  
    
    output$Homes_plot2 <-renderPlot(
        
        Homes %>% filter(Year == input$TheYear) %>% 
            group_by(Type) %>% summarise(Total = sum(Count)) %>% ggplot(aes(x =  Type, y = Total, fill = Type)) +
            geom_bar(stat = 'identity') + 
            theme(axis.text.x = element_text(color="black", size=9, angle=0)) + 
            ggtitle("State of Texas") + 
            scale_fill_manual(values=c("blue", "#028140", "#2e4873")) +
            theme(legend.position = "right") + 
            theme(axis.ticks.x = element_blank(), 
                  axis.text.x = element_blank()) +
            xlab("") +
            theme(plot.background = element_rect(fill = "gray")) 
    )
    
    output$Bivariate_text3 <- renderText({ 
        extract_priority_list(map_data, input$TheYear)
    })  
    
})
