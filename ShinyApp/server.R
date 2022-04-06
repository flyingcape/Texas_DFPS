
# Define server logic required to create a plot
shinyServer(function(input, output) {

    # Homes across the years (statewide data)
    output$Homes_plot1 <-renderPlot(
        
        Homes_Population_Year %>% filter(Year %in% c(2011,2013,2015,2017,2019,2021)) %>% ggplot(aes(x =  Year, y = HomesPer100K, fill=Type)) + 
            geom_bar(stat = 'identity') + 
            ggtitle("") + 
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(axis.text.x = element_text(color="black", size=12, angle=0)) + 
            theme(axis.text.y = element_text(color="black", size=12, angle=0)) + 
            theme(axis.title = element_text(size = 16)) +
            theme(legend.text = element_text(size = 14)) +
            theme(legend.title = element_text(size = 14))  +
            theme(plot.title = element_text(size = 14)) +
            scale_x_continuous(breaks = c(2011,2013,2015,2017,2019,2021)) + ylab("FAD Homes\nper 100K\nresidents") + theme(axis.title.y=element_text(angle=0,vjust=0.55)) + 
            scale_fill_manual(name = "Home Type", values = c("#636363","#aeaeae","black"))
    )
    
    # Removals across the years (statewide data)
    output$Removals_Plot1 <-renderPlot(
        
        Removals_Population_Year %>% filter(Year %in% c(2011,2013,2015,2017,2019,2021)) %>% ggplot(aes(x =  Year, y = RemovalsPer1K, fill=Removal.Stage)) + 
            geom_bar(stat = 'identity') + 
            ggtitle("") + 
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(axis.text.x = element_text(color="black", size=12, angle=0)) + 
            theme(axis.text.y = element_text(color="black", size=12, angle=0)) + 
            theme(axis.title = element_text(size = 16)) +
            theme(legend.text = element_text(size = 14)) +
            theme(legend.title = element_text(size = 14))  +
            theme(plot.title = element_text(size = 14)) +
            scale_x_continuous(breaks = c(2011,2013,2015,2017,2019,2021)) + ylab("     Removals   \n     per 1K   \n     children   ") + theme(axis.title.y=element_text(angle=0,vjust=0.50)) + ylim(0,3.01) +
            scale_fill_manual(name = "Removal Stage", values = c("#55565B","black"))
    )

    output$map1 <-renderPlot({
        create_homes_plot_simple(map_data, input$TheYear, states)
    })

    output$map2 <-renderPlot({
        create_removals_plot_simple(map_data, input$TheYear)
    })
    
    output$map3 <-renderPlot({
        create_homes_removals_plot(map_data, input$TheYear, states)
    })
    
    output$Bivariate_text3 <- renderText({ 
        paste("The highest priority counties: ", extract_priority_list(map_data, input$TheYear))
        
    })  
    
    output$HighNeed_barplot <-renderPlot({
        create_highneed_bar_chart(map_data, input$TheYear)
    })  
    
    url <- a("View Project Blog", href="https://nycdatascience.com/blog/student-works/a-shortage-of-texas-foster-and-adoptive-homes-an-r-data-analysis/")
    output$tab <- renderUI({
        tagList("Author: Mandy McClintock | R Data Analysis Project  |  NYC Data Science Academy | ", url)
    })
})
