
# Define server logic required to create a plot
shinyServer(function(input, output) {
    
    output$selected_var <- renderText({ 
        paste("For Your County, you selected", input$YourCounty)
    })
    output$selected_var2 <- renderText({ 
        paste("For Your County, you selected", input$YourCounty)
    })
    
    output$plot1 <-renderPlot(
        
        Children_DFPS %>% filter(Year == "2020") %>% 
            group_by(Age_Group) %>% summarise(Total = sum(Count)) %>% ggplot(aes(x =  Age_Group, y = Total, fill = Age_Group)) +
            geom_bar(stat = 'identity') + 
            theme(axis.text.x = element_text(color="black", size=9, angle=0)) + 
            ggtitle("State of Texas") + 
            scale_fill_manual(values=c("#53b69c", "#238e9c", "#2e4873")) +
            theme(legend.position = "right") + 
            theme(axis.ticks.x = element_blank(), 
                  axis.text.x = element_blank()) +
            xlab("") +
            labs(fill = "Age Groups") +
            theme(plot.background = element_rect(fill = "gray")) 
    )
    
    output$plot2 <-renderPlot(
    
        Children_DFPS %>% filter(Year == "2020") %>% 
            filter(County == input$YourCounty) %>% group_by(Year, Age_Group) %>% summarise(Total = sum(Count)) %>% 
            ggplot(aes(x =  Age_Group, y = Total, fill = Age_Group)) + 
            geom_bar(stat = 'identity') + 
            theme(axis.text.x = element_text(color="black", size=9, angle=0)) + 
            ggtitle(paste(input$YourCounty," County",sep="")) + 
            scale_fill_manual(values=c("#53b69c", "#238e9c", "#2e4873")) +
            theme(axis.title.y = element_blank()) + 
            theme(legend.position = "none") + 
            theme(axis.ticks.x = element_blank(), 
                  axis.text.x = element_blank()) +
            xlab("") +
            theme(plot.background = element_rect(fill = "light blue"))
    )
    
    output$plot9 <-renderPlot(
        
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
            theme(plot.title = element_text(size = 14)) 
    )
    
    output$plot3 <-renderPlot(
        
        Removals %>% group_by(Year,Removal.Stage) %>% summarise(Total = sum(Removals)) %>% ggplot(aes(x =  Year, y = Total, fill = Removal.Stage)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(color="black", size=12, angle=45)) + 
            theme(axis.text.y = element_text(color="black", size=12, angle=0)) + 
            theme(axis.title = element_text(size = 16)) +
            theme(legend.text = element_text(size = 14)) +
            theme(legend.title = element_text(size = 14))  +
            theme(plot.title = element_text(size = 14)) +
            ggtitle("") +
            scale_x_continuous(name="Year", limits=c(2010.5, 2020.5), breaks = c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)) + 
            guides(fill=guide_legend(title="Removal Stage"))
    )
    
    output$plot4 <-renderPlot(
        
        Adopt_Need %>% filter(Year == "2020") %>% 
            group_by(`Placement Intention`) %>% summarise(Total = sum(Count)) %>% 
            ggplot(aes(x =  `Placement Intention`, y = Total, fill = `Placement Intention`)) +
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
    
    output$plot5 <-renderPlot(
        
        Adopt_Need %>% filter(Year == "2020") %>% 
            filter(County == input$YourCounty) %>% group_by(`Placement Intention`) %>% summarise(Total = sum(Count)) %>% 
            ggplot(aes(x =  `Placement Intention`, y = Total, fill = `Placement Intention`)) + 
            geom_bar(stat = 'identity') + 
            theme(axis.text.x = element_text(color="black", size=9, angle=0)) + 
            ggtitle(paste(input$YourCounty," County",sep="")) + 
            scale_fill_manual(values=c("blue", "#028140", "#2e4873")) +
            theme(axis.title.y = element_blank()) + 
            theme(legend.position = "none") + 
            theme(axis.ticks.x = element_blank(), 
                  axis.text.x = element_blank()) +
            xlab("") +
            theme(plot.background = element_rect(fill = "#dbc9c3"))
    )
    
    output$plot6 <-renderPlot(
        
        Homes %>% filter(Year == "2020") %>% 
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
    
    output$plot7 <-renderPlot(
        
        Homes %>% filter(Year == "2020") %>% 
            filter(County == input$YourCounty) %>% group_by(Type) %>% summarise(Total = sum(Count)) %>% 
            ggplot(aes(x =  Type, y = Total, fill = Type)) + 
            geom_bar(stat = 'identity') + 
            theme(axis.text.x = element_text(color="black", size=9, angle=0)) + 
            ggtitle(paste(input$YourCounty," County",sep="")) + 
            scale_fill_manual(values=c("blue", "#028140", "#2e4873")) +
            theme(axis.title.y = element_blank()) + 
            theme(legend.position = "none") + 
            theme(axis.ticks.x = element_blank(), 
                  axis.text.x = element_blank()) +
            xlab("") +
            theme(plot.background = element_rect(fill = "#dbc9c3"))
    )

    output$plot8a <-renderPlot(
        
        ggplot() +
            geom_tile(
                data = leg_data,
                mapping = aes(
                    x = x,
                    y = y,
                    fill = fill)
            ) +
            scale_fill_identity() +
            ylab("More Homes ->") +
            xlab("More Removals  ->") +
            theme(axis.ticks.y = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.x = element_blank(), axis.text.y=element_blank(), 
                  axis.title = element_text(size = 12)) + 
            coord_fixed() 
    )       
    
    output$plot8 <-renderPlot(
        
        plot_usmap(data = foo, values = "fill", include = c("TX"), color = "black", ) + 
            labs(x = NULL, 
                 y = NULL, 
                 title = "By Texas County", 
                 caption = "Data Source: data.texas.gov") +
            theme(legend.position="right", 
                  plot.title = element_text(color = "black", size = 14),
                  plot.caption = element_text(color = "black", size = 12)) +
            scale_fill_manual(
                values = colours,
                breaks = colours,
                name = "Foobar",
                drop = FALSE, na.value = "white") + theme(legend.position = "none") 
    )    
    
        
})
