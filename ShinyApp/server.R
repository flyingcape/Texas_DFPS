
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
    output$plot3 <-renderPlot(
        
        plot_usmap(data = map_data_year, values = "DFPS_children_Total_County", include = c("TX"), color = "black") + 
            scale_fill_steps(name = "Total", n.breaks = 8, low = "grey", high = "navy", na.value = "white") +
            labs(x = NULL, 
                 y = NULL, 
                 caption = "Data Source: data.texas.gov") + 
            theme(legend.position="right", 
                  plot.caption = element_text(color = "black", size = 12))
    )
    
    output$plot4 <-renderPlot(
        
        Adopt_Need %>% filter(Year == "2020") %>% 
            group_by(`Placement Intention`) %>% summarise(Total = sum(Count)) %>% 
            ggplot(aes(x =  `Placement Intention`, y = Total, fill = `Placement Intention`)) +
            geom_bar(stat = 'identity') + 
            theme(axis.text.x = element_text(color="black", size=9, angle=0)) + 
            ggtitle("State of Texas") + 
            scale_fill_manual(values=c("#53b69c", "#238e9c", "#2e4873")) +
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
            scale_fill_manual(values=c("#53b69c", "#238e9c", "#2e4873")) +
            theme(axis.title.y = element_blank()) + 
            theme(legend.position = "none") + 
            theme(axis.ticks.x = element_blank(), 
                  axis.text.x = element_blank()) +
            xlab("") +
            theme(plot.background = element_rect(fill = "light blue"))
    )
    
    output$plot6 <-renderPlot(
        
        Homes %>% filter(Year == "2020") %>% 
            group_by(Type) %>% summarise(Total = sum(Count)) %>% ggplot(aes(x =  Type, y = Total, fill = Type)) +
            geom_bar(stat = 'identity') + 
            theme(axis.text.x = element_text(color="black", size=9, angle=0)) + 
            ggtitle("State of Texas") + 
            scale_fill_manual(values=c("#53b69c", "#238e9c", "#2e4873")) +
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
            scale_fill_manual(values=c("#53b69c", "#238e9c", "#2e4873")) +
            theme(axis.title.y = element_blank()) + 
            theme(legend.position = "none") + 
            theme(axis.ticks.x = element_blank(), 
                  axis.text.x = element_blank()) +
            xlab("") +
            theme(plot.background = element_rect(fill = "light blue"))
    )

    output$plot8a <-renderPlot(
        
        ggplot() +
            geom_tile(
                data = leg_data,
                mapping = aes(
                    x = var1,
                    y = var2,
                    fill = fill)
            ) +
            scale_fill_identity() +
            ylab("More Homes ->") +
            xlab(expression(atop("More Children Waiting", paste("for adoption ->")))) +
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
