# Define UI for application 
shinyUI(fluidPage(
    titlePanel("\bTexas Foster Care"),
    
    sidebarLayout(
        
        sidebarPanel(
            selectizeInput(inputId = "YourCounty",
                           label = "Choose Your County",
                           choices = unique(Children_DFPS$County))
        ),
        
        mainPanel(
        # -----------------------------------
        tabsetPanel(
            tabPanel("A Shortage of Homes", 
                     h2("There is a Shortage of Foster/Adoptive Homes"),
                     h5("In Texas, the number of foster/adoptive homes has decreased over the years. "),
                     plotOutput("plot9"),
                     h2("Children Removed from their Homes"),
                     h5("A removal occurs when CPS determines that a child cannot safely remain in their own home and that DFPS needs to seek legal custody to ensure child safety. Removals can occur during an Investigation or Family Preservation."),
                     plotOutput("plot3"),
                     h5("The goal of this project is to apply a geo-spatial approach to examining the Homes and Removals data. "),
                     h3("")
                     
                     ),
            
            tabPanel("Geospatial Approach", 
                     h3("Number of Homes vs. Number of Children Removed from their Homes"),
                     fluidRow(
                         splitLayout(cellWidths = c("70%", "30%"), plotOutput("plot8"), plotOutput("plot8a"))
                     ),
                     h5("The aim of the geo-spatial map is to guide foster family recruitement and investments, i.e. identify where there is need."),
                     h3(""),
                     h5("Use the pull-down menu to see Your County."),
                     h3("Foster/Adoptive Homes"),
                     textOutput("selected_var2"),
                     h3(""),
                     fluidRow(
                         splitLayout(cellWidths = c("45%", "55%"), plotOutput("plot6"), plotOutput("plot7"))
                     ),  
                     h3("Children Waiting to be Adopted"),
                     fluidRow(
                         splitLayout(cellWidths = c("45%", "55%"), plotOutput("plot4"), plotOutput("plot5"))
                     )),
            
            tabPanel("Resources", 
                     h5("The Texas Department of Family and Protective Services (DFPS) works with communities to promote safe and healthy families and protect children and vulnerable adults from abuse, neglect, and exploitation."),
                     HTML("</a></li><li><a href='https://www.dfps.state.tx.us/' target='_blank'>Texas DFPS"),
                     HTML("</a></li><li><a href='https://data.texas.gov/browse?Dataset-Category_Agency=Texas+Department+of+Family+and+Protective+Services' target='_blank'>data.texas.gov"), 
                     HTML("</a></li><li><a href='https://www.dfps.state.tx.us/About_DFPS/Data_Book/' target='_blank'>TFPS Data Book"))
            ),
        # -----------------------------------
                
            
        ) # mainPanel parenthesis
        
    
    ) # sidebarLayout parenthesis
))
