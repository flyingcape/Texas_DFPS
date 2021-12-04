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
            tabPanel("Children in DFPS Custody", 
                     h2("Texas Children in DFPS Custody"),
                     h3("Number of Children by County"),
                     plotOutput("plot3"),
                     h5("Children in DFPS custody are those for whom a court has appointed DFPS legal responsibility through temporary or permanent managing conservatorship or other court ordered legal basis. The following data include any child in DFPS custody at some point during the year."),
                     h5("In 2020, there were a total of 47,913 Texas Children in DFPS custody."),
                     h3(""),
                     h5("Use the pull-down menu to see Your County."),
                     h3("Distribution by Age"),
                     textOutput("selected_var"),
                     h3(""),
                     fluidRow(
                         splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot1"), plotOutput("plot2"))
                     )
                     
                     ),
            
            tabPanel("Foster/Adoptive Homes", 
                     h3("Number of Homes vs. Number of Children Waiting to be Adopted"),
                     fluidRow(
                         splitLayout(cellWidths = c("70%", "30%"), plotOutput("plot8"), plotOutput("plot8a"))
                     ),
                     h5("The aim of the geo-spatial map is to guide foster care capacity investments and strategies, i.e. identify where there is need."),
                     h5("Take-away: In 2020, Bexar County stands out for a high adoption demand and a low foster care capacity. This county is a good candidate to increase target foster care recruitment and implement new strategies."),
                     h3(""),
                     h5("Use the pull-down menu to see how Your County compares? "),
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
