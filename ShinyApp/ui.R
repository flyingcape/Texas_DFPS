# Define UI for application 
bootstrapPage(theme = shinytheme("flatly"),
  navbarPage(HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Texas Foster Care</a>'), id="nav",
             windowTitle = "Texas Foster Care",
             
             tabPanel("About this Project",
                sidebarLayout(
                  sidebarPanel(
                    h3('Key Project Objective'),
                    span((h5("The overall goal is to identify TX counties that may require more resources or spotlight.")), style="color:#000000"),
                    h3(''),
                    h4('GEOSPATIAL APPROACH'),
                    span((h5("Overlay two CPS datasets onto a Texas county map. ")), style="color:#000000"),
                    img(src='tx_blank_map.png', align = "right", height="200px", width="100%"),
                    span((h6("\nIn this R Shiny App, the following datasets are overlaid")), style="color:#000000"),
                    span((h6("(1) foster/adoptive homes")), style="color:#000000"),
                    span((h6("(2) child removals")), style="color:#000000")

                    ), # end of sidebarPanel
                        
                  mainPanel(
                    h3("Child Protective Services (CPS)"),
                    h5("CPS is part of the Texas Department of Family and Protective Services (DFPS). It’s function is to protect Texas children from abuse, neglect, and exploitation. CPS is responsible for placing children in foster care."),
                    h5("A child is removed from their home when CPS determines that the child cannot safely remain in their home. The number of children removed is ~18K per year."),
                    plotOutput("Removals_Plot1", height="325px", width="100%"),
                    h3("OUR CHALLENGE"),
                    h5("The Texas foster care system is experiencing a state-wide shortage of available foster and adoptive (FAD) homes."),
                    h5("The total number of homes have steadily decreased since 2011."),
                    plotOutput("Homes_plot1", height="325px", width="100%"),
                    h6("Author: Mandy McClintock | R Data Analysis Project  |  NYC Data Science Academy")
                    
                        )
                      )
             ),

            tabPanel("Geospatial Approach",
              sidebarLayout(
              sidebarPanel(
                selectizeInput(inputId = "TheYear",
                               label = "Choose Year",
                               choices = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)), # end of selectizeInput
                selectizeInput(inputId = "YourCounty",
                               label = "Choose Your County",
                               choices = sort(TX_counties$CNTY_NM)) 
              ), # end of sidebarPanel

              mainPanel(
                  tabsetPanel(
                    tabPanel("Foster/Adoptive Homes", 
                        h3("FAD Homes per 100K Residents"),
                        plotOutput("map1")
                        
                        
                    ), # end of tabPanel
                    tabPanel("Child Removals", 
                        h3("Child Removals per 1K Children"),
                        plotOutput("map2")
                        
                    ), # end of tabPanel 
                    tabPanel("Homes vs. Removals",
                             h3("A Bivariate Approach"),
                             h5("The foster/adoptive homes per 100K residents and child removals per 1K children data were overlaid onto a TX county map."),
                             fluidRow(
                               splitLayout(cellWidths = c("70%", "30%"), plotOutput("map3"), 
                                           img(src='bivariate_leg.png', align = "center", height="160px", width="100%"))
                             ),
                             h3("The highest priority counties include: "),
                             textOutput("Bivariate_text3")
                             
                    ),
                    tabPanel("See Your County",
                             tags$style("#County_text0 {font-size:25px;
                                color: black;
                                display:block; }"),
                             textOutput("County_text0"),
                             fluidRow(column(12, align="left",
                                             textOutput("Homes_text3"),
                                             textOutput("Homes_text4"))
                             ),
                             fluidRow(
                               splitLayout(cellWidths = c("40%", "60%"), plotOutput("map_tiny1"), 
                                           plotOutput("map1_again"))),
                             fluidRow(column(12, align="left",
                                             textOutput("Removals_text3"),
                                             textOutput("Removals_text4"))
                             ),
                             fluidRow(
                               splitLayout(cellWidths = c("40%", "60%"), plotOutput("map_tiny2"), 
                                           plotOutput("map2_again"))),
                             h2("For the Bivariate Analysis: "),
                             h4("Homes per 100K Residents vs. Removals per 1K Children"),
                             fluidRow(
                               splitLayout(cellWidths = c("65%","35%"),  
                                           plotOutput("map3_again"),
                                           img(src='bivariate_leg.png', align = "left", height="180px", width="100%"))
                               ),
                             h5("Your county was classified as the following"),
                             plotOutput("map_tiny3", height="180px", width="50%")
                             
                    )
                  )
           ) # end of mainPanel
           )
),
tabPanel("Future Tinkering",
         sidebarLayout(
           sidebarPanel(
           ), # end of sidebarPanel
           
           mainPanel(
             tabsetPanel(
               tabPanel("Tab1", 

               ), # end of tabPanel
               tabPanel("Tab2", 

               ), # end of tabPanel 
               tabPanel("Tab3", 

               )
             )
           ) # end of mainPanel
         )
)
             
  )          
)