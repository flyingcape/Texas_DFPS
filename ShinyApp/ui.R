# Define UI for application 
bootstrapPage(theme = shinytheme("flatly"),
  navbarPage(HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Texas Foster Care</a>'), id="nav",
             windowTitle = "Texas Foster Care",
             
             tabPanel("About this Project",
                sidebarLayout(
                  sidebarPanel(
                    h3('Key Project Objective'),
                    span((h5("This project sought to identify areas of the state that are in high-need of FAD homes and/or may require more spotlight or resources.")), style="color:#000000"),
                    h3(''),
                    h4('GEOSPATIAL APPROACH'),
                    span((h5("Overlay two CPS datasets onto a Texas county map. ")), style="color:#000000"),
                    img(src='tx_blank_map.png', align = "right", height="auto", width="100%"),
                    span((h6("\nIn this R Shiny App, the following datasets are overlaid")), style="color:#000000"),
                    span((h6("(1) foster/adoptive homes")), style="color:#000000"),
                    span((h6("(2) DFPS child removals")), style="color:#000000")

                    ), # end of sidebarPanel
                        
                  mainPanel(
                    h3("Child Protective Services (CPS)"),
                    h5("CPS is part of the Texas Department of Family and Protective Services (DFPS). It’s function is to protect Texas children from abuse, neglect, and exploitation. CPS is responsible for placing children in foster care."),
                    h5("A child is removed from their home if and when CPS determines that the child cannot safely remain in their home. Between 2011-2021, the number of children removed was on average ~2.4 per 1K children."),
                    plotOutput("Removals_Plot1", height="350px", width="680px"),
                    h3("OUR CHALLENGE"),
                    h5("The Texas foster care system is experiencing a shortage of available foster, adoptive, and foster/adoptive (FAD) homes."),
                    h5("Between 2011-2021, the quantity of statewide FAD homes per 100K residents decreased by 64%."),
                    plotOutput("Homes_plot1", height="350px", width="650px"),
                    uiOutput("tab")
                    
                        )
                      )
             ),

            tabPanel("Geospatial Approach",
              sidebarLayout(
              sidebarPanel(
                selectizeInput(inputId = "TheYear",
                               label = "Choose Year",
                               choices = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,2021)) # end of selectizeInput
              ), # end of sidebarPanel

              mainPanel(
                  tabsetPanel(
                    tabPanel("Homes vs. Removals",
                             h3("A Bivariate Approach"),
                             h5("The foster/adoptive homes per 100K residents and child removals per 1K children data were overlaid onto a TX county map."),
                             fluidRow(
                               splitLayout(cellWidths = c("70%", "30%"), plotOutput("map3"), 
                                           img(src='bivariate_leg2.png', align = "center", height="auto", width="100%"))
                             ),
                             textOutput("Bivariate_text3"), 
                             h3("Texas DFPS Regions"),
                             h5("To place the high-need counties in relation to familiar major metropolitan areas, here is the breakdown by Texas DFPS region:"),
                             plotOutput("HighNeed_barplot", height="350px", width="475px")
                             
                    ),
                    tabPanel("Foster/Adoptive Homes", 
                        h3("FAD Homes per 100K Residents"),
                        plotOutput("map1")
                        
                        
                    ), # end of tabPanel
                    tabPanel("Child Removals", 
                        h3("Child Removals per 1K Children"),
                        plotOutput("map2")
                        
                    ) # end of tabPanel 
                  )
           ) # end of mainPanel
           )
)
             
  )          
)