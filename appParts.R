#   ____________________________________________________________________________
#   Neighborhood Browser                                                    ####

neighborhoodDescription <- function() {
  tagList(
    div(class = "container",
        h1("Neighborhood Browser", class = "title fit-h1"),
        p("You are new to New York City or real estate investment? Use Intelligentsia's neighborhood browser to identify pockets of opportunity in the city."),
        p("Use the map to browse New York City's gentrifying census tracts. Click on any one of them to get more detailed information. Use the slider to show only the top k tracts."),
        fluidRow(
          column(7,
          
                 leafletOutput("map", height = 600)
          ),
          hidden(column(5, class = "hood-info", id = "reactiveOutput1",
                        h1(textOutput("hood"), class = "heading"),
                        htmlOutput("hoodInfo")
          ))
        ),
        hidden(
          div(class = "kpi-group",
              fluidRow(style = "margin-top: 10px;",
                       id = "reactiveOutput2a",
                       column(3,
                              div(plotlyOutput("donut", height = "100%"), align = "center"),
                              h3("Humidity", class = "kpi-name")
                       ),
                       column(3,
                              h2(textOutput("kpi1"), class = "kpi"),
                              h3("Mean Temperature", class = "kpi-name")
                       ),
                       column(3,
                              h2(textOutput("kpi2"), class = "kpi"),
                              HTML("<h3 class='kpi-name'>Water Supply<sup>*</sup></h3>")
                       ),
                       column(3,
                              h2(textOutput("kpi3"), class = "kpi"),
                              HTML("<h3 class='kpi-name'>Forested Area<sup>**</sup></h3>")
                       )
              ),
              fluidRow(id = "reactiveOutput2b",
                       column(3,
                              h2(textOutput("kpi4"), class = "kpi"),
                              HTML("<h3 class='kpi-name'>Non-Forested Area<sup>**</sup></h3>")
                       ),
                       column(3,
                              h2(textOutput("kpi5"), class = "kpi"),
                              HTML("<h3 class='kpi-name'>Permanent Reserved Area <sup>**</sup></h3>")
                       ),
                       column(3,
                              h2(textOutput("kpi6"), class = "kpi"),
                              HTML("<h3 class='kpi-name'>Area for Harvesting<sup>**</sup></h3>")
                       ),
                       column(3,
                              h2(textOutput("kpi7"), class = "kpi"),
                              HTML("<h3 class='kpi-name'>Population Estimates<sup>***</sup></h3>")
                       )
              ),
              fluidRow(id = "reactiveOutput2c",
                       column(3,
                              h2(textOutput("kpi8"), class = "kpi"),
                              HTML("<h3 class='kpi-name'>Clinical Waste Handled<sup>****</sup></h3>")
                       ),
                       column(3,
                              h2(textOutput("kpi9"), class = "kpi"),
                              HTML("<h3 class='kpi-name'>Waste Managed<sup>****</sup></h3>")
                       ),
                       column(3,
                              h2(textOutput("kpi10"), class = "kpi"),
                              h3("Rainfall Avg (Mm)", class = "kpi-name")
                       ),
                       column(3,
                              h2(textOutput("kpi11"), class = "kpi"),
                              h3("Population Density", class = "kpi-name")
                       )
              ),
              fluidRow(column(12,tags$small("* Million Litres, ** Hectares, *** Thousand, **** Tonnes ")))
          ),
          hr(),
          fluidRow(id = "reactiveOutput3",
                   column(12,
                          h2("Development of Water Supplies"),
                          div(plotlyOutput("zillow1", width = "100%"), align = "center", 
                              class = "chart", width = "100%")
                   )
          ),
          hr(),
          fluidRow(id = "reactiveOutput4",
                   column(12,
                          h2("Forest Area vs Clinical Waste Over Time"),
                          div(plotlyOutput("yelp", height = "420px"), align = "center",
                              class = "chart")
                   )
          ),
          hr(),
          fluidRow(id = "reactiveOutput4a1",
                   column(12,h2("Agriculture"))),
          fluidRow(id = "reactiveOutput4a",
                   column(6,
                          h3("Area Opened for Harvesting"),
                          div(plotlyOutput("google"), align = "center",
                              class = "chart")
                   ),
                   column(6,
                          h3("Rainfall (MM)"),
                          div(plotlyOutput("wiki"), align = "center",
                              class = "chart")
                   )
          ),
          hr(),
          fluidRow(id = "reactiveOutput5",
                   column(12,
                          h2("Reserved Area"),
                          div(plotlyOutput("taxi"), align = "center", 
                              class = "chart")
                   )
          ),
          hr()
        )
    )
  )
}


#   ____________________________________________________________________________
#   Location Comparison                                                     ####

legend <- "<div class='legend-custom'>
               <div class='legend-group'>
                   <div class='legend-element-label' color='location'></div>
                   <div class='legend-element-name'>Property</div>
               </div>
               <div class='legend-group'>
                   <div class='legend-element-label' color='yelp'></div>
                   <div class='legend-element-name'>Yelp</div>
               </div>
               <div class='legend-group'>
                   <div class='legend-element-label' color='schools'></div>
                   <div class='legend-element-name'>Schools</div>
               </div>
           </div>"

propertyComparison <- function() {
  
  sampleLocations <- c("Selangor",
                       "Sarawak",
                       "Pahang",
                       "Johor")
  
  sampleLocations <- sample(sampleLocations,2)
  
  tagList(
    div(class = "container",
        h1("Location Comparison", class = "title fit-h1"),
        #tags$script(src = "plugins/fittext.js"),
        p("You have already identified two locations of interest, but cannt decide which one to invest in? Let us help make your final decision."),
        p("Enter the addresses of two properties below and click the button to compare the locations in terms of gentrification potential."),
        p("Click the button to compare two sample locations or enter search addresses of your own!"),
        p("Using the buttons above the map, you can also show the location of nearby schools, subway stations and places on Yelp."),
        fluidRow(
          column(4,
                 div(class = "addrSearch",
                     textInput("searchAddr1", 
                               value = sampleLocations[1],
                               placeholder = "Enter address...",
                               label = NA),
                     class = "search")
          ),
          column(4, class="text-center", 
                 disabled(actionButton("compare", width = "75%",
                                       class = "btn-primary", style = "margin: 20px 0 20px 0;",
                                       HTML("&laquo; Compare locations &raquo;")))
          ),
          column(4,
                 div(class = "addrSearch",
                     textInput("searchAddr2", 
                               value = sampleLocations[2],
                               placeholder = "Enter address...",
                               label = NA),
                     class = "search"
                 )
          )
        ),
        hidden(
          div(id = "mapControls",
              fluidRow(style = "margin-bottom: 15px; text-align: center;",
                       column(12,
                              bsButton("showLayerSchools", "Schools", style = "info", size = "small", type = "toggle"),
                              bsButton("showLayerSubway", "Subway", style = "info", size = "small", type = "toggle"),
                              bsButton("showLayerYelp", "Yelp", style = "info", size = "small", type = "toggle")
                       )
              )
          )
        ),
        fluidRow(
          column(6,
                 hidden(div(id = "reactiveOutput7a",
                            leafletOutput("mapLocation1"),
                            HTML(legend),
                            h2(textOutput("hoodName1")),
                            p(textOutput("iScore1", inline = TRUE), style = "height: 60px;"),
                            div(id = "hood-details1",
                                h4("Monthly development of real estate prices"),
                                plotlyOutput("zillowLocation1", height = "150px", width = "100%")
                            )
                 ))
          ),
          column(6,
                 hidden(div(id = "reactiveOutput7b",
                            leafletOutput("mapLocation2"),
                            HTML(legend),
                            h2(textOutput("hoodName2")),
                            p(textOutput("iScore2", inline = TRUE), style = "height: 60px;"),
                            div(id = "hood-details2",
                                h4("Monthly development of real estate prices"),
                                plotlyOutput("zillowLocation2", height = "150px", width = "100%")
                            )
                 ))
          )
        ),
        fluidRow(
          column(12,
                 
                 hidden(div(id = "reactiveOutput8",
                            hr(),
                            h3("Comparison of key demographics"),
                            plotlyOutput("CTcomparisonChart", height = "750px"))
                 ))
        ),
        fluidRow(
          column(12,
                 hidden(div(id = "reactiveOutput9", 
                            style = "margin-top: 30px; font-size: 1.5em;",
                            hr(),
                            h3("Values for key demographics and real estate metrics"),
                            DT::dataTableOutput("CTcomparisonTable"))
                 ))
        )
    )
  )
}