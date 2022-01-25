#   ____________________________________________________________________________
#   Neighborhood Browser                                                    ####

neighborhoodDescription <- function() {
  tagList(
    div(class = "container",
        h1("State Explorer", class = "title fit-h1"),
        p("Explore the states by clicking on the map.Selected Enviromental factors are first shown for the year ended 2018. Historical data and more is shown in plots down below"),
        
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


propertyComparison <- function() {
  
  sampleLocations <- c("Selangor",
                       "Sarawak",
                       "Pahang",
                       "Johor")
  
  sampleLocations <- sample(sampleLocations,2)
  
  tagList(
    div(class = "container",
        h1("State Comparer", class = "title fit-h1"),
        #tags$script(src = "plugins/fittext.js"),
        p("Compare States Enviromental data by entering the states below."),
        p("Enter the name of two states to compare below."),
        p("Click the button to compare two sample locations or enter search addresses of your own!"),
        
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
        
        fluidRow(
          column(6,
                 hidden(div(id = "reactiveOutput7a",
                          
                            h2(textOutput("hoodName1")),
                            p(textOutput("iScore1", inline = TRUE), style = "height: 60px;"),
                            div(id = "hood-details1",
                                h4("Water Supply in Million Litres per day"),
                                plotlyOutput("zillowLocation1", height = "150px", width = "100%")
                            )
                 ))
          ),
          column(6,
                 hidden(div(id = "reactiveOutput7b",
            
                            
                            h2(textOutput("hoodName2")),
                            p(textOutput("iScore2", inline = TRUE), style = "height: 60px;"),
                            div(id = "hood-details2",
                                h4("Water Supply in Million Litres per day"),
                                plotlyOutput("zillowLocation2", height = "150px", width = "100%")
                            )
                 ))
          )
        ),
        fluidRow(
          column(6,
                 
                 hidden(div(id = "reactiveOutput8",
                            hr(),
                            h4("Forested Area Hectares"),
                            plotlyOutput("zillowLocation1b", height = "150px", width = "100%")
                 ))
          ),
          column(6,
                 
                 hidden(div(id = "reactiveOutput9",
                            hr(),
                            h4("Forested Area Hectares"),
                            plotlyOutput("zillowLocation2b", height = "150px", width = "100%")
                 ))
          )
        ),
        fluidRow(
          column(6,
                 
                 hidden(div(id = "reactiveOutput10",
                            hr(),
                            h4("Population Estimate"),
                            plotlyOutput("zillowLocation1c", height = "150px", width = "100%")
                 ))
          ),
          column(6,
                 
                 hidden(div(id = "reactiveOutput11",
                            hr(),
                            h4("Population Estimate"),
                            plotlyOutput("zillowLocation2c", height = "150px", width = "100%")
                 ))
          )
        ),
        fluidRow(
          column(6,
                 
                 hidden(div(id = "reactiveOutput12",
                            hr(),
                            h4("Clinical Waste"),
                            plotlyOutput("zillowLocation1d", height = "150px", width = "100%")
                 ))
          ),
          column(6,
                 
                 hidden(div(id = "reactiveOutput13",
                            hr(),
                            h4("Clinical Waste"),
                            plotlyOutput("zillowLocation2d", height = "150px", width = "100%")
                 ))
          )
        ),
        fluidRow(
          column(6,
                 
                 hidden(div(id = "reactiveOutput14",
                            hr(),
                            h4("Population Density"),
                            plotlyOutput("zillowLocation1e", height = "150px", width = "100%")
                 ))
          ),
          column(6,
                 
                 hidden(div(id = "reactiveOutput15",
                            hr(),
                            h4("Population Density"),
                            plotlyOutput("zillowLocation2e", height = "150px", width = "100%")
                 ))
          )
        ),
        fluidRow(
          column(6,
                 
                 hidden(div(id = "reactiveOutput16",
                            hr(),
                            h4("Scheduled Waste"),
                            plotlyOutput("zillowLocation1f", height = "150px", width = "100%")
                 ))
          ),
          column(6,
                 
                 hidden(div(id = "reactiveOutput17",
                            hr(),
                            h4("Scheduled Waste"),
                            plotlyOutput("zillowLocation2f", height = "150px", width = "100%")
                 ))
          )
        ),
        fluidRow(
          column(6,
                 
                 hidden(div(id = "reactiveOutput18",
                            hr(),
                            h4("Area Opened for Harvesting"),
                            plotlyOutput("zillowLocation1g", height = "150px", width = "100%")
                 ))
          ),
          column(6,
                 
                 hidden(div(id = "reactiveOutput19",
                            hr(),
                            h4("Area Opened for Harvesting"),
                            plotlyOutput("zillowLocation2g", height = "150px", width = "100%")
                 ))
          )
        )
    )
  )
}

dashboardDescription <- function (){
  tagList(
    div(class = "container",
        h1("Country Overview", class = "title fit-h1"),
        p("See the top figures and time series environmental data of Malaysia."),
       
        h1(paste0("Malaysia Enviromental Overview")) ,
        fluidRow(
          valueBoxOutput("ExGBox")%>% withSpinner(type=4),
          valueBoxOutput("ImTotBox"),
          valueBoxOutput("BlTotBox")
        
        ),
        fluidRow(
          valueBoxOutput("ExSBox") ,
          valueBoxOutput("ImSBox") ,
          valueBoxOutput("BlSBox")
        ) ,
        ## 1.2 Time serise plot ----------------------------------------
        h2(paste0("Country at Glance")),
        fluidRow( column( width = 6,h4("Enviromental Damage Cause", align = 'center'), highchartOutput('IEGSLineHc') ),
                  column( width = 6,h4("Dependency on Enviroment", align = 'center'), highchartOutput('GSTotalBalanceLineHc') )
        ),
        
        
        
        fluidRow( h3("Key Criteria", align = 'center'),
                  column( width = 6, h4("Emissions"), highchartOutput('KeyExLine')  ),
                  column( width = 6, h4("Renewable Energy"), highchartOutput('KeyExLinePercent')  ) ),
        fluidRow( h3("Depletion", align = 'center'),
                  column( width = 6, h4("Key Depletions"), highchartOutput('KeyImLine') ),
                  column( width = 6, h4("Key Cause(s)"), highchartOutput('KeyImLinePercent')  ) ),
        ## total Exports
        fluidRow( h3("Dependecies" ,align = 'center'),
                  h4(tags$b("Total dependencies")),
                  column( width = 6, h4("Total Rent"), highchartOutput("ExMarketLine") ),
                  column( width = 6, h4("Alternative Rent"), highchartOutput("ExMarketLinePercent") )
                  #column( width = 6, h4("key IMPORTS markets over $1b"), highchartOutput("ImMarketLine") )
        ),
        
        
        fluidRow( h4(tags$b("Enablers of sustainable enviroments")),
                  column( width = 6, h4("Foodstock"), highchartOutput("ExSMarketLine") ),
                  column( width = 6, h4("Access to clean energies"), highchartOutput("ExSMarketLinePercent") )
                  #column( width = 6, h4("key IMPORTS markets over $1b"), highchartOutput("ImMarketLine") )
        )
    )
    )
  
  
}
VB_style <- function(msg = 'Hello', style="font-size: 100%;"){
  tags$p( msg , style = style, color="green" )
}


