#   ____________________________________________________________________________
#   UI                                                                      ####

library(shiny)
library(leaflet)
library(plotly)
library(shinyjs)
library(shinyBS)
library(shinydashboard)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(lubridate)
library(stringr)
library(withr)
library(treemap)
library(DT)
library(shinyBS)
library(shinyjs)
library(WDI)
library(geosphere)
library(magrittr)
library(shinycssloaders)
options(spinner.color="#006272")
library(timevis)

source("appParts.R")
source("readData.R")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Colors                                                                  ####

#C10250 purple
#03BCC0 green
#D2D945 yellow/green
#FCB040 orange
#FF5850 red
#436983 hipster blue

shinyUI(navbarPage(title = "Enviro",
                   theme = "style/style.css",
                   footer = includeHTML("footer.html"),
                   fluid = TRUE, 
                   collapsible = TRUE,
                   # ----------------------------------
                   # tab panel 1 - Home
                   tabPanel("Home",
                            includeHTML("home.html"),
                            tags$script(src = "plugins/scripts.js"),
                            tags$head(
                              tags$link(rel = "stylesheet", 
                                        type = "text/css", 
                                        href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                              tags$link(rel = "icon", 
                                        type = "image/png", 
                                        href = "images/logo_icon.png")
                            )
                   ), 
                   tabPanel('Country Overview',
                            dashboardPage(
                              dashboardHeader(disable = TRUE),
                              dashboardSidebar(disable = TRUE),
                              dashboardBody(
                                dashboardDescription(),
                                tags$head(
                                  # ## JS codes
                                  # tags$script(src = "fixedElement.js" ),
                                  # tags$style(HTML(".scroller_anchor{height:0px; margin:0; padding:0;}; 
                                  #                  .scroller{background: white; 
                                  #                   border: 1px solid #CCC; 
                                  #                   margin:0 0 10px; 
                                  #                   z-index:100; 
                                  #                   height:50px; 
                                  #                   font-size:18px; 
                                  #                   font-weight:bold; 
                                  #                   text-align:center; 
                                  #                  width:500px;}")),
                                  
                                  #tags$script(src = "world.js" ),
                                  
                                  ### Styles 
                                  
                                  tags$style(HTML(".fa { font-size: 35px; }")),
                                  tags$style(HTML(".glyphicon { font-size: 33px; }")),  ## use glyphicon package
                                  tags$style(HTML(".fa-dashboard { font-size: 20px; }")),
                                  tags$style(HTML(".fa-globe { font-size: 20px; }")),
                                  tags$style(HTML(".fa-barcode { font-size: 20px; }")),
                                  tags$style(HTML(".tab-content { padding-left: 20px; padding-right: 30px; }")) ,
                                  tags$style(HTML(".fa-wrench { font-size: 15px; }")),
                                  tags$style(HTML(".fa-refresh { font-size: 15px; }")),
                                  tags$style(HTML(".fa-search { font-size: 15px; }")),
                                  tags$style(HTML(".fa-comment { font-size: 20px; }")),
                                  tags$style(HTML(".fa-share-alt { font-size: 20px; }")),
                                  tags$style(HTML(".fa-envelope { font-size: 20px; }")),
                                  tags$style(HTML(".fa-question-circle { font-size: 20px; }")),
                                  tags$style(HTML(".fa-chevron-circle-down { font-size: 15px; }")),
                                  tags$style(HTML(".fa-bell { font-size: 17px; }")),
                                  tags$style(HTML(".fa-check { font-size: 14px; }")),
                                  tags$style(HTML(".fa-times { font-size: 14px; }")),
                                  
                                  #tags$style(HTML(".fa-twitter { font-size: 10px; color:red;}")),
                                  #tags$style(HTML(".fa-facebook { font-size: 10px; color:red;}")),
                                  #tags$style(HTML(".fa-google-plus { font-size: 10px; color:red;}")),
                                  #tags$style(HTML(".fa-pinterest-p { font-size: 10px; color:red;}")),
                                  #tags$style(HTML(".fa-linkedin { font-size: 10px; color:red;}")),
                                  #tags$style(HTML(".fa-tumblr { font-size: 10px; color:red;}")),
                                  
                                  ## modify the dashboard's skin color
                                  tags$style(HTML('
                       /* logo */
                       .skin-blue .main-header .logo {
                       background-color: #006272;
                       }
                       /* logo when hovered */
                       .skin-blue .main-header .logo:hover {
                       background-color: #006272;
                       }
                       /* navbar (rest of the header) */
                       .skin-blue .main-header .navbar {
                       background-color: #006272;
                       }
                       /* active selected tab in the sidebarmenu */
                       .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                       background-color: #006272;
                                 }
                       ')
                                  ),
                                  
                                  ## modify icon size in the sub side bar menu
                                  tags$style(HTML('
                       /* change size of icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview-menu>li>a>.fa {
                      font-size: 15px;
                      }
                      .sidebar .sidebar-menu .treeview-menu>li>a>.glyphicon {
                      font-size: 13px;
                      }
                      /* Hide icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview>a>.fa-angle-left {
                      display: none;
                      } 
                      '
                                  )) ,
                                  
                                  tags$style( HTML("hr {border-top: 1px solid #000000;}") ),
                                  
                                  ## to not show error message in shiny
                                  tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
                                  tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
                                  
                                  ## heand dropdown menu size
                                  #tags$style(HTML('.navbar-custom-menu>.navbar-nav>li>.dropdown-menu { width:100px;}'))
                                  tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.dropdown-menu { width:10px; font-size:10px; padding:1px; margin:1px;}')),
                                  tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > h4 {width:0px; font-size:0px; padding:0px; margin:0px;}')),
                                  tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > p {width:0px; font-size:0px; padding:0px; margin:0px;}'))
                                )))
                   ),
                   # ----------------------------------
                   # tab panel 3 - State Explorer
                   tabPanel("State Explorer",
                            neighborhoodDescription(),
                            includeHTML("scrollToTop.html")
                   ),
                   # tab panel 4 - State Comparer
                   tabPanel("State Comparer",
                            propertyComparison(),
                            tags$script(src="plugins/fixedElement.js"),
                            tags$head(
                              tags$link(rel="stylesheet",
                                        type="text/css",
                                        href="plugins/TableStyle.css")
                            )
                   ),
                   
                   
                   
                   # tab panel 6 - Documentation
                   tabPanel("Documentation",
                            includeHTML("document.html")
                            
                            
                   ),
                   
                   # tab panel 5 - About
                   tabPanel("About",
                            includeHTML("about.html"),
                            shinyjs::useShinyjs(),
                            tags$head(
                              tags$link(rel = "stylesheet", 
                                        type = "text/css", 
                                        href = "plugins/carousel.css"),
                              tags$script(src = "plugins/holder.js")
                            ),
                            tags$style(type="text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"
                            )
                   )
                   
))