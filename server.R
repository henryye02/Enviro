
library(shiny)
library(rgdal)
library(leaflet)
library(sp)
library(plotly)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(ggmap)
library(xts)
library(shinyjs)
library(jsonlite)
library(urltools)
library(utils)
library(rvest)
library(stringr)
library(rgeos)
library(xml2)
library(selectr)

library(purrr)
library(RColorBrewer)
library(DT)
library(shinyBS)



format_metric <- function(x, type) {
  switch(type,
         currency = paste0("$", 
                           formatC(round(as.numeric(x), 0), 
                                   big.mark = ",", 
                                   digits = nchar(as.character(round(as.numeric(x), 0)))
                           )
         ),
         real = format(round(as.numeric(x), 1), 
                       nsmall = 1, big.mark = ","),
         int = formatC(as.numeric(x), big.mark = ",", 
                       digits = nchar(as.character(x))),
         year = round(as.numeric(x),0),
         pct = paste0(format(round(as.numeric(x) * 100, 1), 
                             nsmall = 1, big.mark = ","),"%"))
}

shinyServer(function(input, output) {
  
  
  ##  ............................................................................
  ##  Neighborhood browser                                                    ####
  
  ##  ............................................................................
  ##  Map chart                                                               ####
  
  GeoDFr <- reactive({
    GeoDF[GeoDF$rank <= input$topK,]
  })
  
  geoIDs <- reactive({
    GeoDF <- GeoDF[GeoDF$rank <= input$topK,]
    as.vector(GeoDF$NAME_1)
  })
  
  labels <- paste("<p>", GeoDF$NAME_1,"</p>",
                  "<p>","Population Density ",round(GeoDF$Pop_Den,digits = 3),"</p>",
                  sep = "")
  
  # ---------------------------
  # create color scheme for map
  pal <- colorFactor(palette='Set2', domain=state_data$St)
  
  output$map <- renderLeaflet({
    
    leaflet(GeoDFr()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(109.45547499999998,4.1093195,5) %>%
      addPolygons(data=GeoDF,
                  layerId = ~NAME_1,
                  weight = 1,
                  smoothFactor = 1,
                  fillOpacity=0.8,
                  fillColor = ~pal(state_data$St),
                  label=lapply(labels,HTML),
                  
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal",
                                 padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) 
  })
  # --------------------------------------------------------
  # observe click events in map and get neighborhood details
  
  geo <- observeEvent(input$map_shape_click, {
   
    shinyjs::show("reactiveOutput6")
    shinyjs::show("reactiveOutput5")
    shinyjs::show("reactiveOutput4a1")
    shinyjs::show("reactiveOutput4a")
    shinyjs::show("reactiveOutput4")
    shinyjs::show("reactiveOutput3")
    shinyjs::show("reactiveOutput2a")
    shinyjs::show("reactiveOutput2b")
    shinyjs::show("reactiveOutput2c")
    shinyjs::show("reactiveOutput1")
    shinyjs::removeClass(class = "shinyjs-hide", selector = "hr")
    shinyjs::removeClass(class = "shinyjs-hide", selector = ".kpi-group")
    click <- input$map_shape_click
    print(click$id)
    as.numeric(click$id)
    hood<- click$id
    output$hood<-renderText(hood)
    kpis=state_data %>%
      filter (St==hood)
    
    output$kpi1 <- renderText(kpis$Mean_Temp)
    output$kpi2 <- renderText(kpis$Wat_Sup)
    output$kpi3 <- renderText(kpis$For_Area)
    output$kpi4 <- renderText(kpis$Non_For)
    output$kpi5 <- renderText(kpis$Res_Ar)
    output$kpi6 <- renderText(kpis$AR_OP)
    output$kpi7 <- renderText(kpis$Pop_Es)
    output$kpi8 <- renderText(kpis$Cl_Ws)
    output$kpi9 <-renderText(kpis$Sch_Ws)
    output$kpi10 <-renderText(kpis$Rain)
    output$kpi11 <- renderText(kpis$Pop_Den)
    
    # ------
    # zillow
    
    output$zillow1 <- renderPlotly({
      
      p %>% 
        dplyr::filter(St == hood) %>% 
        plot_ly(x = ~Year) %>%
        add_lines(y = ~round(Wat_Sup,2), 
                  line = list(shape = "linear", color = "#4fbfa8"),
                  name = "Zestimate") %>% 
        layout(xaxis = list(title = "",
                            showgrid = FALSE,
                            ticks = "",
                            fixedrange = TRUE,
                            gridcolor = "#a0a0a0"),
               yaxis = list(title = "Water Supplied Million Litres Per Day",
                            fixedrange = TRUE, zeroline = TRUE, 
                            rangemode = "tozero",
                            ticks = "", gridcolor = "#a0a0a0"),
               legend = list(x = .5, y = -.1, xanchor = "center",
                             orientation = "h"),
               annotations = list(text = "Source: Department of Statistics", align = "left",
                                  x = 0, y = -.18, showarrow = FALSE,
                                  xref = "paper", yref = "paper"),
               margin = list(b = 50, pad = 15),
               font = list(family = "'Raleway', sans-serif"),
               paper_bgcolor = "rgba(1,1,1,0)",
               plot_bgcolor = "rgba(1,1,1,0)") %>% 
        config(displayModeBar = FALSE)
    })
    
    # ----
    # yelp
    
    output$yelp <- renderPlotly({
      p %>% dplyr::filter(St == hood) %>%
       
        plot_ly(x = ~ Year) %>%
        add_lines(y = ~For_Area, 
                  line = list(shape = "linear", color = "#4fbfa8"),
                  name = "Forested Area") %>% 
        add_lines(y = ~Cl_Ws, yaxis = "y2",
                  line = list(shape = "linear", color = "#416983"),
                  name = "Clinical Waste Handled") %>%
        layout(xaxis = list(title = "",
                            showgrid = FALSE,
                            ticks = "",
                            fixedrange = TRUE,
                            gridcolor = "#a0a0a0"),
               yaxis = list(title = "Forested Area (Hectares)",
                            fixedrange = TRUE, zeroline = TRUE, 
                            ticks = "", gridcolor = "#a0a0a0",
                            rangemode = "tozero"),
               yaxis2 = list(title = "Clinical Waste (Tonnes)", side = "right",
                             fixedrange = TRUE, zeroline = FALSE,
                             showgrid = TRUE, autorange = TRUE,
                             rangemode = "tozero", overlaying = "y"),
               legend = list(x = .5, y = -.08, xanchor = "center",
                             orientation = "h"),
               annotations = list(text = "Source: Department of Statistics", align = "left",
                                  x = 0, y = -.20, showarrow = FALSE,
                                  xref = "paper", yref = "paper"),
               font = list(family = "'Raleway', sans-serif"),
               margin = list(r = 40, b = 30, pad = 3),
               paper_bgcolor = "rgba(1,1,1,0)",
               plot_bgcolor = "rgba(1,1,1,0)") %>% 
        config(displayModeBar = FALSE)
    })
    # --------------------
    # google search trends
    
    output$google <- renderPlotly({
      
      p %>% 
        dplyr::filter(St==hood) %>% 
        plot_ly(x = ~Year) %>%
        add_bars(y = ~AR_OP, 
                 type = "bar", marker = list(color = "#436983"),
                 name = "Area Opened for harvesting") %>% 
        layout(xaxis = list(title = "",
                            showgrid = FALSE,
                            ticks = "",
                            fixedrange = TRUE,
                            gridcolor = "#a0a0a0"),
               yaxis = list(title = "Area Opened for Harvesting (Hectares)",
                            fixedrange = TRUE, zeroline = TRUE, 
                            rangemode = "tozero",
                            ticks = "", gridcolor = "#a0a0a0"),
               legend = list(x = .5, y = -.1, xanchor = "center",
                             orientation = "h"),
               annotations = list(text = "Source: Department of Statistics", align = "left",
                                  x = 0, y = -.15, showarrow = FALSE,
                                  xref = "paper", yref = "paper"),
               margin = list(b = 50, pad = 3),
               font = list(family = "'Raleway', sans-serif"),
               paper_bgcolor = "rgba(1,1,1,0)",
               plot_bgcolor = "rgba(1,1,1,0)") %>% 
        config(displayModeBar = FALSE)
    })
    
    # ---------------
    # wikipedia edits
    
    output$wiki <- renderPlotly({
      
      p %>% 
        dplyr::filter(St == hood) %>% 
        plot_ly(x = ~Year) %>%
        add_bars(y = ~round(Rain,0), 
                 type = "bar", marker = list(color = "#436983"),
                 name = "Rainfall") %>% 
        layout(xaxis = list(title = "",
                            showgrid = FALSE,
                            ticks = "",
                            fixedrange = TRUE,
                            gridcolor = "#a0a0a0"),
               yaxis = list(title = "Rainfall(MM)",
                            fixedrange = TRUE, zeroline = TRUE, 
                            rangemode = "tozero",
                            ticks = "", gridcolor = "#a0a0a0"),
               legend = list(x = .5, y = -.1, xanchor = "center",
                             orientation = "h"),
               annotations = list(text = "Source: Department of Statistics", align = "left",
                                  x = 0, y = -.15, showarrow = FALSE,
                                  xref = "paper", yref = "paper"),
               margin = list(b = 50, pad = 3),
               font = list(family = "'Raleway', sans-serif"),
               paper_bgcolor = "rgba(1,1,1,0)",
               plot_bgcolor = "rgba(1,1,1,0)") %>% 
        config(displayModeBar = FALSE)
    })
    
    # -----------------
    # taxitrips
    
    output$taxi <- renderPlotly({
     p %>% dplyr::filter(St==hood) %>%
        dplyr::arrange(Year) %>%
        plot_ly(x = ~Year) %>%
        add_lines(y = ~Res_Ar, 
                  line = list(shape = "linear", color = "#4fbfa8"),
                  name = "Reserved Area") %>%
        layout(xaxis = list(title = "",
                            showgrid = FALSE,
                            ticks = "",
                            fixedrange = TRUE,
                            gridcolor = "#a0a0a0"),
               yaxis = list(title = "Area Reserved (Hectares)",
                            fixedrange = TRUE, zeroline = TRUE, 
                            ticks = "", gridcolor = "#a0a0a0",
                            rangemode = "tozero"),
               legend = list(x = .5, y = -.1, xanchor = "center",
                             orientation = "h"),
               annotations = list(text = "Source: Department of Statistics", align = "left",
                                  x = 0, y = -.15, showarrow = FALSE,
                                  xref = "paper", yref = "paper"),
               margin = list(b = 50, pad = 3),
               font = list(family = "'Raleway', sans-serif"),
               paper_bgcolor = "rgba(1,1,1,0)",
               plot_bgcolor = "rgba(1,1,1,0)") %>% 
        config(displayModeBar = FALSE)
    })
    
    # -----------------
    # donut chart
    
    output$donut <- renderPlotly({
      
      iScore <- round(kpis$Mean_Temp)
      
      color <- brewer.pal(9,"YlOrRd")[max(1,floor((iScore-1) / 10))]
      
      lw <- 0
      
      plot_ly(width = 80, height = 80,
              marker = list(colors = c(color, "rgba(0,0,0,0)"),
                            line = list(width = lw, color = "#444"))) %>%
        add_pie(values = c(iScore, 100 - iScore),
                hole = .7, sort = FALSE, direction = "clockwise",
                textposition = 'none') %>%
        layout(font = list(family = "'Raleway', sans-serif"),
               autosize = TRUE,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               paper_bgcolor = "rgba(1,1,1,0)",
               plot_bgcolor = "rgba(1,1,1,0)",
               hovermode = FALSE,
               showlegend = FALSE,
               margin = list(l = 0, r = 0, t = 5, b = 0),
               annotations = list(text = paste0("<b>",iScore,"</b>"),
                                  align = "center",
                                  font = list(size = 28),
                                  showarrow = FALSE)) %>%
        config(displayModeBar = F)
    })
   
  })
  ##  ............................................................................
  ##  Location comparison                                                     ####
  
  ##  ............................................................................
  ##  Reactive values                                                         ####
  
  observeEvent(input$compare, {
    shinyjs::show("reactiveOutput7a")
    shinyjs::show("reactiveOutput7b")
    shinyjs::show("reactiveOutput8")
    shinyjs::show("reactiveOutput9")
    shinyjs::show("reactiveOutput10")
    shinyjs::show("reactiveOutput11")
    shinyjs::show("reactiveOutput12")
    shinyjs::show("reactiveOutput13")
    shinyjs::show("reactiveOutput14")
    shinyjs::show("reactiveOutput15")
    shinyjs::show("reactiveOutput16")
    shinyjs::show("reactiveOutput17")
    shinyjs::show("reactiveOutput18")
    shinyjs::show("reactiveOutput19")
    
  })
  
  observeEvent(input$searchAddr1, {
    shinyjs::hide("reactiveOutput7a")
  })
  observeEvent(input$searchAddr1, {
    shinyjs::hide("reactiveOutput8a")
  })
  observeEvent(input$searchAddr2, {
    shinyjs::hide("reactiveOutput7b")
  })
  observeEvent(input$searchAddr2, {
    shinyjs::hide("reactiveOutput8b")
  })
  
  observe({
    if ((is.null(input$searchAddr1) || input$searchAddr1 == "") ||
        (is.null(input$searchAddr2) || input$searchAddr2 == "")) {
      shinyjs::disable("compare")
    } else {
      shinyjs::enable("compare")
    }
  })
  zillowSmall <- function(x) {
    p %>% 
      dplyr::filter(St== x) %>% 
      plot_ly(x = ~Year) %>%
      add_lines(y =~Wat_Sup, 
                line = list(shape = "linear", color = "#4fbfa8"),
                name = "Water Supply") %>% 
      layout(xaxis = list(title = "",
                          showgrid = FALSE,
                          ticks = "",
                          fixedrange = TRUE),
             yaxis = list(title = "", fixedrange = TRUE, 
                          showticklabels = FALSE, showgrid = FALSE,
                          ticks = "", zeroline = TRUE, rangemode = "tozero"),
             annotations = list(text = "Source: Department of Statistics", align = "left",
                                x = 0, y = -.5, showarrow = FALSE,
                                xref = "paper", yref = "paper"),
             font = list(family = "'Raleway', sans-serif"),
             margin = list(l = 40, r = 40),
             paper_bgcolor = "rgba(1,1,1,0)",
             plot_bgcolor = "rgba(1,1,1,0)") %>% 
      config(displayModeBar = FALSE)
  }
  zillowSmallb <- function(x) {
    p %>% 
      dplyr::filter(St== x) %>% 
      plot_ly(x = ~Year) %>%
      add_lines(y =~For_Area, 
                line = list(shape = "linear", color = "#4fbfa8"),
                name = "Forest Area") %>% 
      layout(xaxis = list(title = "",
                          showgrid = FALSE,
                          ticks = "",
                          fixedrange = TRUE),
             yaxis = list(title = "", fixedrange = TRUE, 
                          showticklabels = FALSE, showgrid = FALSE,
                          ticks = "", zeroline = TRUE, rangemode = "tozero"),
             annotations = list(text = "Source: Department of Statistics", align = "left",
                                x = 0, y = -.5, showarrow = FALSE,
                                xref = "paper", yref = "paper"),
             font = list(family = "'Raleway', sans-serif"),
             margin = list(l = 40, r = 40),
             paper_bgcolor = "rgba(1,1,1,0)",
             plot_bgcolor = "rgba(1,1,1,0)") %>% 
      config(displayModeBar = FALSE)
  }
  zillowSmallc <- function(x) {
    p %>% 
      dplyr::filter(St== x) %>% 
      plot_ly(x = ~Year) %>%
      add_lines(y =~Pop_Es, 
                line = list(shape = "linear", color = "#4fbfa8"),
                name = "Population Estimate") %>% 
      layout(xaxis = list(title = "",
                          showgrid = FALSE,
                          ticks = "",
                          fixedrange = TRUE),
             yaxis = list(title = "", fixedrange = TRUE, 
                          showticklabels = FALSE, showgrid = FALSE,
                          ticks = "", zeroline = TRUE, rangemode = "tozero"),
             annotations = list(text = "Source: Department of Statistics", align = "left",
                                x = 0, y = -.5, showarrow = FALSE,
                                xref = "paper", yref = "paper"),
             font = list(family = "'Raleway', sans-serif"),
             margin = list(l = 40, r = 40),
             paper_bgcolor = "rgba(1,1,1,0)",
             plot_bgcolor = "rgba(1,1,1,0)") %>% 
      config(displayModeBar = FALSE)
  }
  zillowSmalld <- function(x) {
    p %>% 
      dplyr::filter(St== x) %>% 
      plot_ly(x = ~Year) %>%
      add_lines(y =~Cl_Ws, 
                line = list(shape = "linear", color = "#4fbfa8"),
                name = "Clinical Waste") %>% 
      layout(xaxis = list(title = "",
                          showgrid = FALSE,
                          ticks = "",
                          fixedrange = TRUE),
             yaxis = list(title = "", fixedrange = TRUE, 
                          showticklabels = FALSE, showgrid = FALSE,
                          ticks = "", zeroline = TRUE, rangemode = "tozero"),
             annotations = list(text = "Source: Department of Statistics", align = "left",
                                x = 0, y = -.5, showarrow = FALSE,
                                xref = "paper", yref = "paper"),
             font = list(family = "'Raleway', sans-serif"),
             margin = list(l = 40, r = 40),
             paper_bgcolor = "rgba(1,1,1,0)",
             plot_bgcolor = "rgba(1,1,1,0)") %>% 
      config(displayModeBar = FALSE)
  }
  zillowSmalle <- function(x) {
    p %>% 
      dplyr::filter(St== x) %>% 
      plot_ly(x = ~Year) %>%
      add_lines(y =~Pop_Den, 
                line = list(shape = "linear", color = "#4fbfa8"),
                name = "Population Density") %>% 
      layout(xaxis = list(title = "",
                          showgrid = FALSE,
                          ticks = "",
                          fixedrange = TRUE),
             yaxis = list(title = "", fixedrange = TRUE, 
                          showticklabels = FALSE, showgrid = FALSE,
                          ticks = "", zeroline = TRUE, rangemode = "tozero"),
             annotations = list(text = "Source: Department of Statistics", align = "left",
                                x = 0, y = -.5, showarrow = FALSE,
                                xref = "paper", yref = "paper"),
             font = list(family = "'Raleway', sans-serif"),
             margin = list(l = 40, r = 40),
             paper_bgcolor = "rgba(1,1,1,0)",
             plot_bgcolor = "rgba(1,1,1,0)") %>% 
      config(displayModeBar = FALSE)
  }
  zillowSmallf <- function(x) {
    p %>% 
      dplyr::filter(St== x) %>% 
      plot_ly(x = ~Year) %>%
      add_lines(y =~Sch_Ws, 
                line = list(shape = "linear", color = "#4fbfa8"),
                name = "Scheduled Waste") %>% 
      layout(xaxis = list(title = "",
                          showgrid = FALSE,
                          ticks = "",
                          fixedrange = TRUE),
             yaxis = list(title = "", fixedrange = TRUE, 
                          showticklabels = FALSE, showgrid = FALSE,
                          ticks = "", zeroline = TRUE, rangemode = "tozero"),
             annotations = list(text = "Source: Department of Statistics", align = "left",
                                x = 0, y = -.5, showarrow = FALSE,
                                xref = "paper", yref = "paper"),
             font = list(family = "'Raleway', sans-serif"),
             margin = list(l = 40, r = 40),
             paper_bgcolor = "rgba(1,1,1,0)",
             plot_bgcolor = "rgba(1,1,1,0)") %>% 
      config(displayModeBar = FALSE)
  }
  zillowSmallg <- function(x) {
    p %>% 
      dplyr::filter(St== x) %>% 
      plot_ly(x = ~Year) %>%
      add_lines(y =~AR_OP, 
                line = list(shape = "linear", color = "#4fbfa8"),
                name = "Area Opened for Harvesting") %>% 
      layout(xaxis = list(title = "",
                          showgrid = FALSE,
                          ticks = "",
                          fixedrange = TRUE),
             yaxis = list(title = "", fixedrange = TRUE, 
                          showticklabels = FALSE, showgrid = FALSE,
                          ticks = "", zeroline = TRUE, rangemode = "tozero"),
             annotations = list(text = "Source: Department of Statistics", align = "left",
                                x = 0, y = -.5, showarrow = FALSE,
                                xref = "paper", yref = "paper"),
             font = list(family = "'Raleway', sans-serif"),
             margin = list(l = 40, r = 40),
             paper_bgcolor = "rgba(1,1,1,0)",
             plot_bgcolor = "rgba(1,1,1,0)") %>% 
      config(displayModeBar = FALSE)
  }
  # ------------
  # location 1
  
  location1 <- reactive({
    location(input$searchAddr1)
  })
  
  output$hoodName1 <- renderText({
    x <- as.data.frame(input$searchAddr1)
    x$hood
  })
  output$zillowLocation1 <- renderPlotly({
    x <- input$searchAddr1
    zillowSmall(x)
  })
  output$zillowLocation1b <- renderPlotly({
    x <- input$searchAddr1
    zillowSmallb(x)
  })
  output$zillowLocation1c <- renderPlotly({
    x <- input$searchAddr1
    zillowSmallc(x)
  })
  output$zillowLocation1d <- renderPlotly({
    x <- input$searchAddr1
    zillowSmalld(x)
  })
  output$zillowLocation1e <- renderPlotly({
    x <- input$searchAddr1
    zillowSmalle(x)
  })
  output$zillowLocation1f <- renderPlotly({
    x <- input$searchAddr1
    zillowSmallf(x)
  })
  output$zillowLocation1g <- renderPlotly({
    x <- input$searchAddr1
    zillowSmallg(x)
  })
# ------------
# location 2

location2 <- reactive({
  location(input$searchAddr2)
})

output$hoodName2 <- renderText({
  x <- as.data.frame(input$searchAddr2)
  x$hood
})

output$zillowLocation2 <- renderPlotly({
  x <- input$searchAddr2
  zillowSmall(x)
})
output$zillowLocation2b <- renderPlotly({
  x <- input$searchAddr2
  zillowSmallb(x)
})
output$zillowLocation2c <- renderPlotly({
  x <- input$searchAddr2
  zillowSmallc(x)
})
output$zillowLocation2d <- renderPlotly({
  x <- input$searchAddr2
  zillowSmalld(x)
})
output$zillowLocation2e <- renderPlotly({
  x <- input$searchAddr2
  zillowSmalle(x)
})
output$zillowLocation2f <- renderPlotly({
  x <- input$searchAddr2
  zillowSmallf(x)
})
output$zillowLocation2g <- renderPlotly({
  x <- input$searchAddr2
  zillowSmallg(x)
})


country %<>% filter(Year <= 2018)
fff <- subset(country, Year==2018)
output$ExGBox <- renderValueBox({
  valueBox(
    VB_style(fff$Land_Ar, "font-size: 60%;"),
    subtitle = "Land Area SQ Km",
    icon("stats",lib="glyphicon"),
    color = "green"
  )
})
output$ImTotBox <- renderValueBox({
  valueBox(
    VB_style( fff$Agricultural.land....of.land.area., "font-size: 60%;"),
    subtitle = "Agricultural land % of Land Area",
    icon("stats",lib="glyphicon"),
    color = "green"
  )
})
output$BlTotBox <- renderValueBox({
  valueBox(
    VB_style( fff$Surface.area..sq..km., "font-size: 60%;"),
    subtitle = "Surface area SQ KM",
    icon("stats",lib="glyphicon"),
    color = "red"
  )
})
output$ExSBox <- renderValueBox({
  valueBox(
    VB_style( fff$Renewable.energy.consumption....of.total.final.energy.consumption., "font-size: 60%;"),
    subtitle = "% of Renewable Energy Consumption",
    icon("stats",lib="glyphicon"),
    color = "green"
  )
})
output$ImSBox <- renderValueBox({
  valueBox(
    VB_style( fff$CO2.emissions..metric.tons.per.capita., "font-size: 60%;"),
    subtitle = "Co2 Emissions tonnes",
    icon("stats",lib="glyphicon"),
    color = "red"
  )
})
output$BlSBox <- renderValueBox({
  valueBox(
    VB_style( fff$Total.greenhouse.gas.emissions..kt.of.CO2.equivalent., "font-size: 60%;"),
    subtitle = "Greenhouse Tonne Emissions",
    icon("stats",lib="glyphicon"),
    color = "red"
  )
})
output$IEGSLineHc <-renderHighchart({
  highchart() %>%
    hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
    hc_chart(type = 'line') %>%
    hc_series( list(name = 'Carbon Damage', data =country$Adjusted.savings..carbon.dioxide.damage..current.US.., color='green', marker = list(symbol = 'circle') ),
               list(name = 'Energy Depletion', data =country$Adjusted.savings..energy.depletion..current.US.., color = 'green', dashStyle = 'shortDot', marker = list(symbol = 'triangle') ),
               list(name = 'National Savings', data =country$Adjusted.savings..net.national.savings..current.US.., color = 'red', marker = list(symbol = 'circle') ),
               list(name = 'Particulate Damage', data =country$Adjusted.net.savings..including.particulate.emission.damage..current.US.., color = 'red', dashStyle = 'shortDot', marker = list(symbol = 'triangle')  )
    )%>%
    hc_xAxis( categories = unique(country$Year) ) %>%
    hc_yAxis( title = list(text = "$ million, USD"),
              labels = list( format = "${value:,.0f} m")  ) %>%
    hc_plotOptions(column = list(
      dataLabels = list(enabled = F),
      #stacking = "normal",
      enableMouseTracking = T ) 
    )%>%
    hc_tooltip(table = TRUE,
               sort = TRUE,
               pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                     " {series.name}: ${point.y} m"),
               headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
    ) %>%
    hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )
})
output$GSTotalBalanceLineHc <-renderHighchart({
  highchart() %>%
    hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
    hc_chart(type = 'line') %>%
    hc_series( list(name = 'Mineral Rents', data =country$Mineral.rents....of.GDP., color='brown' , marker = list(enabled = F), lineWidth = 3 ),
               list(name = 'Forest Rents', data =country$Forest.rents....of.GDP., color = 'darkgreen', dashStyle = 'shortDot', marker = list(symbol = 'circle') ),
               list(name = 'Oil Rents', data =country$Oil.rents....of.GDP., color = 'darkblue', dashStyle = 'shortDot',  marker = list(symbol = 'triangle') )
    )%>%
    hc_xAxis( categories = unique(country$Year) ) %>%
    hc_yAxis( title = list(text = "$ % of, GDP"),
              labels = list( format = "${value:,.0f} m"),
              plotLines = list(
                list(#label = list(text = "This is a plotLine"),
                  color = "#ff0000",
                  #dashStyle = 'shortDot',
                  width = 2,
                  value = 0 ) )
    ) %>%
    hc_plotOptions(column = list(
      dataLabels = list(enabled = F),
      #stacking = "normal",
      enableMouseTracking = T ) 
    )%>%
    hc_tooltip(table = TRUE,
               sort = TRUE,
               pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                     " {series.name}: ${point.y} m"),
               headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
    ) %>%
    hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )
})
output$KeyExLine <- renderHighchart({
  highchart() %>%
    hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
    hc_add_series( data =  country$CO2.emissions.from.gaseous.fuel.consumption..kt. ,
                   mapping = hcaes(  x = Year, y = Value ),
                   type = 'line', name="Co2 Emissions from Gaseous Consumption",
                   marker = list(symbol = 'circle') 
    ) %>%
    hc_add_series( data =  country$Total.greenhouse.gas.emissions..kt.of.CO2.equivalent.,
                   mapping = hcaes(  x = Year, y = Value),
                   type = 'line', name="Total Greenhouse Emissions", dashStyle = 'DashDot', marker = list(symbol = 'circle') 
    ) %>%
    hc_xAxis( categories = c( unique( country$Year) ) ) %>%
    hc_yAxis( title = list(text = "$ million, USD"), #"Commodities and services exports over $1 bn"
              labels = list( format = "${value:,.0f} m")  ) %>%
    hc_plotOptions(line = list(
      dataLabels = list(enabled = F),
      #stacking = "normal",
      enableMouseTracking = T)
    )%>%
    hc_tooltip(table = TRUE,
               sort = TRUE,
               pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                     " {series.name}: ${point.y} m"),
               headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
    ) %>%
    hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
})
tmp_export_percent_hc <- 
  highchart() %>%
  hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
  hc_xAxis( categories = c( unique( country$Year) ) ) %>%
  hc_yAxis( title = list(text = "Percentage (%)"),
            labels = list( format = "{value:,.1f} %")  ) %>%
  hc_plotOptions(line = list(
    dataLabels = list(enabled = F),
    #stacking = "normal",
    enableMouseTracking = T)
  )%>%
  hc_tooltip(table = TRUE,
             sort = TRUE,
             pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                   " {series.name}: {point.y:,.1f} %"),
             headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
  ) 
output$KeyExLinePercent <- 
  renderHighchart({
    tmp_export_percent_hc %>%
      hc_add_series( data =  country$Renewable.energy.consumption....of.total.final.energy.consumption.,
                     mapping = hcaes(  x = country$Year, y = Value ),
                     type = 'line', name="Renewable Energy", dashStyle = 'DashDot', marker = list(symbol = 'circle')
                     
      )%>%
      hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
  })
output$KeyImLine <- renderHighchart({
  highchart() %>%
    hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
    hc_add_series( data =  country$Adjusted.savings..energy.depletion....of.GNI.,
                   mapping = hcaes(  x = Year, y = Value),
                   type = 'line',
                   name="Energy Depletion",
                   marker = list(symbol = 'circle') 
    ) %>%
    hc_add_series( data =  country$Adjusted.savings..carbon.dioxide.damage....of.GNI.,
                   mapping = hcaes(  x = Year, y = Value ),
                   type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle'), name="Particulate Emission damage"
    ) %>%
    hc_xAxis( categories = c( unique( country$Year) ) ) %>%
    hc_yAxis( title = list(text = "$ million, USD"), # "Commodities and services imports over $1 bn"
              labels = list( format = "${value:,.0f} m")  ) %>%
    hc_plotOptions(line = list(
      dataLabels = list(enabled = F),
      #stacking = "normal",
      enableMouseTracking = T)
    )%>%
    hc_tooltip(table = TRUE,
               sort = TRUE,
               pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                     " {series.name}: ${point.y} m"),
               headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
    ) %>%
    hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
})
tmp_import_percent_hc <- 
  highchart() %>%
  hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
  hc_xAxis(categories = c( unique( country$Year) ) ) %>%
  hc_yAxis( title = list(text = "Percentage (%)"),
            labels = list( format = "{value:,.1f} %")  ) %>%
  hc_plotOptions(line = list(
    dataLabels = list(enabled = F),
    #stacking = "normal",
    enableMouseTracking = T)
  )%>%
  hc_tooltip(table = TRUE,
             sort = TRUE,
             pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                   " CO2 Emissions: {point.y:,.1f} %"),
             headerFormat = '<span style="font-size: 13px">Year {country$Year}</span>'
  )
  

output$KeyImLinePercent <- 
  renderHighchart({
    tmp_import_percent_hc %>%
      hc_add_series( data =  country$CO2.emissions.from.solid.fuel.consumption....of.total.,
                     mapping = hcaes( x =Year),
                     type = 'line', name="Co2 Emissions from solid fuel", dashStyle = 'DashDot', marker = list(symbol = 'circle')
                     

      )%>%
      hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
  })
### plot
output$ExMarketLine <- renderHighchart({
  highchart() %>%
    hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
    hc_add_series( data =  country$Total.natural.resources.rents....of.GDP. ,
                   mapping = hcaes(  x = Year, y = Value),
                   type = 'line',
                   marker = list(symbol = 'circle'),
                   name="Total Natural Rent"
    ) %>%
    hc_xAxis( categories = c( unique(country$Year) ) ) %>%
    hc_yAxis( title = list(text = "$ million, USD"), #"Exports markets over $1bn"
              labels = list( format = "${value:,.0f} m")  ) %>%
    hc_plotOptions(line = list(
      dataLabels = list(enabled = F),
      #stacking = "normal",
      enableMouseTracking = T)
    )%>%
    hc_tooltip(table = TRUE,
               sort = TRUE,
               pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                     " {series.name}: ${point.y} m"),
               headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
    ) %>%
    hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
})
output$ExMarketLinePercent <-
  renderHighchart({
    highchart() %>%
      hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
      hc_add_series( data =  country$Mineral.rents....of.GDP. ,
                     mapping = hcaes(  x = Year, y = Value),
                     type = 'line',
                     marker = list(symbol = 'circle'),
                     name="Mineral Rents"
      ) %>%
      hc_xAxis( categories = c( unique( country$Year) )   ) %>%
      hc_yAxis( title = list(text = "Percentage (%)"),
                labels = list( format = "{value:,.1f} %")  ) %>%
      hc_plotOptions(line = list(
        dataLabels = list(enabled = F),
        #stacking = "normal",
        enableMouseTracking = T)
      )%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y:,.1f} %"),
                 headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
      ) %>%
      hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
  })
output$ExSMarketLine <- 
  renderHighchart({
    highchart() %>%
      hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
      hc_add_series( data =  country$Total.fisheries.production..metric.tons.,
                     mapping = hcaes(  x = Year, y = Value),
                     type = 'line',
                     marker = list(symbol = 'circle') ,
                     name="Total Fisheries"
      ) %>%
      hc_xAxis( categories = c( unique( country$Year) ) ) %>%
      hc_yAxis( title = list(text = "$ million, USD"), #"Exports markets over $500mn for services"
                labels = list( format = "${value:,.0f} m")  ) %>%
      hc_plotOptions(line = list(
        dataLabels = list(enabled = F),
        #stacking = "normal",
        enableMouseTracking = T)
      )%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: ${point.y} m"),
                 headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
      ) %>%
      hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
  })
output$ExSMarketLinePercent <-
  renderHighchart({
    highchart() %>%
      hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
      hc_add_series( data =  country$Access.to.clean.fuels.and.technologies.for.cooking....of.population.,
                     mapping = hcaes(  x = Year, y = Value),
                     type = 'line',
                     marker = list(symbol = 'circle'),
                     name="Access to clean fuel" 
      ) %>%
      hc_xAxis( categories = c( unique( country$Year) )   ) %>%
      hc_yAxis( title = list(text = "Percentage (%)"),
                labels = list( format = "{value:,.1f} %")  ) %>%
      hc_plotOptions(line = list(
        dataLabels = list(enabled = F),
        #stacking = "normal",
        enableMouseTracking = T)
      )%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y:,.1f} %"),
                 headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
      ) %>%
      hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
  })


})
