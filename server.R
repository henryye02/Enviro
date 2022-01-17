
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
library(raster)
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
                    direction = "auto")) %>% 
      leaflet::addLegend(pal = pal, 
                         values = palette.colors(), 
                         opacity = 0.7, 
                         title = NULL,
                         position = "bottomright")
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
    
  })
  
  observeEvent(input$searchAddr1, {
    shinyjs::hide("reactiveOutput7a")
  })
  
  observeEvent(input$searchAddr2, {
    shinyjs::hide("reactiveOutput7b")
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

# -------------
# CT comparison

output$CTcomparisonChart <- renderPlotly({
  l1 <- input$searchAddr1
  l2 <- input$searchAddr2
  
  l <- c(l1,l2)
  
  cols <- c("For_Area", "Non_For",
            "Res_Ar", "Pop_Es", 
            "Cl_Ws", "Sch_Ws","Mean_Temp", "Rain")
  cts <- p[,c("St",cols)] %>% data.frame() %>% filter(St %in% l )
  cts <- cts[,cols]
  ct1 <- cts[1,]
  ct2 <- cts[2,]
  
  d <- ct2[,1:ncol(ct2)] - ct1[,1:ncol(ct1)]
  
  max(abs(d))
  
  if(max(abs(d)) > .6){
    o <- .2
  } else if(max(abs(d)) > .2) {
    o <- .1
  } else {
    o <- 0
  }
  
  d %<>% gather() %>% 
    filter(value < 0) %>% mutate(
      valueOffset = value - o,
      offset = -o) %>% 
    inner_join(vars, by = c("key" = "ï..Data.Element")) %>% 
    bind_rows(
      d %>% gather() %>% 
        filter(value >= 0) %>% mutate(
          valueOffset = value + o,
          offset = +o) %>% 
        inner_join(vars, by = c("key" = "ï..Data.Element"))
    )
  
  # format long labels (wrap long labels with <br> tag)
  wrap_strings <- function(vector_of_strings, width){
    as.character(
      sapply(vector_of_strings,
             function(x){
               paste(strwrap(x, width), collapse="<br>")
             })
    )
  }
  
  d$Description %<>% wrap_strings(20)
  
  r <- round(max(abs(range(d$valueOffset)))+.05,1)
  r <- pretty(range(-r,r))
  rLabels <- abs(r)
  rLabels[rLabels != 0] <- as.numeric(rLabels[rLabels != 0]) - o
  rLabels <- sprintf("%.0f", abs(rLabels * 100))
  rLabels %<>% paste0("%")
  rLabels[rLabels == "0%"] <- ""
  
  d %>% slice(match(cols, key)) %>% 
    plot_ly(showlegend = FALSE) %>% 
    add_trace(x = ~offset, y = ~key, type = "bar", marker = list(color = "rgba(1,1,1,0)")) %>% 
    add_trace(x = ~value,  y = ~key, type = "bar", marker = list(color = "#436983")) %>% 
    add_text(x = 0, y = ~key, text = ~d$Description, type = "scatter", 
             hoverinfo = "text", mode = "text",
             textfont = list(family = "'Raleway', sans-serif", 
                             size = 14, color = "#hhh")) %>% 
    layout(barmode = "stack", hovermode = FALSE,
           xaxis = list(title = "", ticks = "",
                        range = c(-max(r),max(r)),
                        fixedrange = TRUE, 
                        zeroline = TRUE, zerolinecolor = "#f2fcff",
                        tickmode = "array",
                        tickvals = r,
                        ticktext = rLabels),
           yaxis = list(title = "", fixedrange = TRUE,
                        showticklabels = FALSE),
           shapes = list(list(x0 = -o, x1 = -o, y0 = -1, y1 = nrow(d),
                              line = list(color = "#adadad")),
                         list(x0 =  o, x1 =  o, y0 = -1, y1 = nrow(d),
                              line = list(color = "#adadad"))),
           annotations = list(
             list(text = "Source: ACS", align = "left",
                  x = 0, y = -.3, showarrow = FALSE,
                  xref = "paper", yref = "paper"),
             list(text = paste("Higher value in",l1$hood), 
                  font = list(size = 16),
                  align = "center", xref = "paper",
                  yref = "paper", xanchor = "center",
                  x = .25, y = 1.1, showarrow = FALSE),
             list(text = paste("Higher value in",l2$hood), 
                  font = list(size = 16),
                  align = "center", xref = "paper",
                  yref = "paper", xanchor = "center",
                  x = .75, y = 1.1, showarrow = FALSE)
           ),
           margin = list(l = 10, r = 10, t = 80, b = 80, pad = 5),
           font = list(family = "'Raleway', sans-serif"),
           paper_bgcolor = "rgba(1,1,1,0)",
           plot_bgcolor = "rgba(1,1,1,0)") %>% 
    config(displayModeBar = FALSE)
  print(ct1)
})

output$CTcomparisonTable <- DT::renderDataTable({
  l1 <- input$searchAddr1
  l2 <- input$searchAddr2
  
  l <- c(l1,l2)
  
  cols <- c("For_Area", "Non_For",
            "Res_Ar", "Pop_Es",
            "Cl_Ws", "Sch_Ws","Mean_Temp", "Rain")
  
  cts <- p[,c("St",cols)] %>% data.frame() %>% filter(St %in% l)
  cts$St %<>% paste("St:",.)
  ct1 <- cts[1,]
  ct2 <- cts[2,]
  
  
  
  t <- ct1 %>% gather("key","value1",2:length(ct1)) %>% dplyr::select(key, value1) %>% 
    bind_cols(
      ct2 %>% gather("key","value2",2:length(ct2)) %>% dplyr::select(value2)
    ) %>% 
    inner_join(vars, by = c("key" = "ï..Data.Element")) %>% 
    slice(match(cols, key)) %>% 
    dplyr::select(value1, Description, value2, Type)
  
  for(r in 1:nrow(t)){
    t[r,"value1"] <- format_metric(t[r,"value1"], t[r,"Type"])
    t[r,"value2"] <- format_metric(t[r,"value2"], t[r,"Type"])
  }
  
  t
  
  t %>% 
    dplyr::select(value2, Description, value1) %>% 
    datatable(colnames = c("","Metric",""),
              rownames = FALSE,
              filter = "none",
              options = list(
                paging = FALSE, searching = FALSE,
                sort = FALSE, info = FALSE,
                columnDefs = list(list(className = 'dt-center', targets = 0:2,
                                       width = '200px', targets = c(2)))))
})
}) 
