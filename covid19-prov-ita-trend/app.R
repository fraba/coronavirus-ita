#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load
setwd("/Users/143852/public_git/coronavirus-ita")
load("prov_wt_fit.RData")
provinces <- unique(prov_long.df$denominazione_provincia[order(prov_long.df$perc, decreasing = T)])

library(shiny)
library(leaflet)
library(sf)

load("prov_ita_simp.sf.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel(sprintf("Covid-19 | Aggiornato/Updated: %s", max(prov_long.df$date[!is.na(prov_long.df$totale_casi)]))),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("prov",
                           "Provincia/Province",
                           provinces)
        ),
        
        
        mainPanel(
            plotOutput("provPlot", height = 300),
            HTML("Day zero is estimated by prediction based on regression results. They are manually set for the provinces of Lodi and Cremona."),
            leafletOutput("provMap"),
            HTML("<p><a href='http://github.com/fraba/coronavirus-ita'>Replication/Data</a> | Hosted by <a href='https://nectar.org.au/'>nectar.org.au</a>, designed by <a href='https://www.uts.edu.au/staff/francesco.bailo'>me</a></p>")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    require(ggplot2)
    require(dplyr)
    
    dayzero <- reactive({
        return(unique(prov_long.df$pred_date_origin[prov_long.df$denominazione_provincia == input$prov]))
    })
    
    long <-  reactive({
        return(unique(prov_ita_simp.sf$long[prov_ita_simp.sf$DEN_UTS == input$prov]))
    })
    lat <- reactive({
        return(unique(prov_ita_simp.sf$lat[prov_ita_simp.sf$DEN_UTS == input$prov]))
    })
    
    
    output$provPlot <- renderPlot({
        
        prov_long.df %>% 
            filter(denominazione_provincia == input$prov) %>%
            ggplot(aes(x=date)) +
            geom_point(aes(y=perc, colour = "black")) +
            geom_line(aes(y=perc, colour = "black")) +
            geom_line(aes(y=wuhan_curve, colour = "blue")) +
            labs(title = sprintf("Casi COVID-19 Cases: Provincia di %s | Giorno/Day 0 (est.): %s", input$prov, dayzero()), 
                 x=NULL, y="per/every 10,000 abitanti/people", colour = NULL,
                 caption = sprintf("Data: Province of %s => github.com/pcm-dpc/COVID-19 | City of Wuhan => bit.ly/39rGZPr", input$prov)) +
            scale_color_manual(values = c("black", "blue"), labels = c(paste0(input$prov, " (observed)" ), "Wuhan (regr.)")) +
            theme_bw() + theme(legend.position = c(.8, .3))
    })
    
    output$provMap <- renderLeaflet({
        
        cuts <- c(0, .2, .5, .6, .7, .8, .9, .95, .99, 1)
        
        pal <- colorQuantile("YlGnBu", domain = prov_ita_simp.sf$perc,
                             probs = cuts)
        
        leaflet(prov_ita_simp.sf) %>%
            addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5,
                        fillColor = ~pal(perc),
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE),
                        popup=sprintf("<p><b>%s</b> Casi/Cases: %s (%s ‱)</p>", 
                                      prov_ita_simp.sf$DEN_UTS, prov_ita_simp.sf$totale_casi, 
                                      round(prov_ita_simp.sf$perc,1)),
                        popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE)
            ) %>%
            setView(lng = 10.4, lat = 45.3, zoom = 6) %>%
            addLegend(position = "bottomleft",
                      pal = pal, values = quantile(prov_ita_simp.sf$perc,
                                                   probs = cuts),
                      labFormat = function(type, cuts, p) {
                          n = length(cuts)
                          paste0(round(cuts[-1],2), "‱")
                      })
    })
    
    observeEvent(input$prov, {
        
        selected_polygon <- 
            prov_ita_simp.sf %>% dplyr::filter(DEN_UTS == input$prov)
        
        leafletProxy("provMap", session) %>% removeShape("highlighted_polygon")
        
        leafletProxy("provMap", session) %>% 
            setView(lng = long(), lat = lat(), zoom = 7) %>%
            addPolylines(data = selected_polygon, stroke=TRUE, weight = 4, color="red", 
                         layerId="highlighted_polygon")
            
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
