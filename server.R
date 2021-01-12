
function(input, output) {
 
# -------- TAB 1
    
## plotly: ecological footprint per region
    
output$ef_reg_plot <- renderPlotly({
        
        ef_region <- footprint %>% 
            group_by(Region) %>% 
            summarize(Ecological.Footprint = sum(Total.Ecological.Footprint)) %>% 
            arrange(desc(Ecological.Footprint)) %>% 
            mutate(text = paste0("Ecological Footprint: ", Ecological.Footprint, " gha"))
        
        ef_reg_plot <- ggplot(ef_region, aes(x=reorder(Region, Ecological.Footprint), y=Ecological.Footprint, text = text)) +
            geom_col(aes(fill=Ecological.Footprint), show.legend = F) +
            coord_flip() +
            labs(title = NULL,
                 y = "global hectares (gha)",
                 x = NULL) +
            scale_y_continuous(limits = c(0, 150),
                           breaks = seq(0,150, 25)) +
            scale_fill_gradient(low = "#F78181", high = "#3B0B0B") +
            theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.04),
              axis.ticks.y = element_blank(),
              panel.background = element_rect(fill = "#ffffff"), 
              panel.grid.major.x = element_line(colour = "grey"),
              axis.line.x = element_line(color = "grey"),
              axis.text = element_text(size = 10, colour = "black")
                 )
    
    ggplotly(ef_reg_plot, tooltip = "text")
    
})

# plotly: biocapacity per region

output$b_reg_plot <- renderPlotly({
    
    b_region <- footprint %>% 
        group_by(Region) %>% 
        summarize(Biocapacity = sum(Total.Biocapacity)) %>% 
        arrange(desc(Biocapacity)) %>% 
        mutate(text = paste0("Biocapacity: ", Biocapacity, " gha"))
    
    b_reg_plot <- ggplot(b_region, aes(x=reorder(Region, Biocapacity), y=Biocapacity, text = text)) +
        geom_col(aes(fill=Biocapacity), show.legend = F) +
        coord_flip() +
        labs(title = NULL,
             y = "global hectares (gha)",
             x = NULL) +
        scale_y_continuous(limits = c(0, 275),
                           breaks = seq(0,250, 50)) +
        scale_fill_gradient(low = "#9AFE2E", high = "#0B6121") +
        theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.04),
              axis.ticks.y = element_blank(),
              panel.background = element_rect(fill = "#ffffff"), 
              panel.grid.major.x = element_line(colour = "grey"),
              axis.line.x = element_line(color = "grey"),
              axis.text = element_text(size = 10, colour = "black"))
    
    ggplotly(b_reg_plot, tooltip = "text")

})

# plot: scatterplot HDI on Ecological Footprint

output$scat_plot <- renderPlotly({
    
    scat_plot <- ggplot(scat_plot_data, aes(x = Human.Development.Index, y = Ecological.Footprint, text = text)) +
        geom_smooth(col="#61380B", size = 0.7) +
        geom_point(aes(color = Status, size=GDP.per.Capita)) +
        scale_y_continuous(limits = c(0,18)) +
        scale_color_manual(values = c("#DF0101", "#04B486")) +
        labs(title = "HDI on Ecological Footprint",
             y = "Ecological Footprint",
             x = "Human Development Index") +
        theme(plot.title = element_text(face = "bold", size = 14, hjust = 0),
              panel.background = element_rect(fill = "#ffffff"), 
              panel.grid.major.x = element_line(colour = "grey"),
              panel.grid.major.y = element_line(colour = "grey"),
              axis.line.x = element_line(color = "grey"),
              axis.line.y = element_line(color = "grey"),
              axis.text = element_text(size = 10, colour = "black"),
              legend.title = element_blank())
    
    
    ggplotly(scat_plot, tooltip = "text") %>%
        layout(
            legend = list(orientation = "v",
                          y = 1, x = 0))
})

# -------- TAB 2

# leaflet

output$leaflet <- renderLeaflet({
    
   m <- leaflet(shape) %>% 
        addProviderTiles("Esri.NatGeoWorldMap") %>%

        # untuk menggunakan Google Map, gunakan addTiles (urlTemplate)
        # addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", 
        #         attribution = 'Google') %>% 
       
        setView( lat=10, lng=0 , zoom=2) %>%
    # for choropleth
        addPolygons( 
            fillColor = ~mypalette(diff), 
            color = "green",
            dashArray = "3", 
            fillOpacity = 0.6,
            weight=1,
            label = mytext,
            labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px", 
                direction = "auto"),
            popup = popup_shape
        ) %>%
        addLegend(pal=mypalette, 
                  values=~diff, opacity=0.9, 
                  title = paste("Remaining","<br>","Biocapacity (gha)"), 
                  position = "bottomleft")
    
    m
})

# plotly reactive: Ecologocal Footprint & Biocapacity Proportion

output$con_fill <- renderPlotly({

# filter with reactive
country_reg <- footprint %>%
    filter(Region == input$Region) %>%
    group_by(Country) %>%
    summarise("Ecological.Footprint" = Total.Ecological.Footprint,
              "Biocapacity" = Total.Biocapacity,
              "Status" = Status) %>%
    arrange(desc(Ecological.Footprint)) %>%
    rename("Ecological Footprint" = Ecological.Footprint)

# data aggregation
country_fill <- country_reg %>%
    select(-Status) %>%
    gather(key, value, -Country) %>%
    rename(Measure = key, Value = value)

# visualization
con_fill <- ggplot(country_fill, aes(x = Country, y = Value)) +
    geom_col(aes(fill=Measure), position = "fill") +
    geom_hline(yintercept = 0.5, linetype = "dashed", size = 1, col = "white") +
    scale_fill_manual(values = c("#04B486", "#DF0101")) +
    labs(title = NULL,
         x = NULL,
         y = NULL) +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 30, hjust = 1),
          plot.title = element_text(face = "bold"),
          panel.background = element_rect(fill = "#ffffff"),
          axis.line.y = element_line(colour = "grey"),
          axis.line.x = element_blank(),
          panel.grid = element_blank())

ggplotly(con_fill) %>% 
    layout(legend = list(orientation = "h",
                         y = 1.1, x = 0.5))

})

# plotly reactive: Ecologocal Footprint & Biocapacity Sources

output$con_def_plot <- renderPlotly({
    
    # filter with reactive
    con_dodge_ef <- footprint %>% 
        select(Country, Cropland.Footprint, Grazing.Footprint, Forest.Footprint, 
               Carbon.Footprint, Fish.Footprint, Cropland, Grazing.Land, Forest.Land, 
               Fishing.Water, Urban.Land) %>%
        rename("Cropland Footprint" = Cropland.Footprint, 
               "Grazing Footprint" = Grazing.Footprint, 
               "Carbon Footprint" = Carbon.Footprint, 
               "Forest Footprint" = Forest.Footprint, 
               "Fishing Footprint" = Fish.Footprint,  
               "Grazing Land" = Grazing.Land, 
               "Forest Land" = Forest.Land, 
               "Fishing Water" = Fishing.Water, 
               "Urban Land" = Urban.Land) %>%
        gather(key = "Source", value, -Country) %>% 
        mutate(text = paste0(Source,": ", value, " gha")) %>%
        filter(Country==input$Country) %>% 
    # reordering levels in `Source` for visual in legend
        mutate(Source = ordered(Source, levels = c("Fishing Footprint",
                                                   "Fishing Water",
                                                   "Forest Footprint",
                                                   "Forest Land",
                                                   "Grazing Footprint",
                                                   "Grazing Land",
                                                   "Cropland Footprint",
                                                   "Cropland",
                                                   "Carbon Footprint",
                                                   "Urban Land")))
    
    # visualization
    con_def_plot <- ggplot(con_dodge_ef, aes(x=Country, y=value, text = text)) +
        geom_col(aes(fill=Source), position = "dodge") +
        labs(title = "Ecological Footprint and Biocapacity Sources",
             x = NULL, y = NULL) +
        scale_fill_brewer(palette = "Paired") +
        theme(legend.position = "topright",
              legend.title = element_blank(),
              axis.text.y = element_text(colour = "black"),
              axis.text.x = element_blank(),
              plot.title = element_text(face = "bold"),
              panel.background = element_rect(fill = "#ffffff"),
              axis.line = element_blank(),
              axis.ticks.x = element_blank(),
              panel.grid.major.y = element_line(colour = "grey"),
              panel.grid.minor.y = element_line(colour = "grey"))
    
    ggplotly(con_def_plot, tooltip = "text")
    
})

# valuebox reactive: country
output$CountryBox <- renderValueBox({
    
    co <- footprint %>% 
        filter(Country == input$Country)
    
    valueBox(value = co$Country, subtitle = "Country", color = "green", icon = icon("flag"))

})

# valuebox reactive: Ecological Deficiency Status
output$StatusBox <- renderValueBox({
    
    status <- footprint %>% 
        filter(Country == input$Country)
    
    valueBox(value = status$Status, subtitle = "Ecological Status",
             color = ifelse(status$Status == "Reserve","green","red"), 
             icon = icon("leaf"))
})

# valuebox reactive: Ecological/Biocapacity
output$EFBBox <- renderValueBox({
    
    efb <- footprint %>% 
        filter(Country == input$Country)
    
    valueBox(value = paste0(efb$Total.Ecological.Footprint,"/",
                            efb$Total.Biocapacity), 
             subtitle = "Footprint/Biocapacity", 
             color = "teal",
             icon = icon("pagelines"))
    
})

# valuebox reactive: population number
# output$PopulationBox <- renderValueBox({
#     
#     pop <- footprint %>% 
#         filter(Country == input$Country)
#     
#     valueBox(value = pop$Population.millions, subtitle = "Population in Millions", color = "green",
#              icon = icon("users"))
# })

# valuebox reactive: HDI score
# output$HDIBox <- renderValueBox({
#     
#     hdi <- footprint %>% 
#         filter(Country == input$Country)
#     
#     valueBox(value = hdi$HDI, subtitle = "Human Development Index", color = "green", icon = icon("user-graduate"))
#     
# })

# valuebox reactive: GDP
# output$GDPBox <- renderValueBox({
#     
#     gdp <- footprint %>% 
#         filter(Country == input$Country)
#     
#     valueBox(value = gdp$GDP.per.Capita, subtitle = "GDP per Capita", color = "green",
#              icon = icon("money-bill-wave"))
# })

# valuebox reactive: Earths Required
# output$ERBox <- renderValueBox({
#      
#      er <- footprint %>% 
#          filter(Country == input$Country)
#      
#      valueBox(value = er$Earths.Required, subtitle = "Earths Required", color = "green",
#               icon = icon("globe-americas"))
#      
# })

# -------- TAB 3

output$data3 <- DT::renderDataTable(datadis, options = list(scrollX = T))

}