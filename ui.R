
header <- dashboardHeader(
    title = "Ecological Footprint"
)

sidebar <- dashboardSidebar(
    collapsed = F,
    sidebarMenu(
        menuItem(
            text = "Overview",
            tabName = "Overview",
            icon = icon("globe-asia")
        ),
        menuItem(
            text = "Eco-Status",
            tabName = "Eco-Status",
            icon = icon("leaf")
        ),
        menuItem(
            text = "Data",
            tabName = "Data",
            icon = icon("book")
        )
    )
)

body <- dashboardBody(
  tabItems(
      
    # TAB 1  
      
    tabItem(
        tabName = "Overview",
        fluidPage(
                h2(tags$b("Ecological Footprint")),
                br(),
                div(style = "text-align:justify", 
                    p("The Ecological Footprint is the metric that measures
                    how much nature we have (Biocapacity) and how much nature we use (Ecological Footprint).", 
                    "A country in ecological deficit meets demand by importing, 
                    liquidating its own ecological assets (such as overfishing), 
                    and/or emitting carbon dioxide into the atmosphere.",
                    "Based on the 2016 Ecological Footprint Data provided by Global Footprint Nework,
                    It is not exaggerating to say that our earth is running on an ecological crisis!"),
                    br()
                    )
                 ),
        
        fluidPage(
                tabBox(width = 8,
                       title = tags$b("Ecological Footprint per Region"),
                       id = "tabset1",
                       side = "right",
                       tabPanel(tags$b("Ecological Footprint"), 
                                plotlyOutput("ef_reg_plot")
                                ),
                       tabPanel(tags$b("Biocapacity"), 
                                plotlyOutput("b_reg_plot")
                                )    
                      ),
                valueBox("524.86 gha", 
                         "Total Ecological Footprint", 
                         icon = icon("city"),
                         color = "maroon"),
                valueBox("585.12 gha",
                         "Total Biocapacity",
                         icon = icon("tree"),
                         color = "green"),
                valueBox("117",
                         "Ecological Deficient Countries",
                         icon = icon("bomb"),
                         color = "maroon"),
                valueBox("45",
                        "Ecological Reserve Countries",
                         icon = icon("solar-panel"),
                         color = "green")
                 ),
        fluidPage(
            box(width = 9,
                plotlyOutput("scat_plot")
                ),
            box(width = 3,
                height = 425,
                h3("Go Think!"),
                p("Higher Human Development Index reveals higher Ecological Footprint for a Country. 
                  Nevertheles, it does not reflect country's ecological status. 
                  It really is depends on how a country manages its resources!"),
                br(),
                p("Find out more about your country ecological deficiency status on", tags$b("Eco-Status"),"!"),
                h3("Go Act!"),
                p("Find out more about your own footprint,
                  how to manage them and help nurture earth in Global Footprint Network!")
                )
                )
            ),
    
    # TAB 2
    
    tabItem(
        tabName = "Eco-Status",
        fluidPage(
            box(width = 12,
                solidHeader = T,
                h3(tags$b("Ecological Status of Each Country")),
                leafletOutput("leaflet", height = 530)),
            box(width = 9,
                solidHeader = T,
                title = tags$b("Ecological Footprint/Biocapacity Proportion"), 
                plotlyOutput("con_fill")),
            box(width = 3,
                solidHeader = T,
                background = "green",
                height = 460,
                selectInput(inputId = "Region",
                            label = h4(tags$b("Select Region:")),
                            choices = selectRegion))
                 ),
        fluidPage(
                box(solidHeader = T,
                    width = 9,
                    plotlyOutput("con_def_plot")),
                box(solidHeader = T,
                    width = 3,
                    height = 175,
                    background = "green",
                    selectInput(inputId = "Country",
                                label = h4(tags$b("Select Country:")),
                                choices = selectCountry)),
                valueBoxOutput(width = 3,
                               "StatusBox"),
                valueBoxOutput(width = 3,
                                "EFBBox")
                # valueBoxOutput("CountryBox"),
                # valueBoxOutput("PopulationBox"),
                # valueBoxOutput("HDIBox"),
                # valueBoxOutput("GDPBox"),
                # valueBoxOutput("ERBox")
            )
        ),
    
    # TAB 3
    
    tabItem(
        tabName = "Data",
        h2(tags$b("Ecological Footprint 2016 Data")),
        DT::dataTableOutput("data3")
            )
        )
    )

# Combining Dashboard Part
dashboardPage(
    header = header,
    body = body,
    sidebar = sidebar,
    skin = "green"
)