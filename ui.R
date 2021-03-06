
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
    ),
    menuItem("Source Code", icon = icon("file-code-o"), 
             href = "https://github.com/NabiilahArdini/Eco-Status")
  )
)

body <- dashboardBody(
  
  # using custom CSS (disable dashboard skins)
  
  # tags$head(tags$style(HTML('
  #                               /* logo */
  #                               .skin-blue .main-header .logo {
  #                               background-color: darkslategray;
  #                               font-family: "Bahnschrift";
  #                               }
  # 
  #                               /* logo when hovered */
  #                               .skin-blue .main-header .logo:hover {
  #                               background-color: darkslategray;
  #                               font-family: "Bahnschrift";
  #                               }
  # 
  #                               /* navbar (rest of the header) */
  #                               .skin-blue .main-header .navbar {
  #                               background-color: darkslategray;
  #                               }
  # 
  #                               /* main sidebar */
  #                               .skin-blue .main-sidebar {
  #                               background-color: darkslategray;
  #                               font-family: "Bahnschrift";
  #                               }
  # 
  #                               /* active selected tab in the sidebarmenu */
  #                               .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
  #                               background-color: seagreen;
  #                               color: white;
  #                               font-family: "Bahnschrift";
  #                               }
  # 
  #                               /* other links in the sidebarmenu */
  #                               .skin-blue .main-sidebar .sidebar .sidebar-menu a{
  #                               background-color: darkslategray;
  #                               color: white;
  #                               font-family: "Bahnschrift";
  #                               }
  # 
  #                               /* other links in the sidebarmenu when hovered */
  #                               .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
  #                               background-color: seagreen;
  #                               color: white;
  #                               font-family: "Bahnschrift";
  #                               }
  #                               /* toggle button when hovered  */
  #                               .skin-blue .main-header .navbar .sidebar-toggle:hover{
  #                               background-color: seagreen;
  #                               }
  # 
  #                               /* body */
  #                               .content-wrapper, .right-side {
  #                               background-color: #ffffff;
  #                               font-family: "Bahnschrift";
  #                               
  #                               }
  # 
  #                               '))),
  
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
            div(style = "text-align:justify",
                p("Higher Human Development Index (HDI) reveals higher Ecological Footprint for a Country. 
                  Nevertheles, it does not reflect country's ecological status. 
                  It really depends on how a country manages its resources!"),
                br(),
                p("Find out more about your country ecological deficiency status on", tags$b("Eco-Status"),"!"),
                h3("Go Act!"),
                p("Find out more about your own footprint and help nurture earth in", 
                  a(href = "https://www.footprintnetwork.org/",
                    "Global Footprint Network"))
            )
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
