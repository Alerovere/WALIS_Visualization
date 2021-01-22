# Load packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(shinyWidgets)
library(readr)
library(DT)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(ggplot2) 
library(gtable) 
library(ggnewscale)
library(ggiraph)

# Load data
# Define UI

latest_walis<-"http://www.warmcoasts.eu/sealevel/RSLmap/analysis_summary.csv"
rsl_summary_file <-"walis.csv"

download.file(latest_walis, rsl_summary_file)
rsl_summary <- read.csv(rsl_summary_file)

rsl_indicator <- unique(rsl_summary$RSL.Indicator)

rsl_indicator <-
  rsl_indicator[order(nchar(rsl_indicator), rsl_indicator)]

ui <-
  navbarPage(
    
    title = div(img(src = "walis_logo.png", width = "30")),
    windowTitle = "Walis Explorer" ,
    id = "nav",
    theme = shinytheme("flatly"),
    tabPanel(
      "Interactive map",
      icon = icon("map"),
      tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
      includeCSS("style.css"),
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          fluidRow(
            column(10,p(strong('The World Atlas of Last Interglacial Shorelines'), style = "font-size:22px;")),
            column(1,
                    dropdownButton(
                      fluidRow(
                        column(1,img(src = "walis_logo.png", width = "25")),
                        column(11,strong("The World Atlas of Last Interglacial Shorelines", style = "font-size:15px;"))),
                      br(),
                      p("As part of the", a("WARMCOASTS project",href="https://warmcoasts.eu/index.html"), 
                        "and in collaboration with", a("PALSEA",href="https://palseagroup.weebly.com/")," (a PAGES-INQUA working group), 
                               we launched WALIS. 
                               WALIS aims at collecting existing and new data on Last Interglacial sea-level indicators reviewed following a standardized template.",style="font-size:12px;text-align:justify"),
                      p(strong("WALIS EXPLORER")," is one of the tools designed to easily explore the Last Interglacial sea-level indicators available in ",strong("WALIS"),".",style="font-size:12px;text-align:justify"),
                      p("Keep exploring WALIS in the following resources:",style="font-size:12px;text-align:justify"),
                      fluidRow(
                        column(4,
                               br(),
                               p(a(icon("desktop","fa-4x"), href="https://warmcoasts.eu/world-atlas.html"),style="text-align:center"),
                               p("Visualize and download WALIS. You can also contribute with new data !",style="font-size:10px;text-align:justify")),
                        column(4,
                               br(),
                               p(a(icon("code-branch","fa-4x"), href="https://github.com/sbastiangarzon"),style="text-align:center"),
                               p("Fork our ",strong(icon('github'), 'WALIS-Explorer '),"repository to create new features to explore WALIS. You can also report issues and create pull requests.",style="font-size:10px;text-align:justify"),
                        ),
                        column(4,
                               br(),
                               p(a(icon("twitter","fa-4x"), href="https://twitter.com/WALISDatabase"),style="text-align:center"),
                               p("Feed of the work-in-progress research effort to build a World Atlas of Last Interglacial Shorelines",style="font-size:10px;text-align:justify"),
                        )
                      ),
                      fluidRow(column(10,
                                      br(),
                                      p(strong("WALIS EXPLORER"),"Designed by:", a(icon("github"),"sbastiangarzon",href="https://github.com/sbastiangarzon"),"/",
                                        a(icon("github"),"Alerovere",href="https://github.com/Alerovere"),style="font-size:10px;text-align:right"),
                                      offset=2))
                      ,     
                      status = "primary",
                      size = "sm",
                      icon = icon("info", "fa-1x"),
                      width= "400px",
                      tooltip = tooltipOptions(title = "Info")
                    )
                   )      
                   
                   ),
          p(
            "Welcome to ",
            strong("WALIS EXPLORER"),
            ". Customize your search and graphics by changing the parameters in the",
            strong(icon("stopwatch"),"Age filter", style = "color: blue"),
            ",",
            strong(icon("filter","glyphicon"),"RSL indicator filter", style = "color: orange"),
            ",",
            strong(icon("globe",lib="glyphicon"),"Geographic Extent", style = "color: green"),
            "and ",
            strong("\U25c8 Indicator type (map)", style = "color: red"),
            " menu.",
            style = "font-size:15px;text-align:justify"
          ),
          #https://github.com/dreamRs/shinyWidgets/blob/master/R/input-sliderText.R
          
          fluidRow(
            column(
              1,
              offset = 3,
              dropdownButton(
                p(strong("Age filter menu", style = "font-size:20px")),
                fluidRow(column(
                  8,
                  sliderTextInput(
                    inputId = "temp",
                    label = span(tagList(icon("history"), "Age Range")),
                    grid = TRUE,
                    choices = seq(from = 500, to = 0, by = -5),
                    selected = c(500, 0),
                    width = "100%",
                    post = " (ka)",
                    
                  )
                ),
                column(
                  4,
                  br(),
                  p(
                    style = "font-size:10px;text-align:justify",
                    "Select the Age Range of your query.
            Only RSL within this range are going to be displayed."
                  )
                ))
                ,
                fluidRow(column(
                  7,
                  radioGroupButtons(
                    justified = FALSE,
                    inputId = "perc_age",
                    status = "info",
                    size = "xs",
                    label = span(tagList(
                      icon("arrows-alt-h"), "Percentiles range"
                    )),
                    choices = c(
                      "15.9-84.9" = "1",
                      "2.3-97.7" = "2",
                      "0.1-99.5" = "3"
                    ),
                    selected = c("1")
                  )
                ),
                column(
                  5,
                  br(),
                  p(
                    style = "font-size:10px;text-align:justify",
                    "Dated ages are represented as probability distribution functions.
              Select the range (percentiles) used for all ages."
                  )
                )),
                fluidRow(column(
                  7,
                  checkboxGroupButtons(
                    inputId = "dating_tech",
                    label = span(tagList(icon("flask"), "Dating techniques:")),
                    #choices = c("Uranium-series"="U-Series" ,"Amino Acid Racemization"="AAR", "Electron spin resonance"="ESR","Stratigraphy"="Stratigraphic constraint","Other"="Other age constraint"),
                    choices = c(
                      "U-Series" = "U-Series",
                      "AAR" = "AAR",
                      "ESR" = "ESR",
                      "Luminescence" =
                        "Luminescence",
                      "Stratigraphy" = "Stratigraphic constraint",
                      "Other" = "Other age constraint"
                    ),
                    selected = c(
                      "U-Series",
                      "AAR",
                      "ESR",
                      "Luminescence",
                      "Stratigraphic constraint",
                      "Other age constraint"
                    ),
                    justified = FALSE,
                    status = "info",
                    size = "xs",
                    width = "100%",
                    checkIcon = list(
                      yes = icon("ok", lib = "glyphicon"),
                      no = icon("remove", lib = "glyphicon")
                    )
                  )
                ),
                column(
                  5,
                  br(),
                  p(
                    style = "font-size:10px;text-align:justify",
                    "Select RSL by dating technique. Only RSL dated with these techniques are going to be displayed."
                  )
                ))
                ,
                circle = TRUE,
                status = "info",
                icon = icon("stopwatch", "fa-1x"),
                width = "400px",
                tooltip = tooltipOptions(title = "Age filter menu")
              )
            ),
            column(
              1,
              offset = 1,
              dropdownButton(
                p(strong("RSL Indicator Filter", style = "font-size:20px")),
                strong("Elevation measurements"),
                br(),
                fluidRow(column(
                  8,
                  sliderTextInput(
                    inputId = "elev_error",
                    label = span(tagList(icon("ruler"), "Elevation error:")),
                    grid = TRUE,
                    choices = c(c(seq(0, 20, 1), "More than 20")),
                    selected = c(0, "More than 20"),
                    width = "100%",
                    post = "(m)",
                  )
                ), column(
                  4,
                  br(),
                  p(
                    style = "font-size:10px;text-align:justify",
                    "Select a range of errors associated with the elevation measurement. This variable applies to all types of RSL indicators."
                  )
                )),
                strong("Paleo-RSL"),
                br(),
                fluidRow(column(
                  6,
                  column(
                    6,
                    radioGroupButtons(
                      justified = TRUE,
                      inputId = "perc_elev",
                      status = "warning",
                      size = "xs",
                      direction = "vertical",
                      label = span(tagList(
                        icon("arrows-alt-h"), "Percentiles range:"
                      )),
                      choices = c(
                        "15.9-84.9" = "1",
                        "2.3-97.7" = "2",
                        "0.1-99.5" = "3"
                      ),
                      selected = c("1")
                    )
                  ), column(
                    6,
                    br(),
                    br(),
                    p(
                      style = "font-size:10px;text-align:justify",
                      "Elevations of Sea level index points (SLI) are represented as probability distribution functions.
                                    Select the percentiles used for SLI points elevations."
                    )
                  )
                ),
                column(6,
                       fluidRow(
                         column(
                           7,
                           sliderTextInput(
                             inputId = "elev_uncert",
                             label = span(tagList(icon("ruler"), "Uncertainty (m):")),
                             grid = TRUE,
                             choices = c(c(seq(0, 20, 1), "More than 20")),
                             selected = c(0, "More than 20"),
                             width = "100%",
                             post = "(m)",
                           )
                         ), column(
                           5,
                           br(),
                           br(),
                           p(
                             style = "font-size:10px;text-align:justify",
                             "Select a range of Paleo RSL uncertainty (m) for Sea level index points (SLI). This variable does not applies to Limiting (Marine or Terrestrial) RSL."
                           )
                         )
                       ))),
                fluidRow(column(
                  10,
                  strong("RSL filter menu"),
                  selectInput(
                    inputId = 'type_indicators',
                    label = NULL,
                    width = "100%",
                    choices = rsl_indicator,
                    selected = rsl_indicator,
                    multiple = TRUE,
                    selectize = TRUE
                  ),
                ), column(
                  2,
                  br(),
                  p(style = "font-size:10px;text-align:justify", "Add and remove RSL Indicator types.")
                ))
                ,
                circle = TRUE,
                status = "warning",
                icon = icon("filter", "fa-1x"),
                width = "600px",
                tooltip = tooltipOptions(title = "RSL indicator filter menu")
              )
            )
            ,
            column(
              1,
              offset = 1,
              dropdownButton(
                p(strong("Geographic extent", style = "font-size:20px")),
                fluidRow(column(6,
                                radioGroupButtons(
                                  inputId = "geo",
                                  label = "Area of interest:",
                                  choices = c(
                                    `<i class="far fa-square"></i> From extent of map` = "map",
                                    `<i class="fas fa-draw-polygon"></i> From polygon (draw)` = "polygon"
                                  ),
                                  selected = c("map"),
                                  justified = TRUE,
                                  status = "success",
                                  direction = "vertical",
                                ))
                         ,column(6,
                                 br(),
                                 p("Select the area of interest selection method. ",
                                   icon("square"),strong('From extend of map'), "uses the current area of the map.",
                                   icon("draw-polygon"),strong('From polygon'), "enables a drawing selection tool.",
                                   style = "font-size:10px;text-align:justify",
                                 )
                         ))
                ,
                circle = TRUE,
                status = "success",
                icon = icon("globe",lib="glyphicon"),
                width = "400px",
                tooltip = tooltipOptions(title = "Geographic extent menu")
                
              )
            ),
          ),
          fluidRow(column(10,p(
            span(tagList(icon("water")), style = "color:#000101"),
            strong("Sea level"),
            style = "font-size:30px"
          )),
          column(1,
                 dropdownButton(
                   strong("Download menu", style = "font-size:20px"),
                   br(),
                   strong("Summary Table"),
                   p(style = "font-size:10px;text-align:justify", 
                     "Summary table consist on a portion of WALIS. The table 
                contains the most relevant information for each RSL. 
                For more details download the full WALIS Database.")
                   ,
                   fluidRow(
                     column(6, downloadButton("downloadDataTable", "Current selection")),
                     column(6,
                            p(style = "font-size:10px;text-align:justify", "Download", strong("summary table") ,"of current RSL selection. 
                         You can preview the content of this file in the",icon("book"), strong("Summary table tab."))
                     )
                   ), 
                   br(),
                   strong("WALIS - Database"),
                   p(style = "font-size:10px;text-align:justify",
                     "Full WALIS Database. The database includes files, scripts and metadata required to produce the Summary table."),
                   fluidRow(column(6,
                                   br(),
                                   actionButton(inputId='ab1', label="WALIS - Database",
                                                width = "100%",
                                                icon = icon("database"), 
                                                onclick ="window.open('https://zenodo.org/communities/walis_database?page=1&size=20')"),
                   ),
                   column(6,
                          p(style = "font-size:10px;text-align:justify", 
                            " WALIS Zenodo repository. 
                This Repository contains WALIS data in different formats,
                as well as scripts to query the database.
                The content coincides with the data on the Special Issue
                in the",a("journal Earth System Science Data",href= "https://essd.copernicus.org/articles/special_issue1055.html") 
                          )       
                   ),
                   column(12,
                          p(style = "font-size:10px;text-align:justify",
                            "WALIS is the result of the work of several people, within different projects. We kindly ask you to follow three simple rules to properly acknowledge those who worked on it: ",
                            strong("1. Cite the original authors"),br(),
                            strong("2. Acknowledge the database contributor"),br(),
                            strong("3. Acknowledge the database structure creators"),br(),
                            a("More details and examples on how to cite", href="https://walis-help.readthedocs.io/en/latest/citation/")
                          ))
                   ),
                   width = "400px",
                   size = "sm",
                   status = "primary",
                   icon = icon("download", "fa-1x"),
                   tooltip = tooltipOptions(title = "Download menu")
                 )
                 
                 )
                 )
          ,
          fluidRow(column(12,
          girafeOutput("mygraph", width = "100%",
                       height = "300px"))),
          fluidRow(
                  column(4,
                  strong("Terrestrial Limiting",style ="text-align:center;font-size:10px"),
                  fluidRow(
                   column(3,
                   dropdownButton(
                     p(strong("Terrestrial Limiting (Younger than...)")),
                     fluidRow(column(3,
                                     p(icon("level-down-alt","fa-flip-horizontal"), style="font-size:70px;color: red;text-align:center")
                                     ),column(9,
                                              p(p(strong("Arrow position:"),"Timing constraint"),
                                                p(strong("Timing constraint:"),"Younger than"),
                                                p(strong("Horizontal length:"), "Age range"),
                                              p(strong("Vertical length: "),"No meaning"))),style="font-size:12px"),
                     
                     up = TRUE,
                     width = "275px",
                     tooltip = tooltipOptions(title = "Younger than..."),
                     size = "sm",
                     icon = p(icon("level-down-alt","fa-flip-horizontal"), style="color: red")
                                     )),
                   column(3,dropdownButton(
                     p(strong("Terrestrial Limiting (Equal to)")),
                     fluidRow(column(3,
                                     p("\u21A7", style="color:red;font-size:80px;text-align:center")
                     ),column(9,
                              p(
                                p(strong("Arrow position:"),"Timing constraint"),
                                p(strong("Timing constraint:"),"Equal to"),
                                p(strong("Horizontal length:"), "Age range"),
                                p(strong("Vertical length: "),"No meaning"))),style="font-size:12px"),
                     width = "275px",
                     up = TRUE,
                     tooltip = tooltipOptions(title = "Equal to..."),
                     size = "sm",
                     icon = p("\u21A7", style="color:red;font-size:30px")
                   )),
                   column(3,dropdownButton(
                     p(strong("Terrestrial Limiting (Older than)")),
                     fluidRow(column(3,
                                     p("\u21B4", style="color: red;font-size:80px;text-align:center")),
                                     column(9,
                              p(
                                p(strong("Arrow position:"),"Timing constraint"),
                                p(strong("Timing constraint:"),"Older than"),
                                p(strong("Horizontal length:"), "Age range"),
                                p(strong("Vertical length: "),"No meaning"))),style="font-size:12px"),
                     width = "275px",
                     up = TRUE,
                     tooltip = tooltipOptions(title = "Older than..."),
                     size = "sm",
                     icon = p("\u21B4", style="color: red;font-size:30px")
                   )))),
                  column(4,
                         strong("Marine Limiting",style ="text-align:center;font-size:10px"),
                         fluidRow(
                           column(3,
                                  dropdownButton(
                                    p(strong("Marine Limiting (Younger than)")),
                                    fluidRow(column(3,
                                                    p(icon("level-up-alt","fa-flip-horizontal"), style="color: blue;font-size:70px;text-align:center")
                                    ),column(9,
                                             p(
                                               p(strong("Arrow position:"),"Timing constraint"),
                                               p(strong("Timing constraint:"),"Younger than"),
                                               p(strong("Horizontal length:"), "Age range"),
                                               p(strong("Vertical length: "),"No meaning"))),style="font-size:12px"),
                                    width = "275px",
                                    up = TRUE,
                                    tooltip = tooltipOptions(title = "Younger than..."),
                                    size = "sm",
                                    icon = p(icon("level-up-alt","fa-flip-horizontal"), style="color: blue")
                                  )),
                           column(3,dropdownButton(
                             p(strong("Marine Limiting (Equal to)")),
                             fluidRow(column(3,
                                             p("\u21A5", style="color:blue;font-size:80px")),column(9,
                                      p(
                                        p(strong("Arrow position:"),"Timing constraint"),
                                        p(strong("Timing constraint:"),"Equal to"),
                                        p(strong("Horizontal length:"), "Age range"),
                                        p(strong("Vertical length: "),"No meaning"))),style="font-size:12px"),
                             width = "275px",
                             up = TRUE,
                             tooltip = tooltipOptions(title = "Equal to..."),
                             size = "sm",
                             icon = p("\u21A5", style="color:blue;font-size:30px")
                           )),
                           column(3,dropdownButton(
                             p(strong("Marine Limiting (Older than)")),
                             fluidRow(column(3,
                                             p(icon("level-up-alt"), style="color: blue;font-size:70px")),
                                      column(9, p(p(strong("Arrow position:"),"Timing constraint"), 
                                                  p(strong("Timing constraint:"),"Older than"),
                                                  p(strong("Horizontal length:"), "Age range"),
                                                  p(strong("Vertical length: "),"No meaning"))),style="font-size:12px"),
                             width = "275px",
                             up = TRUE,
                             tooltip = tooltipOptions(title = "Older than..."),
                             size = "sm",
                             icon = p(icon("level-up-alt"), style="color: blue")
                           )))),
                  column(4,
                         strong("Sea-level index",style ="text-align:center;font-size:10px"),
                         fluidRow(
                           column(3,
                                  dropdownButton(
                                    p(strong("Sea-level index (Younger than)")),
                                    fluidRow(column(3,
                                                    p("\u2348", style="color: orange;font-size:70px")),
                                             column(9, p(p(strong("Arrow direction:"),"Timing constraint"), 
                                                         p(strong("Timing constraint:"),"Younger than"),
                                                         p(strong("Horizontal length:"), "Age range"),
                                                         p(strong("Vertical length: "),"Paleo-RSL elevation range"))),style="font-size:12px"),
                                    width = "350px",
                                    up = TRUE,
                                    tooltip = tooltipOptions(title = "Younger than..."),
                                    size = "sm",
                                    icon = p("\u2348", style="color: orange;font-size:30px")
                                  )),
                           column(3,dropdownButton(
                             p(strong("Sea-level index (Equal to)")),
                             fluidRow(column(3,
                                             p("\u229e", style="color:cyan;font-size:70px")),
                                      column(9, p( 
                                                  p(strong("Timing constraint:"),"Equal to"),
                                                  p(strong("Horizontal length:"), "Age range"),
                                                  p(strong("Vertical length: "),"Paleo-RSL elevation range"))),style="font-size:12px"),
                             width = "350px",
                             up = TRUE,
                             tooltip = tooltipOptions(title = "Equal to..."),
                             size = "sm",
                             icon = p("\u229e", style="color:cyan;font-size:30px")
                           )),
                           column(3,dropdownButton(
                             p(strong("Sea-level index (Older than)")),
                             fluidRow(column(3,
                                             p("\u2347", style="color: purple;font-size:70px")),
                                      column(9, p(p(strong("Arrow direction:"),"Timing constraint"), 
                                                  p(strong("Timing constraint:"),"Older than"),
                                                  p(strong("Horizontal length:"), "Age range"),
                                                  p(strong("Vertical length: "),"Paleo-RSL elevation range"))),style="font-size:12px"),
                             width = "350px",
                             up = TRUE,
                             tooltip = tooltipOptions(title = "Older than..."),
                             size = "sm",
                             icon = p("\u2347", style="color: purple;font-size:30px")
                           ))))
                  )
        ),
        
        mainPanel = mainPanel(fluidRow(column(
          12, uiOutput("panel")
        )),
        leafletOutput("mymap", height = 550),
      p("Designed by:", a(icon("github"),"sbastiangarzon",href="https://github.com/sbastiangarzon"),"/",
         a(icon("github"),"Alerovere",href="https://github.com/Alerovere"),style="font-size:10px;text-align:right"))))
    ,
    tabPanel(
      title = "Summary table",
      value = "Summary_table",
      icon = icon("book"),
      fluidPage(
        strong("Last Interglacial sea-level indicators"),
        p("This tab contains the",strong("Summary table"), "of the current selection of RSL.",
          strong(icon("stopwatch"),"Age", style = "color: blue"),",",
          strong(icon("filter","glyphicon"),"RSL indicator", style = "color: orange"),
          " and ", strong("\U25c8 Indicator type (map)", style = "color: red"),
          " filters, as well as the", strong(icon("globe",lib="glyphicon"),"Geographic Extent", style = "color: green")," of the current selection, are summarized in the following section.",
          "You can modify the selection in the ", strong(icon("map"),"Interactive map tab."))
        ,
        fluidRow(
        column(6,
          strong("Filters"),
          uiOutput("panel_2")
        ),
        column(4,
               strong(icon("globe",lib="glyphicon"),"Geographic Extent"),
               leafletOutput("minimap",height = 120)
               ),
        column(2,
              strong("Download"),
               br(),
                 dropdownButton(
                   strong("Download menu", style = "font-size:20px"),
                   br(),
                   strong("Summary Table"),
                   p(style = "font-size:10px;text-align:justify", 
                     "Summary table consist on a portion of WALIS. The table 
                contains the most relevant information for each RSL. 
                For more details download the full WALIS Database.")
                   ,
                   fluidRow(
                     column(6, downloadButton("downloadDataTable_2", "Current selection")),
                     column(6,
                            p(style = "font-size:10px;text-align:justify", "Download", strong("summary table") ,"of current RSL selection.")
                     )
                   ), 
                   br(),
                   strong("WALIS - Database"),
                   p(style = "font-size:10px;text-align:justify",
                     "Full WALIS Database. The database includes files, scripts and metadata required to produce the Summary table."),
                   fluidRow(column(6,
                                   br(),
                                   actionButton(inputId='ab1', label="WALIS - Database",
                                                width = "100%",
                                                icon = icon("database"), 
                                                onclick ="window.open('https://zenodo.org/communities/walis_database?page=1&size=20')"),
                   ),
                   column(6,
                          p(style = "font-size:10px;text-align:justify", 
                            " WALIS Zenodo repository. 
                This Repository contains WALIS data in different formats,
                as well as scripts to query the database.
                The content coincides with the data on the Special Issue
                in the",a("journal Earth System Science Data",href= "https://essd.copernicus.org/articles/special_issue1055.html") 
                          )       
                   ),
                   column(12,
                          p(style = "font-size:10px;text-align:justify",
                            "WALIS is the result of the work of several people, within different projects. We kindly ask you to follow three simple rules to properly acknowledge those who worked on it: ",
                            strong("1. Cite the original authors"),br(),
                            strong("2. Acknowledge the database contributor"),br(),
                            strong("3. Acknowledge the database structure creators"),br(),
                            a("More details and examples on how to cite", href="https://walis-help.readthedocs.io/en/latest/citation/")
                          ))
                   ),
                   right= TRUE,
                   width = "400px",
                   status = "primary",
                   icon = icon("download", "fa-1x"),
                   tooltip = tooltipOptions(title = "Download menu")
                 ))
        ),
        br(),
        fluidRow(column(12, DT::dataTableOutput("table",width="100%"))
        ))
    )
  )

# Define server function

server <- function(input, output) {
  area <-
    reactiveValues(coord = data.frame(
      lon = c(0,0, 0, 0,0),
      lat = c(0, 0, 0,0,0)
    ))
  
  marker_function <-
    function(marker_color, icon, icon_color, name) {
      html <-
        paste0(
          "<div style='position: relative; display: inline-block'; iconSize: 1px; class='awesome-marker-icon-",
          marker_color,
          " awesome-marker'><i class='glyphicon glyphicon-",
          icon,
          " icon-",
          icon_color,
          "'></i></div>",
          name
        )
      return(html)
    }
  
  from_html_to_type <- function(x) {
    sli <- "Sea Level Indicator"
    marine <- "Marine Limiting"
    terrestrial <- "Terrestrial Limiting"
    type <- ifelse(grepl(terrestrial, x),
                   terrestrial,
                   ifelse(grepl(marine, x), marine,
                          ifelse(grepl(sli, x), sli, NA)))
    return(type)
  }
  
  type_sel <-
    reactiveValues(val = c(
      "Marine Limiting",
      "Terrestrial Limiting",
      "Sea Level Indicator"
    ))
  
  type_selected <- observe({
    selected_groups <- input$mymap_groups
    selected_type <- lapply(selected_groups, from_html_to_type)
    selected_type <- selected_type[!is.na(selected_type)]
    type_sel$val <- selected_type
    return(selected_type)
  })
  
  rsl_summary <- read.csv(rsl_summary_file)
  df <-
    st_as_sf(rsl_summary,
             coords = c("Longitude", "Latitude"),
             crs = 4326)# %>% st_jitter(factor = 0.001)
  #Patch
  df[df$Timing.constraint == "Equal to ", "Timing.constraint"] = "Equal to"
  df<-df[is.na(df$Elevation..m.) == FALSE,]
  
  groups = unique(df$Type.of.datapoint)
  df$Type.of.datapoint <- factor(df$Type.of.datapoint)
  
  df$marker_color <- ifelse(
    df$Type.of.datapoint == "Marine Limiting",
    "cadetblue",
    ifelse(
      df$Type.of.datapoint == "Terrestrial Limiting",
      "lightred",
      "darkgreen"
    )
  )
  
  df$icon <-
    ifelse(
      df$Type.of.datapoint == "Marine Limiting",
      "arrow-up",
      ifelse(
        df$Type.of.datapoint == "Terrestrial Limiting",
        "arrow-down",
        "plus-sign"
      )
    )
  
  df$marker <-
    ifelse(
      df$Type.of.datapoint == "Marine Limiting",
      marker_function("cadetblue", "arrow-up", "black", "Marine Limiting"),
      ifelse(
        df$Type.of.datapoint == "Terrestrial Limiting",
        marker_function("lightred", "arrow-down", "black", "Terrestrial Limiting"),
        marker_function("darkgreen", "plus-sign", "black", "Sea Level Indicator")
      )
    )
  
  #####ojo
  
  perc_low = c("1" = 84.1, "2" = 97.7, "3" = 99.5)
  perc_up = c("1" = 15.9, "2" = 2.3, "3" = 0.1)
  
  perc_range_age <-
    reactiveValues(low_age = "Age..ka..84.1.perc",upp_age = "Age..ka..15.9.perc")
  
  observeEvent(input$perc_age,ignoreInit = TRUE,
               {perc_range_age$upp_age <-
                 paste0("Age..ka..", perc_up[toString(input$perc_age)], ".perc", sep = "")
               
               perc_range_age$low_age <-
                 paste0("Age..ka..", perc_low[toString(input$perc_age)], ".perc", sep = "")
               }
               )
  
  perc_range_rsl <-
    reactiveValues(low_rsl = "RSL..m..84.1.perc",upp_rsl = "RSL..m..15.9.perc")
  
  observeEvent(input$perc_elev,ignoreInit = TRUE,
               {
                 perc_range_rsl$upp_rsl <-
                 paste0("RSL..m..", perc_up[toString(input$perc_elev)], ".perc", sep = "")
               
                 perc_range_rsl$low_rsl <-
                 paste0("RSL..m..", perc_low[toString(input$perc_elev)], ".perc", sep = "")
               
               }
  )
  
  
  data <- reactive({
    inp_temp <- input$temp
    inp_dating_tech <- input$dating_tech
    inp_elevation <- c()
    inp_type_rsl <- input$type_indicators
    inp_elev_uncert <- c()
    
    inp_elevation[1] <- as.numeric(input$elev_error[1])
    inp_elev_uncert[1] <- as.numeric(input$elev_uncert[1])
    
    if (input$elev_error[2] == "More than 20") {
      inp_elevation[2] <- 1000000
    } else{
      inp_elevation[2] <- as.numeric(input$elev_error[2])
    }
    
    if (input$elev_uncert[2] == "More than 20") {
      inp_elev_uncert[2] <- 1000000
    } else{
      inp_elev_uncert[2] <- as.numeric(input$elev_uncert[2])
    }
    
    upp_age <- perc_range_age$upp_age
    low_age <- perc_range_age$low_age

    upp_rsl <-perc_range_rsl$upp_rsl
    low_rsl <-perc_range_rsl$low_rsl

    
    df_sub <-
      subset(
        df,
        subset = Dating.technique %in% inp_dating_tech &
          RSL.Indicator %in% inp_type_rsl &
          eval(parse(text = low_age)) < inp_temp[1] &
          eval(parse(text = upp_age)) > inp_temp[2] &
          Elevation.error..m. >= inp_elevation[1] &
          Elevation.error..m. <= inp_elevation[2]
      )

    df_sub$Perc_Paleo_RSL_uncertainty <- abs(df_sub[[low_rsl]] - df_sub[[upp_rsl]])
    
    df_sub_sli <- subset(
      df_sub,
      subset =  is.na(Perc_Paleo_RSL_uncertainty) == FALSE &
        (
          Perc_Paleo_RSL_uncertainty <= inp_elev_uncert[1] |
            Perc_Paleo_RSL_uncertainty >= inp_elev_uncert[2]
        )
    )
    df_sub_final <-
      df_sub[setdiff(rownames(df_sub), rownames(df_sub_sli)), ]
    
    print(paste("RSL (after filters)", nrow(df_sub_final)))
    
    return(df_sub_final)
  })
  
  output$panel <- renderUI({
    fluidPage(fluidRow(
      column(
        12,
        dashboardLabel(
          paste("Age range: ", input$temp[1], "-", input$temp[2], " (ka)"),
          status = "info",
          style = "square"
        ),
        dashboardLabel(
          paste("Percent. range:", perc_up[input$perc_age], "-", perc_low[input$perc_age]),
          status = "info",
          style = "square"
        ),
        dashboardLabel(
          paste(
            "Dating Techniques:",
            paste(input$dating_tech, collapse = "-")
          ),
          status = "info",
          style = "square"
        )
      )
    ),
    fluidRow(
      column(
        12,
        dashboardLabel(
          paste(
            "Elevation error: ",
            input$elev_error[1],
            "-",
            input$elev_error[2],
            "(m)"
          ),
          status = "warning",
          style = "square"
        ),
        dashboardLabel(
          paste("Percent. range:", perc_up[input$perc_age], "-", perc_low[input$perc_age]),
          status = "warning",
          style = "square"
        )
        ,
        dashboardLabel(
          paste(
            "Uncertainty:",
            input$elev_uncert[1],
            "-",
            input$elev_uncert[2],
            " (m)"
          ),
          status = "warning",
          style = "square"
        )
        ,
        dashboardLabel(
          paste(
            "RSL Indicator types (",
            length(input$type_indicators),"/",length(rsl_indicator),")")
          ,
          status = "warning",
          style = "square"
        )
        ,
        dashboardLabel(
          paste("Extent: ", input$geo),
          status = "success",
          style = "square"
        )
      )
    ))
  })
  
  output$panel_2 <- renderUI({
    fluidPage(fluidRow(
      column(
        12,
         dashboardLabel(
           icon("stopwatch"),
           status = "info",
           style = "square"
         ),

        dashboardLabel(
          paste("Age range: ", input$temp[1], "-", input$temp[2], " (ka)"),
          status = "info",
          style = "square"
        ),
        dashboardLabel(
          paste("Percent. range:", perc_up[input$perc_age], "-", perc_low[input$perc_age]),
          status = "info",
          style = "square"
        ),
        dashboardLabel(
          paste(
            "Dating Techniques:",
            paste(input$dating_tech, collapse = "-")
          ),
          status = "info",
          style = "square"
        )
      )
    ),
    fluidRow(
      column(
        12,
         dashboardLabel(
           icon("filter","glyphicon"),
           status = "warning",
           style = "square"
         )
        ,
        dashboardLabel(
          paste(
            "Elevation error: ",
            input$elev_error[1],
            "-",
            input$elev_error[2],
            "(m)"
          ),
          status = "warning",
          style = "square"
        ),
        dashboardLabel(
          paste("Percent. range:", perc_up[input$perc_age], "-", perc_low[input$perc_age]),
          status = "warning",
          style = "square"
        )
        ,
        dashboardLabel(
          paste(
            "Uncertainty:",
            input$elev_uncert[1],
            "-",
            input$elev_uncert[2],
            " (m)"
          ),
          status = "warning",
          style = "square"
        )
        ,
        dashboardLabel(
          paste(
            "RSL Indicator types (",
            length(input$type_indicators),"/",length(rsl_indicator),")")
          ,
          status = "warning",
          style = "square"
        ),
        dashboardLabel(
          paste(
            "\U25c8 Indicator types: ",
            paste(type_sel$val, collapse = "-")
          ),
          status = "danger" ,
          style = "square"
        )
      )
    ))
  })
  
  output$table <- DT::renderDataTable({
    data <- data_in_area$data %>% dplyr::mutate(longitude = sf::st_coordinates(.)[,1],
                                                latitude = sf::st_coordinates(.)[,2]) %>% st_drop_geometry()
    drops<-c("geometry","marker_color","In_area","icon","marker","Perc_Paleo_RSL_uncertainty")
    data[ , !(names(data) %in% drops)]
  }, server= TRUE,
  options = list(dom = 'lfrtip',
                 pageLength = 25
  ))
  
  output$mymap <- renderLeaflet({
    leaflet() %>% addTiles(
      options = providerTileOptions(
        updateWhenZooming = FALSE,
        updateWhenIdle = FALSE,
        minZoom = 3,
        maxZoom = 12
      )
    ) %>% setView(7.595791257753539,51.96953395614229, zoom = 3) %>% setMaxBounds(
      lng1 = -200,
      lat1 = -90,
      lng2 = 200,
      lat2 = 90
    ) %>%
      addEasyButtonBar(position = "topright", 
                       easyButton(
                         icon = icon("globe-africa","fa-2x"), title = "Center in Africa",
                         onClick = JS("function(btn, map){map.setView(new L.LatLng(-1, 12),4);}")),
                       easyButton(
                         icon = icon("globe-asia","fa-2x"), title = "Center in Asia",
                         onClick = JS("function(btn, map){map.setView(new L.LatLng(19.7, 105.9),4);}")),
                       easyButton(
                         icon = icon("globe-europe","fa-2x"), title = "Center in Europe",
                         onClick = JS("function(btn, map){map.setView(new L.LatLng(46.6, 13.3),4);}")),
                       easyButton(
                         icon = icon("globe-americas","fa-2x"), title = "Center in The Americas",
                         onClick = JS("function(btn, map){map.setView(new L.LatLng(8.4, -79.5),4);}"))) %>%
      addLayersControl(overlayGroups = unique(df$marker),position = "topright",options = layersControlOptions(collapsed = FALSE)) %>%
      htmlwidgets::onRender(
                             "
                         function() {
              $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center;color:red\"> \U25c8 Indicator type </label>');
          }
      "
                           )
  })
  
  output$minimap <- renderLeaflet({
    df_area<-area$coord
    matrix_area <- data.matrix(df_area)
    polygon <- st_polygon(list(matrix_area)) %>% st_sfc(crs = 4326)
    l <- leaflet(polygon) %>% addTiles()%>% addPolygons(color = "green",popup ="Selected area")})
  
  observe({
    if (nrow(data()) == 0) {
      leafletProxy("mymap") %>%
        clearMarkers() %>% clearMarkerClusters()
    }
    else {
      rsl.df <- split(data(), data()$marker)
      
      l <- leafletProxy("mymap", data = data()) %>%
        clearMarkers() %>% clearMarkerClusters()
      
      names(rsl.df)  %>%
        purrr::walk(function(df) {
          l <<- l %>% addAwesomeMarkers(
            data = rsl.df[[df]],
            label =  ~ WALIS_ID,
            group = df,
            popup = ~ paste0(
              "<b>WALIS ID: </b>",
              WALIS_ID,
              "<br/><b>Type of point:</b> ",
              Type.of.datapoint,
              "<br/><b>Dating method:</b> ",
              Dating.technique,
              "<br/><b>Age ",
              Timing.constraint,
              " (50th percentile):</b> ",
              Age..ka..50.perc,
              " (ka)",
              "<br/><b>Elevation error: </b> ",
              Elevation.error..m.," (m)",
              "<br/><b> Reference: </b>",
              Reference.s.,
              "<br/><b> RSL digitized by: </b>",
              Record.Created.by
            ),
            icon = awesomeIcons(
              library = "glyphicon",
              icon = ~ icon,
              iconColor = 'black',
              markerColor = ~ marker_color
            ),
            clusterOptions = markerClusterOptions(disableClusteringAtZoom =
                                                    8)
          )
        })
      
      l %>%
        {
          if (input$geo == "polygon")
            addDrawToolbar(
              .,
              polylineOptions = FALSE,
              circleOptions = FALSE,
              markerOptions = FALSE,
              circleMarkerOptions = FALSE,
              position = "topleft",
              editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
              singleFeature = TRUE
            )
          else
            .
        }
    }
  })
  
  observeEvent(input$mymap_draw_new_feature, {
    
    feature <- input$mymap_draw_new_feature 
    coor <- unlist(feature$geometry$coordinates)
    lon <- coor[seq(1, length(coor), 2)]
    lat <- coor[seq(2, length(coor), 2)]
    if (input$geo == "polygon" & !is.null(coor)) {
      df_area <- rbind(data.frame(longitude = lon, latitude = lat))
      area$coord <- df_area
    }
    else{
      area$coord <- area$coord
    }
  })
  
  hover <- reactive({
    if (is.null(input$mymap_bounds))
      list(mymap_bounds=c(west=0,east=0,north=0,south=0))
    else
      input$mymap_bounds
  })
  hover <- hover %>% debounce(500)
  
  observeEvent(c(hover(), input$geo), ignoreInit = TRUE, {
    if (input$geo == "map") {
      feature <- unlist(hover())
      lon <-
        c(feature[["west"]], feature[["west"]], feature[["east"]], feature[["east"]], feature[["west"]])
      lat <-
        c(feature[["south"]], feature[["north"]], feature[["north"]], feature[["south"]], feature[["south"]])
      df_area <- rbind(data.frame(longitude = lon, latitude = lat))
      area$coord <- df_area
    }
  })
  
  data_in_area <- reactiveValues(data = data.frame())
  
  in_area_data <-
    observeEvent(c(
      data(),
      area$coord,
      type_sel$val
    ),
    {
      data <- data()
      df_area <- area$coord
      matrix_area <- data.matrix(df_area)
      polygon <-
        st_polygon(list(matrix_area)) %>% st_sfc(crs = 4326)
      data$In_area <- st_intersects(data, polygon, sparse = FALSE)
      if (nrow(data) != 0) {
        sub_data <-
          subset(data, subset = Type.of.datapoint %in% type_sel$val &
                   In_area == TRUE)
      }
      else{
        sub_data <- data
      }
      data_in_area$data <- sub_data
      print(paste0("RSL in area:",nrow(data_in_area$data)))
      return("Change")
    })
  
  output$mygraph <- renderGirafe({
    girafe(
      ggobj = plot_sea_level(
        data_in_area$data,
        type_sel$val
      ),
      options = list(opts_zoom(max = 5), opts_tooltip(use_fill = TRUE))
    )
  })
  
  output$downloadDataTable <- downloadHandler(
    filename = function() {
      paste("walis_summary.csv")
    },
    content = function(filename) {
      
      metadata<-data.frame(metadata= c("# Publisher: WALIS - https://warmcoasts.eu/",paste("#Age Range (ka):", input$temp),"#Percetiles range:","#Dating tecniques:","#Elevation error:","#Percentiles range","#Uncertainty(m)","#RSL indicator type:","#Extent:"))
      
      data <- data_in_area$data %>% dplyr::mutate(longitude = sf::st_coordinates(.)[,1],
                                                  latitude = sf::st_coordinates(.)[,2]) %>% st_drop_geometry()
      drops<-c("geometry","marker_color","In_area","icon","marker","Perc_Paleo_RSL_uncertainty")
      data <- data[ , !(names(data) %in% drops)]
      con <- file(filename,'wt')
      cat(paste0('#Publisher:WALIS-https://warmcoasts.eu/world-atlas.html','\n'), file = con)
      cat(paste0('#FILTERS','\n'), file = con)
      cat(paste0('#Age range (ka): ',input$temp[1],"-",input$temp[2],'\n'), file = con)
      cat(paste0('#Percentiles range [Age]: ',perc_range_age$low_age,"-",perc_range_age$upp_age,'\n'), file = con)
      cat(paste0('#Dating techniques: ',paste(unlist(input$dating_tech), collapse='/' ),'\n'), file = con)
      cat(paste0('#Elevation error: ',paste(unlist(input$elev_error), collapse='/' ),'\n'), file = con)
      cat(paste0('#Percentiles range  [Paleo RSL]: ',perc_range_rsl$low_rsl,"-",perc_range_rsl$upp_rsl,'\n'), file = con)
      cat(paste0('#Uncertainty (m) [Paleo RSL]: ',paste(unlist(input$elev_uncert),collapse='-'),'\n'), file = con)
      cat(paste0('#RSL indicator type: ',paste(unlist(input$type_indicators),collapse='/'),'\n'), file = con)
      cat(paste0('#Extent','\n'), file = con)
      write.table(data, file = con, row.names = FALSE,sep = ",")
    }
  )
  
  output$downloadDataTable_2 <- downloadHandler(
    filename = function() {
      paste("walis_summary.csv")
    },
    content = function(filename) {
      
      metadata<-data.frame(metadata= c("# Publisher: WALIS - https://warmcoasts.eu/",paste("#Age Range (ka):", input$temp),"#Percetiles range:","#Dating tecniques:","#Elevation error:","#Percentiles range","#Uncertainty(m)","#RSL indicator type:","#Extent:"))
      
      data <- data_in_area$data %>% dplyr::mutate(longitude = sf::st_coordinates(.)[,1],
                                                  latitude = sf::st_coordinates(.)[,2]) %>% st_drop_geometry()
      drops<-c("geometry","marker_color","In_area","icon","marker","Perc_Paleo_RSL_uncertainty")
      data <- data[ , !(names(data) %in% drops)]
      con <- file(filename,'wt')
      cat(paste0('#Publisher:WALIS-https://warmcoasts.eu/world-atlas.html','\n'), file = con)
      cat(paste0('#FILTERS','\n'), file = con)
      cat(paste0('#Age range (ka): ',input$temp[1],"-",input$temp[2],'\n'), file = con)
      cat(paste0('#Percentiles range [Age]: ',perc_range_age$low_age,"-",perc_range_age$upp_age,'\n'), file = con)
      cat(paste0('#Dating techniques: ',paste(unlist(input$dating_tech), collapse='/' ),'\n'), file = con)
      cat(paste0('#Elevation error: ',paste(unlist(input$elev_error), collapse='/' ),'\n'), file = con)
      cat(paste0('#Percentiles range  [Paleo RSL]: ',perc_range_rsl$low_rsl,"-",perc_range_rsl$upp_rsl,'\n'), file = con)
      cat(paste0('#Uncertainty (m) [Paleo RSL]: ',paste(unlist(input$elev_uncert),collapse='-'),'\n'), file = con)
      cat(paste0('#RSL indicator type: ',paste(unlist(input$type_indicators),collapse='/'),'\n'), file = con)
      cat(paste0('#Extent','\n'), file = con)
      write.table(data, file = con, row.names = FALSE,sep = ",")
    }
  )
  
  plot_sea_level <- function(data,
                             type_to_display = c("Marine Limiting",
                                                 "Terrestrial Limiting",
                                                 "Sea Level Indicator")) {
    sub_data <- data %>% st_drop_geometry()
    
    upp_age <- perc_range_age$upp_age
    age <- age <- paste0("Age..ka..", 50, ".perc", sep = "")
    low_age <- perc_range_age$low_age
    
    upp_rsl <-perc_range_rsl$upp_rsl
    rsl <- paste0("RSL..m..", 50, ".perc", sep = "")
    low_rsl <-perc_range_rsl$low_rsl

    if (nrow(sub_data) == 0) {
      text = paste("No data for this selection\n")
      p <- ggplot() +
        annotate(
          "text",
          x = 0,
          y = 0,
          size = 5,
          label = text
        ) +
        theme_void() +
        theme(
          aspect.ratio = 1,
          panel.border = element_rect(
            colour = "black",
            fill = NA,
            size = 5
          )
        )
      return(p)
    }
    
    else{
      min_rsl <- c()
      max_rsl <- c()
      
      limiting <-
        subset(sub_data, subset = Type.of.datapoint != "Sea Level Indicator")
      
      limiting$age_indicator <-
        ifelse(
          limiting["Timing.constraint"] == "Equal to",
          limiting[[age]],
          ifelse(limiting["Timing.constraint"] == "Younger than", limiting[[low_age]], limiting[[upp_age]])
        )
      
      terr_hor <-
        subset(limiting, subset = Type.of.datapoint == "Terrestrial Limiting")
      
      if (nrow(terr_hor) > 0) {
        min_rsl <-
          c(min_rsl, min(terr_hor["Elevation..m."] + terr_hor["Elevation.error..m."]))
        max_rsl <-
          c(max_rsl, max(terr_hor["Elevation..m."] + terr_hor["Elevation.error..m."]))
      }
      
      mar_hor <-
        subset(limiting, subset = Type.of.datapoint == "Marine Limiting")
      if (nrow(mar_hor) > 0) {
        min_rsl <-
          c(min_rsl, min(mar_hor["Elevation..m."] - mar_hor["Elevation.error..m."]))
        max_rsl <-
          c(max_rsl, max(mar_hor["Elevation..m."] - mar_hor["Elevation.error..m."]))
      }
      
      sea_level <-
        subset(sub_data, subset = Type.of.datapoint == "Sea Level Indicator")
      
      sea_level$start <-
        ifelse(
          sea_level["Timing.constraint"] == "Younger than",
          sea_level[[upp_age]],
          ifelse(sea_level["Timing.constraint"] == "Older than", sea_level[[low_age]], NA)
        )
      sea_level$end <-
        ifelse(
          sea_level["Timing.constraint"] == "Younger than",
          sea_level[[low_age]],
          ifelse(sea_level["Timing.constraint"] == "Older than", sea_level[[upp_age]], NA)
        )
      
      sea_level_younger <-
        subset(sea_level, subset = Timing.constraint == "Younger than")
      print(paste0(nrow(sea_level_younger), " <-sea_level_younger"))
      sea_level_older <-
        subset(sea_level, subset = Timing.constraint == "Older than")
      
      if (nrow(sea_level) > 0) {
        min_rsl <-
          c(min_rsl, min(sea_level[upp_rsl], sea_level[rsl], sea_level[low_rsl]))
        max_rsl <-
          c(max_rsl, max(sea_level[upp_rsl], sea_level[rsl], sea_level[low_rsl]))
        
      }
      
      rsl_global_min <- min(min_rsl)
      rsl_global_max <- max(max_rsl)
      print(paste0("min_rsl-> ",min_rsl))
      print(paste0("max_rsl-> ",max_rsl))
      
      arrow_factor <- (rsl_global_max - rsl_global_min) * 0.075
      print(paste0("arrow_factor-> ",arrow_factor))
      
      if (arrow_factor == 0) {
        arrow_factor <- 5
      }
      
      sl_colors <-
        c(
          "Equal to" = "cyan",
          "Older than" = "orange",
          "Younger than" = "purple"
        )
      lim_colors <-
        c(
          "Terrestrial Limiting" = "red",
          "Marine Limiting" = "blue",
          "Sea Level Indicator" = "white"
        )
      
      p <- ggplot() +
        # (Sea level index point)
        geom_rect_interactive(
          data = sea_level,
          aes(
            xmin = eval(parse(text = low_age)),
            xmax = eval(parse(text = upp_age)),
            ymin = eval(parse(text = low_rsl)),
            ymax = eval(parse(text = upp_rsl)),
            tooltip = WALIS_ID,
            fill = Timing.constraint,
            color = Timing.constraint,
          ),
          alpha = 0.2
        ) +
        geom_errorbar(
          data = sea_level,
          aes(
            x = eval(parse(text = age)),
            ymin = eval(parse(text = low_rsl)),
            ymax = eval(parse(text = upp_rsl)),
            color = Timing.constraint,
          ),
          alpha = 0.5,
          width = 0,
        ) +
        geom_errorbar(
          data = sea_level,
          aes(
            y = eval(parse(text = rsl)) ,
            xmin = eval(parse(text = low_age)) ,
            xmax = eval(parse(text = upp_age)),
            color = Timing.constraint
          ),
          alpha = 0.5,
          width = 0,
          
        ) +
        geom_segment_interactive(
          data = sea_level_younger,
          aes(
            y = eval(parse(text = rsl)),
            x = start,
            xend = end,
            yend = eval(parse(text = rsl)),
            tooltip = WALIS_ID,
            group = Timing.constraint,
            color = Timing.constraint
          ),
          arrow = arrow(length = unit(0.1, "inches"), ends = "last")
        ) +
        geom_segment_interactive(
          data = sea_level_older,
          aes(
            y = eval(parse(text = rsl)),
            x = end,
            xend = start,
            yend = eval(parse(text = rsl)),
            tooltip = WALIS_ID,
            group = Timing.constraint,
            color = Timing.constraint
          ),
          arrow = arrow(length = unit(0.1, "inches"), ends = "first")
        ) +
        
        scale_color_manual(values = sl_colors) +
        scale_fill_manual(values = sl_colors) +
        
        #Terrestrial limiting
        new_scale_color() +
        geom_segment_interactive(
          data = terr_hor,
          aes(
            x = eval(parse(text = upp_age)),
            y = Elevation..m. + Elevation.error..m.,
            xend = eval(parse(text = low_age)),
            yend = Elevation..m. + Elevation.error..m.,
            tooltip = WALIS_ID,
            color = Type.of.datapoint
          )
        ) +
        geom_segment_interactive(
          data = terr_hor,
          aes(
            x = age_indicator,
            y = Elevation..m. + Elevation.error..m.,
            xend = age_indicator,
            tooltip = WALIS_ID,
            yend = (Elevation..m. + Elevation.error..m. - arrow_factor),
            color = Type.of.datapoint
          ),
          arrow = arrow(length = unit(0.1, "inches"))
        ) +
        
        #Marine limiting
        geom_segment_interactive(
          data = mar_hor,
          aes(
            x = eval(parse(text = upp_age)),
            y = Elevation..m. - Elevation.error..m.,
            xend = eval(parse(text = low_age)),
            yend = Elevation..m. - Elevation.error..m.,
            tooltip = WALIS_ID,
            color = Type.of.datapoint
          )
        ) +
        geom_segment_interactive(
          data = mar_hor,
          aes(
            x = age_indicator,
            y = Elevation..m. - Elevation.error..m.,
            xend = age_indicator,
            yend = (Elevation..m. - Elevation.error..m. + arrow_factor),
            tooltip = WALIS_ID,
            color = Type.of.datapoint
          ),
          arrow = arrow(length = unit(0.1, "inches"))
        ) +
        
        scale_color_manual(values = lim_colors) +
        scale_x_reverse() +
        theme(legend.position = "none", aspect.ratio = 1) +
        xlab("Age (ka)") +
        ylab("RSL (m)")
      p
    }
  }
}

# Run the app ----
shinyApp(ui = ui, server = server)
