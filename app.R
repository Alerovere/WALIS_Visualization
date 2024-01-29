# Load packages

## Shiny app packages

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(shinyWidgets)
library("shinycssloaders")
library(leaflet)
library(leaflet.extras)
library(shinyjs)
library(readr)
library(data.table)
library(DT)
library(dplyr)
library(sf)
library(ggplot2)
library(gtable)
library(ggnewscale)
library(ggiraph)
library(pbapply)
library(pracma)
library(hexbin)

# Import required functions

source('r/extract_age.R')
source('r/extract_rsl.R')
source('r/join_age_rsl.R')
source('r/define_peaks_ranges.R')

# Set environment for Zip files

Sys.setenv(R_ZIPCMD = "/usr/bin/zip")

# SF configuration
sf::sf_use_s2(FALSE)
root_path <- getwd()

# Load data

latest_walis <-
  "http://www.warmcoasts.eu/sealevel/RSLmap/analysis_summary.csv"
rsl_summary_file <- "walis.csv"
sea_level_stack_pratt <- "Data/sea_level_stack_spratt.csv"

## Relevant for local implementations, only download if analysis_summary do not exist

if (!file.exists(rsl_summary_file)) {
  download.file(latest_walis, rsl_summary_file)
}

## Load Summary table

rsl_summary <- read.csv(rsl_summary_file)

rsl_indicator <- unique(rsl_summary$RSL.Indicator)

rsl_indicator <-
  rsl_indicator[order(nchar(rsl_indicator), rsl_indicator)]

## Load Stack Pratt
sl_stack <- read.csv(sea_level_stack_pratt, sep =)

## Define peak ranges
peak_ranges_spratt <-
  define_peaks_ranges(sl_stack,
                      age_name = 'Age.ka.',
                      mean_rsl = 'X50.',
                      width = 0.2)


############# WALIS EXPLORER APPLICATION #######################################

################################################################################
############################## UI  #############################################
################################################################################


# Define UI

ui <-
  navbarPage(
    ### Title
    title = div(img(src = "walis_logo.png", width = "30")),
    windowTitle = "Walis Explorer" ,
    id = "nav",
    theme = shinytheme("flatly"),
    
    ############################
    ##### INTERACTIVE TAB ######
    ############################
    
    tabPanel(
      "Interactive map",
      icon = icon("map"),
      tags$style(
        "@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"
      ),
      includeCSS("style.css"),
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          fluidRow(column(10, p(
            strong('The World Atlas of Last Interglacial Shorelines'), style = "font-size:22px;"
          )),
          column(
            1,
            
            ### Information dropdownButton
            
            dropdownButton(
              fluidRow(column(1, img(
                src = "walis_logo.png", width = "25"
              )),
              column(
                11,
                strong("The World Atlas of Last Interglacial Shorelines", style = "font-size:15px;")
              )),
              br(),
              p(
                "The World Atlas of Last Interglacial Shorelines (WALIS) is a sea-level database created by the",
                a("WARMCOASTS project.", href = "https://warmcoasts.eu/index.html"),
                br(),
                "WALIS aims at collecting existing and new data on Last Interglacial sea-level indicators reviewed following a standardized template.",
                "The WALIS Dashboard was created to explore WALIS and perform simple data analysis."
              ),
              br(),
              p("Keep exploring WALIS in the following resources:", style =
                  "font-size:14px;text-align:justify"),
              fluidRow(
                column(
                  6,
                  br(),
                  p(
                    a(icon("desktop", "fa-4x"), href = "https://warmcoasts.eu/world-atlas.html"),
                    style = "text-align:center"
                  ),
                  p(
                    "Visualize and download WALIS. You can also contribute with new data !",
                    style = "font-size:12px;text-align:justify"
                  )
                ),
                column(
                  6,
                  br(),
                  p(a(icon(
                    "code-branch", "fa-4x"
                  ), href = "https://github.com/sbastiangarzon"), style = "text-align:center"),
                  p(
                    "Fork our ",
                    strong(icon('github'), 'WALIS-Explorer '),
                    "repository to create new features to explore WALIS. You can also report issues and create pull requests.",
                    style = "font-size:12px;text-align:justify"
                  ),
                )
              ),
              p(
                strong("Funding:"),
                br(),
                ## Funding
                'This software is part of a project that has received funding from the European Research Council (ERC) under the European Union’s Horizon 2020 research and innovation programme (Grant agreement No.',
                a('ERC-StG-802414).', href = 'https://cordis.europa.eu/project/id/802414/it'),
                'Sebastian Garzon was also funded by the',
                a('Data Stewardship Scholarship', href = 'https://pastglobalchanges.org/science/wg/palsea/data'),
                'by',
                a('PAGES.', href = 'https://pastglobalchanges.org/'),
                style = "font-size:12px;text-align:justify"
              ),
              a(img(src = 'PAGES-white-highres.jpeg', width = '50%'), href =
                  'https://pastglobalchanges.org/science/wg/palsea/data'),
              fluidRow(column(
                10,
                br(),
                p(
                  strong("WALIS EXPLORER"),
                  "Designed by:",
                  a(icon("github"), "sbastiangarzon", href = "https://github.com/sbastiangarzon"),
                  "/",
                  a(icon("github"), "Alerovere", href =
                      "https://github.com/Alerovere"),
                  style = "font-size:10px;text-align:right"
                ),
                offset = 2
              ))
              ,
              status = "primary",
              size = "sm",
              icon = icon("info", "fa-1x"),
              width = "400px",
              tooltip = tooltipOptions(title = "Info")
            )
          )),
          
          ### Message 
          
          p(
            "Welcome to ",
            strong("WALIS Dashboard."),
            " In this page, you can customize your search and graphics by changing the parameters in the ",
            strong(icon("stopwatch"), "Age filter", style = "color: blue"),
            ",",
            strong(icon("filter", "glyphicon"), "RSL indicator filter", style = "color: orange"),
            ",",
            strong(icon("globe", lib = "glyphicon"), "Geographic Extent,", style = "color: green"),
            " and ",
            strong("\U25c8 Indicator type (map)", style = "color: red"),
            " menu.",
            style = "font-size:15px;text-align:justify"
          ),
          #https://github.com/dreamRs/shinyWidgets/blob/master/R/input-sliderText.R
          
          
          ### Filters drop down menu (Age / RSL / Geographic extent)
          
          fluidRow(
            column(
              1,
              offset = 3,
              
              # AGE FILTER MENU
              dropdownButton(
                p(strong("Age filter menu", style = "font-size:20px")),
                fluidRow(column(
                  8,
                  
                  # Age range 
                  
                  sliderTextInput(
                    inputId = "temp",
                    label = span(tagList(icon("history"), "Age Range")),
                    grid = TRUE,
                    choices = seq(from = 500, to = 0, by = -5),
                    selected = c(500, 0),
                    width = "100%",
                    post = " (ka)",
                  ),

                ),
                column(
                  4,
                  br(),
                  p(
                    style = "font-size:10px;text-align:justify",
                    "Select the Age Range of your query.
            Only RSL within this range are going to be displayed.",
                    strong("Tip: Use the arrow keys ◄ ► on your keyboard to fine-tune the values.")
                  )
                ))
                ,
                fluidRow(column(
                  7,
                  
                  # Percentiles range
                  
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
                  
                  # Dating techniques
                  
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
                ),
                
                # Reset button
                fluidRow(
                  column(12, offset = 3,
                actionBttn("resetFilters", "Reset filters", icon("refresh"),color="primary",size="md")
                )))
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
                
                #RSL indicator filter menu

                p(strong("RSL Indicator Filter", style = "font-size:20px")),
                strong("Elevation measurements"),
                br(),
                fluidRow(column(
                  8,
                  # Elevation error menu 
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
                    "Select a range of errors associated with the elevation measurement. This variable applies to all types of RSL indicators.",
                    strong("Tip: Use the arrow keys ◄ ► on your keyboard to fine-tune the values.")
                  )
                )),
                
                #Paleo-RSL menu 
                
                strong("Paleo-RSL"),
                br(),
                fluidRow(column(
                  6,
                  column(
                    6,
                    
                    #Percentiles range
                    
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
                      "Elevations of SLIPs are represented as probability distribution functions. Select the percentiles used for SLIP elevations."
                    )
                  )
                ),
                column(6,
                       fluidRow(
                         column(
                           7,
                           
                           # Uncertainty (m)
                           
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
                             "Select a range of Paleo RSL uncertainty (m) for SLIPs. This variable does not applies to Limiting (Marine or Terrestrial) index points.",
                             strong("Tip: Use the arrow keys ◄ ► on your keyboard to fine-tune the values.")
                           )
                         )
                       ))),
                fluidRow(column(
                  10,
                  
                  # RSL filter menu 
                  
                  strong("Select type of RSL indicator"),
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
                )
                
                ),
                fluidRow(
                  column(12, offset = 4,
                         actionBttn("resetFilters2", "Reset filters", icon("refresh"),color="warning",size="md"))
                )
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
              
              # Geographic extent menu
              
              dropdownButton(
                p(strong("Geographic extent", style = "font-size:20px")),
                fluidRow(column(
                  6,
                  
                  # Area of interest 
                  radioGroupButtons(
                    inputId = "geo",
                    label = "Area of interest:",
                    choices = c(
                      `<i class="far fa-square"></i> From map extent` = "map",
                      `<i class="fas fa-draw-polygon"></i> Draw polygon` = "polygon"
                    ),
                    selected = c("map"),
                    justified = TRUE,
                    status = "success",
                    direction = "vertical",
                  )
                )
                , column(
                  6,
                  br(),
                  p(
                    "Select the method to outline the area of interest.",
                    strong(icon("square"),"From map extent"),
                    "uses the current area displayed in the map",
                    strong(icon("draw-polygon"),"Draw polygon"),
                    "enables a drawing tool."
                  )
                ))
                ,
                circle = TRUE,
                status = "success",
                icon = icon("globe", lib = "glyphicon"),
                width = "400px",
                tooltip = tooltipOptions(title = "Geographic extent menu")
              )
            ),
          ),
          
          #### SEA LEVEL SECTION
          
          fluidRow(column(
            10, p(
              span(tagList(icon("water")), style = "color:#000101"),
              strong("Sea level"),
              style = "font-size:30px"
            )
          ),
          column(
            1,
            
            # Dropdown download menu 
            
            dropdownButton(
              strong("Download menu", style = "font-size:20px"),
              br(),
              strong("Summary Table"),
              p(
                style = "font-size:10px;text-align:justify",
                "The Summary Table is a reduced version of WALIS, containing only fundamental information on SLIPs and their age."
              )
              ,
              fluidRow(column(
                6, downloadButton("downloadDataTable", "Current selection")
              ),
              column(
                6,
                p(
                  style = "font-size:10px;text-align:justify",
                  "Download",
                  strong("summary table") ,
                  "of current RSL selection.
                         You can preview the content of this file in the",
                  icon("book"),
                  strong("Summary table tab.")
                )
              )),
              br(),
              strong("WALIS - Database"),
              p(
                style = "font-size:10px;text-align:justify",
                "The full WALIS database is available at the repository linked below. Besides the data, the repository includes scripts to query and analyze WALIS, including the scripts to create the Summary Table used in the Dashboard."
              ),
              fluidRow(column(
                6,
                br(),
                actionButton(
                  inputId = 'ab1',
                  label = "WALIS - Database",
                  width = "100%",
                  icon = icon("database"),
                  onclick = "window.open('https://zenodo.org/records/7348242')"
                ),
              ),
              column(
                6,
                p(
                  style = "font-size:10px;text-align:justify",
                  " WALIS Zenodo repository.
                This Repository contains WALIS data in different formats,
                as well as scripts to query the database.
                The content coincides with the data on the Special Issue
                in the",
                  a("journal Earth System Science Data", href = "https://essd.copernicus.org/articles/special_issue1055.html")
                )
              ),
              column(
                12,
                p(
                  a("More details and examples on how to cite", href =
                      "https://walis-help.readthedocs.io/en/stable/citation/")
                )
              )),
              width = "400px",
              size = "sm",
              status = "primary",
              icon = icon("download", "fa-1x"),
              tooltip = tooltipOptions(title = "Download menu")
            )
            
          ))
          ,
          
          # SEA LEVEL PLOT
          
          fluidRow(column(12,
                          withSpinner(
                            girafeOutput("sealevelPlot", width = "100%",
                                         height = "300px")
                          ))),
          
          # PLOT LEGEND
          
          fluidRow(
            column(
              4,
              # TERRESTRIAL LIMITING LEGEND  
              strong("Terrestrial Limiting", style = "text-align:center;font-size:10px"),
              fluidRow(
                column(
                  3,
                  dropdownButton(
                    p(strong("Terrestrial Limiting (Younger than...)")),
                    fluidRow(column(
                      3,
                      p(icon("level-down-alt", "fa-flip-horizontal"), style =
                          "font-size:70px;color: red;text-align:center")
                    ), column(9,
                              p(
                                p(strong("Arrow position:"), "Timing constraint"),
                                p(strong("Timing constraint:"), "Younger than"),
                                p(strong("Horizontal length:"), "Age range"),
                                p(strong("Vertical length: "), "No meaning")
                              )), style = "font-size:12px"),
                    
                    up = TRUE,
                    width = "275px",
                    tooltip = tooltipOptions(title = "Younger than..."),
                    size = "sm",
                    icon = p(icon("level-down-alt", "fa-flip-horizontal"), style =
                               "color: red")
                  )
                ),
                column(
                  3,
                  dropdownButton(
                    p(strong("Terrestrial Limiting (Equal to)")),
                    fluidRow(column(
                      3,
                      p("\u21A7", style = "color:red;font-size:80px;text-align:center")
                    ), column(9,
                              p(
                                p(strong("Arrow position:"), "Timing constraint"),
                                p(strong("Timing constraint:"), "Equal to"),
                                p(strong("Horizontal length:"), "Age range"),
                                p(strong("Vertical length: "), "No meaning")
                              )), style = "font-size:12px"),
                    width = "275px",
                    up = TRUE,
                    tooltip = tooltipOptions(title = "Equal to..."),
                    size = "sm",
                    icon = p("\u21A7", style = "color:red;font-size:30px")
                  )
                ),
                column(
                  3,
                  dropdownButton(
                    p(strong("Terrestrial Limiting (Older than)")),
                    fluidRow(column(
                      3,
                      p("\u21B4", style = "color: red;font-size:80px;text-align:center")
                    ),
                    column(9,
                           p(
                             p(strong("Arrow position:"), "Timing constraint"),
                             p(strong("Timing constraint:"), "Older than"),
                             p(strong("Horizontal length:"), "Age range"),
                             p(strong("Vertical length: "), "No meaning")
                           )), style = "font-size:12px"),
                    width = "275px",
                    up = TRUE,
                    tooltip = tooltipOptions(title = "Older than..."),
                    size = "sm",
                    icon = p("\u21B4", style = "color: red;font-size:30px")
                  )
                )
              )
            ),
            
            # MARINE LIMITING LEGEND  
            column(
              4,
              strong("Marine Limiting", style = "text-align:center;font-size:10px"),
              fluidRow(
                column(
                  3,
                  dropdownButton(
                    p(strong("Marine Limiting (Younger than)")),
                    fluidRow(column(
                      3,
                      p(icon("level-up-alt", "fa-flip-horizontal"), style =
                          "color: blue;font-size:70px;text-align:center")
                    ), column(9,
                              p(
                                p(strong("Arrow position:"), "Timing constraint"),
                                p(strong("Timing constraint:"), "Younger than"),
                                p(strong("Horizontal length:"), "Age range"),
                                p(strong("Vertical length: "), "No meaning")
                              )), style = "font-size:12px"),
                    width = "275px",
                    up = TRUE,
                    tooltip = tooltipOptions(title = "Younger than..."),
                    size = "sm",
                    icon = p(icon("level-up-alt", "fa-flip-horizontal"), style =
                               "color: blue")
                  )
                ),
                column(
                  3,
                  dropdownButton(
                    p(strong("Marine Limiting (Equal to)")),
                    fluidRow(column(
                      3,
                      p("\u21A5", style = "color:blue;font-size:80px")
                    ), column(9,
                              p(
                                p(strong("Arrow position:"), "Timing constraint"),
                                p(strong("Timing constraint:"), "Equal to"),
                                p(strong("Horizontal length:"), "Age range"),
                                p(strong("Vertical length: "), "No meaning")
                              )), style = "font-size:12px"),
                    width = "275px",
                    up = TRUE,
                    tooltip = tooltipOptions(title = "Equal to..."),
                    size = "sm",
                    icon = p("\u21A5", style = "color:blue;font-size:30px")
                  )
                ),
                column(
                  3,
                  dropdownButton(
                    p(strong("Marine Limiting (Older than)")),
                    fluidRow(column(
                      3,
                      p(icon("level-up-alt"), style =
                          "color: blue;font-size:70px")
                    ),
                    column(9, p(
                      p(strong("Arrow position:"), "Timing constraint"),
                      p(strong("Timing constraint:"), "Older than"),
                      p(strong("Horizontal length:"), "Age range"),
                      p(strong("Vertical length: "), "No meaning")
                    )), style = "font-size:12px"),
                    width = "275px",
                    up = TRUE,
                    tooltip = tooltipOptions(title = "Older than..."),
                    size = "sm",
                    icon = p(icon("level-up-alt"), style = "color: blue")
                  )
                )
              )
            ),
            
            # SEA LEVEL INDEX POINT LEGEND  
            column(
              4,
              strong("Sea-level index", style = "text-align:center;font-size:10px"),
              fluidRow(
                column(
                  3,
                  dropdownButton(
                    p(strong("Sea-level index (Younger than)")),
                    fluidRow(column(
                      3,
                      p("\u2348", style =
                          "color: orange;font-size:70px")
                    ),
                    column(9, p(
                      p(strong("Arrow direction:"), "Timing constraint"),
                      p(strong("Timing constraint:"), "Younger than"),
                      p(strong("Horizontal length:"), "Age range"),
                      p(strong("Vertical length: "), "Paleo-RSL elevation range")
                    )), style = "font-size:12px"),
                    width = "350px",
                    up = TRUE,
                    tooltip = tooltipOptions(title = "Younger than..."),
                    size = "sm",
                    icon = p("\u2348", style = "color: orange;font-size:30px")
                  )
                ),
                column(
                  3,
                  dropdownButton(
                    p(strong("Sea-level index (Equal to)")),
                    fluidRow(column(
                      3,
                      p("\u229e", style = "color:cyan;font-size:70px")
                    ),
                    column(9, p(
                      p(strong("Timing constraint:"), "Equal to"),
                      p(strong("Horizontal length:"), "Age range"),
                      p(strong("Vertical length: "), "Paleo-RSL elevation range")
                    )), style = "font-size:12px"),
                    width = "350px",
                    up = TRUE,
                    tooltip = tooltipOptions(title = "Equal to..."),
                    size = "sm",
                    icon = p("\u229e", style = "color:cyan;font-size:30px")
                  )
                ),
                column(
                  3,
                  dropdownButton(
                    p(strong("Sea-level index (Older than)")),
                    fluidRow(column(
                      3,
                      p("\u2347", style = "color: purple;font-size:70px")
                    ),
                    column(9, p(
                      p(strong("Arrow direction:"), "Timing constraint"),
                      p(strong("Timing constraint:"), "Older than"),
                      p(strong("Horizontal length:"), "Age range"),
                      p(strong("Vertical length: "), "Paleo-RSL elevation range")
                    )), style = "font-size:12px"),
                    width = "350px",
                    up = TRUE,
                    tooltip = tooltipOptions(title = "Older than..."),
                    size = "sm",
                    icon = p("\u2347", style = "color: purple;font-size:30px")
                  )
                )
              )
            )
          )
        ),
        
        mainPanel = mainPanel(
          fluidRow(column(12, uiOutput("inter_tab_infopanel"))),
          leafletOutput("inter_tab_map", height = 550),
          p(
            "Designed by:",
            a(icon("github"), "sbastiangarzon", href = "https://github.com/sbastiangarzon"),
            "/",
            a(icon("github"), "Alerovere", href = "https://github.com/Alerovere"),
            style = "font-size:10px;text-align:right"
          )
        )
      )
    )
    ,
    
    ##############################
    ##### SUMMARY TABLE TAB ######
    ##############################
    
    tabPanel(
      title = "Summary table",
      value = "Summary_table",
      icon = icon("book"),
      fluidPage(
        p(
          "In this page you can explore and download the",
          strong(icon("book"),"Summary table"),
          "of the current selection.",
          strong("All the filters applied to the data"), "are summarized below.",
          "You can modify these filters in the",
          strong(icon("map"), "Interactive map tab"),
          "Cells in the Summary table are blank when no data is present in WALIS for the corresponding property."
        )
        ,
        fluidRow(
          column(6,
                 # Filters summary information
                 strong("Filters"),
                 uiOutput("sum_tab_information")),
          column(
            4,
            strong(icon("globe", lib = "glyphicon"), "Geographic Extent"),
            leafletOutput("minimap", height = 120)
          ),
          column(
            2,
            strong("Download"),
            br(),
            
            
            #######
            
            
            dropdownButton(
              strong("Download menu", style = "font-size:20px"),
              br(),
              strong("Summary Table"),
              p(
                style = "font-size:10px;text-align:justify",
                "The Summary Table is a reduced version of WALIS, containing only fundamental information on SLIPs and their age."
              )
              ,
              fluidRow(column(
                6, downloadButton("downloadDataTable_2", "Current selection")
              ),
              column(
                6,
                p(
                  style = "font-size:10px;text-align:justify",
                  "Download",
                  strong("summary table") ,
                  "of current RSL selection.
                         You can preview the content of this file in the",
                  icon("book"),
                  strong("Summary table tab.")
                )
              )),
              br(),
              strong("WALIS - Database"),
              p(
                style = "font-size:10px;text-align:justify",
                "The full WALIS database is available at the repository linked below. Besides the data, the repository includes scripts to query and analyze WALIS, including the scripts to create the Summary Table used in the Dashboard."
              ),
              fluidRow(column(
                6,
                br(),
                actionButton(
                  inputId = 'ab1',
                  label = "WALIS - Database",
                  width = "100%",
                  icon = icon("database"),
                  onclick = "window.open('https://zenodo.org/records/7348242')"
                ),
              ),
              column(
                6,
                p(
                  style = "font-size:10px;text-align:justify",
                  " WALIS Zenodo repository.
                This Repository contains WALIS data in different formats,
                as well as scripts to query the database.
                The content coincides with the data on the Special Issue
                in the",
                  a("journal Earth System Science Data", href = "https://essd.copernicus.org/articles/special_issue1055.html")
                )
              ),
              column(
                12,
                p(
                  a("More details and examples on how to cite", href =
                      "https://walis-help.readthedocs.io/en/stable/citation/")
                )
              )),
              right = TRUE,
              width = "400px",
              size = "sm",
              status = "primary",
              icon = icon("download", "fa-1x"),
              tooltip = tooltipOptions(title = "Download menu")
            )
          )
        ),
        br(),
        fluidRow(column(
          12, DT::dataTableOutput("summary_tab_table", width = "100%")
        ))
      )
    ),
    
    ##############################
    ###### MERGE SLIP TAB #######
    ##############################
    
    tabPanel(
      title = "Merge SLIPs",
      value = "merge_slip",
      icon = icon("compress-arrows-alt"),
      dashboardPage(
        dashboardHeader(disable = TRUE),
        dashboardSidebar(
          disable = TRUE,
          collapsed = FALSE,
          width = '0'
        ),
        dashboardBody(
          sidebarLayout(sidebarPanel = sidebarPanel(
            box(
              title = "Sea level index point merging",
              solidHeader = TRUE,
              width = 12,
              status = 'navy',
              fluidRow(column(10,
                              p(
                                strong('Merging SLIPs'), style = "font-size:22px;"
                              )),
                       column(
                         2,
                         dropdownButton(
                           status = "primary",
                           size = "sm",
                           icon = icon("info", "fa-1x"),
                           width = "400px",
                           tooltip = tooltipOptions(title = "Info"),
                           fluidRow(column(1, img(
                             src = "walis_logo.png", width = "25"
                           )),
                           column(
                             11,
                             strong("The World Atlas of Last Interglacial Shorelines", style = "font-size:15px;")
                           )),
                           p(),
                           strong('SLIPs Merging'),
                           p(
                             'The merging process follows the methodology proposed by',
                             a("Garzon (2022)", href = "https://github.com/SbastianGarzon/sea_level_indicators_and_models/blob/master/Dissertation/docs/_main.pdf"),
                             '. The process consists of three main steps: \n'
                           ),
                           strong('1. Extract RSL and Age parameters \n'),
                           p(
                             'The RSL and Age parameters are extracted from each constraint of an individual SLIP. For example, the Age parameters of a SLIP from a radiometric dating are the normal distribution parameters',
                             strong('\U03BC'),
                             'and',
                             strong('\U03C3'),
                             '.'
                           ),
                           strong('2. Assign equal probability to each constraint \n'),
                           p(
                             'As a single SLIP can have more than one constraint (e.g multiple dating techniques or analysis), each is assigned an equal probability.'
                           ),
                           strong('3. Generate random values with the RSL and Age parameters'),
                           p(
                             'For an N number of times, a constraint is selected randomly. Then a random number is generated using the parameters (e.g., \U03BC and \U03C3 for normal distributions) from that constraint.'
                           ),
                           p(),
                           p(
                             'This process is repeated for all the SLIPs in the selection. The final result is a point cloud of Age and RSL values.'
                           )
                         )
                       ))
              ,
              
              fluidRow(
                column(
                  12,
                  p(
                    'In this tab, you can transform the sea-level index points (SLIPs) from your selection into a point cloud of Relative Sea-level (RSL) and Age values.
                 Only SLIPS with a timing constraint "Equal to"',
                    strong(icon("plus-square", style = "color:cyan")),
                    ' can be merged.'
                  ),
                  p(strong('Current selection'), style = "font-size:22px;"),
                  
                  fluidRow(column(
                    7,
                    p(
                      'The SLIPs in your selection result from the ',
                      strong(icon("globe", lib = "glyphicon"), "Geographic Extent", style = "color: green"),
                      "and aditional filters (",
                      strong(icon("stopwatch"), "Age", style = "color: blue"),
                      'and',
                      strong(icon("filter", "glyphicon"), "RSL indicator", style = "color: orange"),
                      ') from the',
                      strong(icon("map"), "Interactive map tab.")
                    )
                  ),
                  column(
                    5, leafletOutput("minimap_2", height = 100)
                  )),
                  p(),
                  fluidRow(column(10,
                                  p(
                                    strong('Filtering and merging options'), style = "font-size:22px;"
                                  )),
                           
                           column(
                             2,
                             dropdownButton(
                               status = "primary",
                               size = "sm",
                               icon = icon("info", "fa-1x"),
                               width = "800px",
                               tooltip = tooltipOptions(title = "Info"),
                               fluidRow(column(1, img(
                                 src = "walis_logo.png", width = "25"
                               )),
                               column(
                                 11,
                                 strong("The World Atlas of Last Interglacial Shorelines", style = "font-size:15px;")
                               )),
                               strong('⚈ Points per SLIP', style = "font-size:15px;"),
                               p(),
                               p(
                                 'This is the number of points per individual SLIPs to be sampled. For example, if the user has 3 SLIPs and defines 10.000 points per SLIP ⚈, the resulting point cloud will have 30.000 points.'
                               ),
                               p(
                                 'Walis-Explorer limits the number of points per SLIP depending on the number of SLIPs selected. If you want to do analysis beyond our limits, download a',
                                 strong(icon('docker')),
                                 'docker container from the Download menu after merging a selection of SLIPs.'
                               )
                               
                             )
                           ))
                  
                  ,
                  p(
                    'Define the merging parameters and filter the SLIPs by WALIS ID to generate the point cloud. Once you are ready, press',
                    strong(" \U25B8 Start merging")
                  ),
                  fluidRow(
                    column(6,
                           useShinyjs(),
                           uiOutput('merge_tab_poinsperslip')),
                    column(6,
                           uiOutput("merge_tab_slip_filter")),
                    fluidRow(
                           column(12, offset = 3,
                                  
                                    actionButton(
                                      inputId = "mergeButton",
                                      label = "\U25B8 Start merging",
                                      style = "color: #fff; background-color: #2c3e50;border-color: #FFFFFF"
                                    )
                                  )
                  )),
                  useSweetAlert()
                ),
                uiOutput('merge_tab_download')
              )
            )
          ),
          
          mainPanel = mainPanel(
            box(
              title = "Sea level plot",
              solidHeader = TRUE,
              width = 12,
              status = 'navy',
              fluidRow(
                uiOutput("merge_tab_info_panel"),
                column(10,
                       withSpinner(
                         girafeOutput("mergePlot", width = "100%", height = '600px')
                       )),
                
                column(
                  2,
                  fluidRow(valueBoxOutput('slip', width = 12)),
                  fluidRow(valueBoxOutput('slip_equal', width = 12)),
                  fluidRow(valueBoxOutput('terrestrial_limiting', width = 12)),
                  fluidRow(valueBoxOutput('marine_limiting', width = 12)),
                  fluidRow(valueBoxOutput('pointcloud', width = 12))
                )
              ),
              fluidRow(column(
                8,
                checkboxGroupButtons(
                  inputId = "elements_merge_indicators",
                  label = "Type of indicators to display:",
                  choices = c(
                    'Marine Limiting \u21A5' = "Marine Limiting",
                    'Terrestrial Limiting \u21A7' = "Terrestrial Limiting",
                    'Sea Level Indicator \u229e' = "Sea Level Indicator"
                  ),
                  selected = c("Marine Limiting", "Terrestrial Limiting", "Sea Level Indicator"),
                  checkIcon = list(
                    yes = icon("ok",
                               lib = "glyphicon"),
                    no = icon("remove",
                              lib = "glyphicon")
                  ),
                  status = "primary",
                  justified = TRUE
                )
              ),
              column(4,
                     disabled(
                       checkboxGroupButtons(
                         inputId = "elements_merge_pointcloud",
                         label = "Point cloud elements to display:",
                         choices = c(
                           `Point cloud <i class="far fa-dot-circle"></i>` = "pointcloud",
                           `Density <i class="fas fa-shapes"></i>` = "2ddensity"
                         ),
                         selected = c('pointcloud'),
                         checkIcon = list(
                           yes = icon("ok",
                                      lib = "glyphicon"),
                           no = icon("remove",
                                     lib = "glyphicon")
                         ),
                         status = "primary",
                         justified = TRUE
                       )
                     )))
            )
          ))
        )
      )
    )
  )


################################################################################
############################### SERVER FUNCTION   ##############################
################################################################################

server <- function(input, output, session) {
  
  area <-
    reactiveValues(coord = data.frame(lon = c(0, 0, 0, 0, 0),
                                      lat = c(0, 0, 0, 0, 0)))
  
  
  #### FUNCTIONS #####
  
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
    selected_groups <- input$inter_tab_map_groups
    selected_type <- lapply(selected_groups, from_html_to_type)
    selected_type <- selected_type[!is.na(selected_type)]
    type_sel$val <- selected_type
    return(selected_type)
  })
  
  rsl_summary <- read.csv(rsl_summary_file)
  df <-
    st_as_sf(rsl_summary,
             coords = c("Longitude", "Latitude"),
             crs = 4326)
  #Patch
  df[df$Timing.constraint == "Equal to ", "Timing.constraint"] = "Equal to"
  df <- df[is.na(df$Elevation..m.) == FALSE, ]
  
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
  
  perc_low = c("1" = 84.1, "2" = 97.7, "3" = 99.5)
  perc_up = c("1" = 15.9, "2" = 2.3, "3" = 0.1)
  
  perc_range_age <-
    reactiveValues(low_age = "Age..ka..84.1.perc", upp_age = "Age..ka..15.9.perc")
  
  observeEvent(input$perc_age, ignoreInit = TRUE,
               {
                 perc_range_age$upp_age <-
                   paste0("Age..ka..", perc_up[toString(input$perc_age)], ".perc", sep = "")
                 
                 perc_range_age$low_age <-
                   paste0("Age..ka..", perc_low[toString(input$perc_age)], ".perc", sep = "")
               })
  
  perc_range_rsl <-
    reactiveValues(low_rsl = "RSL..m..84.1.perc", upp_rsl = "RSL..m..15.9.perc")
  
  observeEvent(input$perc_elev, ignoreInit = TRUE,
               {
                 perc_range_rsl$upp_rsl <-
                   paste0("RSL..m..", perc_up[toString(input$perc_elev)], ".perc", sep = "")
                 
                 perc_range_rsl$low_rsl <-
                   paste0("RSL..m..", perc_low[toString(input$perc_elev)], ".perc", sep = "")
                 
               })
  
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
    
    upp_rsl <- perc_range_rsl$upp_rsl
    low_rsl <- perc_range_rsl$low_rsl
    
    
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
    
    df_sub$Perc_Paleo_RSL_uncertainty <-
      abs(df_sub[[low_rsl]] - df_sub[[upp_rsl]])
    
    df_sub_sli <- subset(
      df_sub,
      subset =  is.na(Perc_Paleo_RSL_uncertainty) == FALSE &
        (
          Perc_Paleo_RSL_uncertainty <= inp_elev_uncert[1] |
            Perc_Paleo_RSL_uncertainty >= inp_elev_uncert[2]
        )
    )
    df_sub_final <-
      df_sub[setdiff(rownames(df_sub), rownames(df_sub_sli)),]
    
    print(paste("RSL (after filters)", nrow(df_sub_final)))
    
    return(df_sub_final)
  }) %>% debounce(1000)
  
  ########### Render UI ########
  
  ##############################
  ### INTERACTIVE MAP TAB ######
  ##############################
  
  # Information Panel 
  
  output$inter_tab_infopanel <- renderUI({
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
            length(input$type_indicators),
            "/",
            length(rsl_indicator),
            ")"
          )
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
  
  # Interactive map
  
  output$inter_tab_map <- renderLeaflet({
    leaflet() %>% addTiles(
      options = providerTileOptions(
        updateWhenZooming = FALSE,
        updateWhenIdle = FALSE,
        minZoom = 1,
        maxZoom = 12
      )
    ) %>% setView(7.595791257753539, 51.96953395614229, zoom = 1) %>% setMaxBounds(
      lng1 = -200,
      lat1 = -90,
      lng2 = 200,
      lat2 = 90
    ) %>%
      addEasyButtonBar(
        position = "topright",
        easyButton(
          icon = icon("globe-africa", "fa-2x"),
          title = "Center in Africa",
          onClick = JS(
            "function(btn, map){map.setView(new L.LatLng(-1, 12),3);}"
          )
        ),
        easyButton(
          icon = icon("globe-asia", "fa-2x"),
          title = "Center in Asia",
          onClick = JS(
            "function(btn, map){map.setView(new L.LatLng(19.7, 105.9),3);}"
          )
        ),
        easyButton(
          icon = icon("globe-europe", "fa-2x"),
          title = "Center in Europe",
          onClick = JS(
            "function(btn, map){map.setView(new L.LatLng(46.6, 13.3),3);}"
          )
        ),
        easyButton(
          icon = icon("globe-americas", "fa-2x"),
          title = "Center in The Americas",
          onClick = JS(
            "function(btn, map){map.setView(new L.LatLng(8.4, -79.5),3);}"
          )
        ),
        easyButton(
          icon = icon("globe-oceania", "fa-2x"),
          title = "Center in The Americas",
          onClick = JS(
            "function(btn, map){map.setView(new L.LatLng(-13, 131),3);}"
          )
        )
        
      ) %>%
      addLayersControl(
        overlayGroups = unique(df$marker),
        position = "topright",
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      htmlwidgets::onRender(
        "
                         function() {
              $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center;color:red\"> \U25c8 Indicator type </label>');
          }
      "
      )
  })
  
  # Observe events - Interactive map
  
  
  observeEvent(input$resetFilters, {
    # Reset all filters to their initial values
    updateSliderTextInput(session, inputId = "temp", selected = c(500, 0))
    updateRadioGroupButtons(session, inputId = "perc_age", selected = c("1"))
    updateCheckboxGroupButtons(session, inputId = "dating_tech", selected = c(
      "U-Series",
      "AAR",
      "ESR",
      "Luminescence",
      "Stratigraphic constraint",
      "Other age constraint"
    ))
  })
  
  
  observeEvent(input$resetFilters2, {
    # Reset all filters for the RSL Indicator Filter menu
    updateSliderTextInput(session, inputId = "elev_error", selected = c(0, "More than 20"))
    updateRadioGroupButtons(session, inputId = "perc_elev", selected = c("1"))
    updateSliderTextInput(session, inputId = "elev_uncert", selected = c(0, "More than 20"))
    updateSelectInput(session, inputId = "type_indicators", selected = rsl_indicator)

  })
  
  
  
  
  observe({
    if (nrow(data()) == 0) {
      leafletProxy("inter_tab_map") %>%
        clearMarkers() %>% clearMarkerClusters()
    }
    else {
      rsl.df <- split(data(), data()$marker)
      
      l <- leafletProxy("inter_tab_map", data = data()) %>%
        clearMarkers() %>% clearMarkerClusters()
      
      names(rsl.df)  %>%
        purrr::walk(function(df) {
          l <<- l %>% addAwesomeMarkers(
            data = rsl.df[[df]],
            label =  ~ paste(WALIS_ID,"- Click for info"),
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
              Elevation.error..m.,
              " (m)",
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
  
  # Draw new feature
  
  observeEvent(input$inter_tab_map_draw_new_feature, {
    feature <- input$inter_tab_map_draw_new_feature
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
    if (is.null(input$inter_tab_map_bounds))
      list(inter_tab_map_bounds = c(
        west = 0,
        east = 0,
        north = 0,
        south = 0
      ))
    else
      input$inter_tab_map_bounds
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
  
  # Data in area
  
  data_in_area <- reactiveValues(data = data.frame())
  
  # Update data in area
  
  in_area_data <-
    observeEvent(c(data(),
                   area$coord,
                   type_sel$val),
                 {
                   data <- data()
                   df_area <- area$coord
                   matrix_area <- data.matrix(df_area)
                   polygon <-
                     st_polygon(list(matrix_area)) %>% st_sfc(crs = 4326)
                   data$In_area <-
                     st_intersects(data, polygon, sparse = FALSE)
                   if (nrow(data) != 0) {
                     sub_data <-
                       subset(data, subset = Type.of.datapoint %in% type_sel$val &
                                In_area == TRUE)
                   }
                   else{
                     sub_data <- data
                   }
                   data_in_area$data <- sub_data
                   print(paste0("RSL in area:", nrow(data_in_area$data)))
                   return("Change")
                 })

  # Sea level Plot
  
  output$sealevelPlot <- renderGirafe({
    girafe(
      ggobj = plot_sea_level(data_in_area$data,
                             type_sel$val),
      options = list(opts_zoom(max = 5))
    )
  })
  
  # Download Data table
  
  output$downloadDataTable <- downloadHandler(
    filename = function() {
      paste("walis_summary.csv")
    },
    content = function(filename) {
      metadata <-
        data.frame(
          metadata = c(
            "# Publisher: WALIS - https://warmcoasts.eu/",
            paste("#Age Range (ka):", input$temp),
            "#Percetiles range:",
            "#Dating tecniques:",
            "#Elevation error:",
            "#Percentiles range",
            "#Uncertainty(m)",
            "#RSL indicator type:",
            "#Extent:"
          )
        )
      
      matrix_area <- data.matrix(area$coord)
      polygon <- st_polygon(list(matrix_area)) %>% st_sfc(crs = 4326)
      
      data <-
        data_in_area$data %>% dplyr::mutate(
          longitude = sf::st_coordinates(.)[, 1],
          latitude = sf::st_coordinates(.)[, 2]
        ) %>% st_drop_geometry()
      drops <-
        c(
          "geometry",
          "marker_color",
          "In_area",
          "icon",
          "marker",
          "Perc_Paleo_RSL_uncertainty"
        )
      data <- data[,!(names(data) %in% drops)]
      
      # Write table
      
      matrix_area <- data.matrix(area$coord)
      polygon <- st_polygon(list(matrix_area)) %>% st_sfc(crs = 4326)
      
      fwrite(x = data,
             file = filename,sep=';')
      
      # Write metadata
      
      cat(
        paste0(
          '#Publisher:WALIS-https://warmcoasts.eu/world-atlas.html',
          '\n'
        ),
        file = filename,
        append = TRUE
      )
      cat(paste0('#FILTERS', '\n'), file = filename,
          append = TRUE)
      cat(paste0('#Age range (ka): ', input$temp[1], "-", input$temp[2], '\n'),
          file = filename,
          append = TRUE)
      cat(
        paste0(
          '#Percentiles range [Age]: ',
          perc_range_age$low_age,
          "-",
          perc_range_age$upp_age, '\n'
        ),
        file = filename,
        append = TRUE
      )
      cat(paste0('#Dating techniques: ', paste(
        unlist(input$dating_tech), collapse = '/'
      ), '\n'), file = filename,
      append = TRUE)
      cat(paste0('#Elevation error: ', paste(unlist(
        input$elev_error
      ), collapse = '/'), '\n'), file = filename,
      append = TRUE)
      cat(
        paste0(
          '#Percentiles range  [Paleo RSL]: ',
          perc_range_rsl$low_rsl,
          "-",
          perc_range_rsl$upp_rsl, '\n'
        ),
        file = filename,
        append = TRUE
      )
      cat(paste0(
        '#Uncertainty (m) [Paleo RSL]: ',
        paste(unlist(input$elev_uncert), collapse = '-'),"\n"), file = filename,
      append = TRUE)
      cat(paste0('#RSL indicator type: ', paste(
        gsub(",","",unlist(input$type_indicators)), collapse = '/'
      ),'\n'), file = filename,
      append = TRUE)
      cat(paste0('#Extent(Well-known text):'),st_as_text(polygon),file = filename,
          append = TRUE)

    }
  )

  ##############################
  #### SUMMARY TABLE TAB #######
  ##############################
  
  # Filters menu
  
  output$sum_tab_information <- renderUI({
    fluidPage(fluidRow(
      column(
        12,
        dashboardLabel(icon("stopwatch"),
                       status = "info",
                       style = "square"),
        
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
          icon("filter", "glyphicon"),
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
            length(input$type_indicators),
            "/",
            length(rsl_indicator),
            ")"
          )
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
  
  # Minimap
  
  output$minimap <- renderLeaflet({
    df_area <- area$coord
    matrix_area <- data.matrix(df_area)
    polygon <- st_polygon(list(matrix_area)) %>% st_sfc(crs = 4326)
    l <-
      leaflet(polygon) %>% addTiles() %>% addPolygons(color = "green", popup =
                                                        "Selected area")
  })
  
  # Table
  
  output$summary_tab_table <- DT::renderDataTable({
    data <-
      data_in_area$data %>% dplyr::mutate(
        longitude = sf::st_coordinates(.)[, 1],
        latitude = sf::st_coordinates(.)[, 2]
      ) %>% st_drop_geometry()
    drops <-
      c(
        "geometry",
        "marker_color",
        "In_area",
        "icon",
        "marker",
        "Perc_Paleo_RSL_uncertainty"
      )
    data[,!(names(data) %in% drops)]
  }, server = TRUE,
  extensions = c('FixedHeader', 'FixedColumns'),
  options = list(
    dom = 'lfrtip',
    pageLength = 25,
    scrollX = TRUE,
    fixedHeader = TRUE,
    fixedColumns = list(leftColumns = 2)
  ))
  
  # Download table
  
  output$downloadDataTable_2 <- downloadHandler(
    filename = function() {
      paste("walis_summary.csv")
    },
    content = function(filename) {
      metadata <-
        data.frame(
          metadata = c(
            "# Publisher: WALIS - https://warmcoasts.eu/",
            paste("#Age Range (ka):", input$temp),
            "#Percetiles range:",
            "#Dating tecniques:",
            "#Elevation error:",
            "#Percentiles range",
            "#Uncertainty(m)",
            "#RSL indicator type:",
            "#Extent:"
          )
        )
      
      matrix_area <- data.matrix(area$coord)
      polygon <- st_polygon(list(matrix_area)) %>% st_sfc(crs = 4326)
      
      data <-
        data_in_area$data %>% dplyr::mutate(
          longitude = sf::st_coordinates(.)[, 1],
          latitude = sf::st_coordinates(.)[, 2]
        ) %>% st_drop_geometry()
      drops <-
        c(
          "geometry",
          "marker_color",
          "In_area",
          "icon",
          "marker",
          "Perc_Paleo_RSL_uncertainty"
        )
      data <- data[,!(names(data) %in% drops)]
      
      # Write table
      
      fwrite(x = data,
             file = filename,sep=';')
      
      # Write metadata
      
      cat(
        paste0(
          '#Publisher:WALIS-https://warmcoasts.eu/world-atlas.html',
          '\n'
        ),
        file = filename,
        append = TRUE
      )
      cat(paste0('#FILTERS', '\n'), file = filename,
          append = TRUE)
      cat(paste0('#Age range (ka): ', input$temp[1], "-", input$temp[2], '\n'),
          file = filename,
          append = TRUE)
      cat(
        paste0(
          '#Percentiles range [Age]: ',
          perc_range_age$low_age,
          "-",
          perc_range_age$upp_age, '\n'
        ),
        file = filename,
        append = TRUE
      )
      cat(paste0('#Dating techniques: ', paste(
        unlist(input$dating_tech), collapse = '/'
      ), '\n'), file = filename,
      append = TRUE)
      cat(paste0('#Elevation error: ', paste(unlist(
        input$elev_error
      ), collapse = '/'), '\n'), file = filename,
      append = TRUE)
      cat(
        paste0(
          '#Percentiles range  [Paleo RSL]: ',
          perc_range_rsl$low_rsl,
          "-",
          perc_range_rsl$upp_rsl, '\n'
        ),
        file = filename,
        append = TRUE
      )
      cat(paste0(
        '#Uncertainty (m) [Paleo RSL]: ',
        paste(unlist(input$elev_uncert), collapse = '-'),"\n"), file = filename,
        append = TRUE)
      cat(paste0('#RSL indicator type: ', paste(
        gsub(",","",unlist(input$type_indicators)), collapse = '/'
      ),'\n'), file = filename,
      append = TRUE)
      cat(paste0('#Extent(Well-known text):'),st_as_text(polygon),file = filename,
          append = TRUE)
    }
  )
  
  ##############################
  ####### MERGE SLIP TAB ######
  ##############################
  
  
  # SLIP Filter
  
  
  mySamplingStrategy <- "Regular sampling"
  
  
  output$merge_tab_slip_filter <- renderUI({
    data_in_area_selection <- data_in_area$data %>% st_drop_geometry()
    pickerInput(
      inputId = "slip_selection",
      label = "SLIPs filter",
      choices = unique(data_in_area_selection[data_in_area_selection$Type.of.datapoint ==
                                                'Sea Level Indicator', 'WALIS_ID']),
      selected = unique(data_in_area_selection[data_in_area_selection$Type.of.datapoint ==
                                                 'Sea Level Indicator', 'WALIS_ID']),
      multiple = TRUE,
      options = list(`actions-box` = TRUE, style = "btn-primary")
    )
  })
  
  # Points per SLIP
  
  output$merge_tab_poinsperslip <- renderUI({
    num_slips <- nrow(unique(data_in_area_merging$data[data_in_area_merging$data$Type.of.datapoint ==
                                                         'Sea Level Indicator' &
                                                         data_in_area_merging$data$Timing.constraint == 'Equal to', 'WALIS_ID']))
    
    max_points <- 10000
    
    if (num_slips > 30) {
      max_points <- round(300000 / num_slips, -2)
    }
    
    sliderInput(
      "points_per_slip",
      "⚈ Points per SLIP",
      min = 100,
      max = max_points,
      step = 100,
      value = round((max_points + 100) / 2, -2)
    )
  })

  # Information Panel
  
  output$merge_tab_info_panel <- renderUI({
    fluidPage(fluidRow(
      column(
        12,
        dashboardLabel(icon("stopwatch"),
                       status = "info",
                       style = "square"),
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
          icon("filter", "glyphicon"),
          status = "warning",
          style = "square"
        ),
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
            length(input$type_indicators),
            "/",
            length(rsl_indicator),
            ")"
          )
          ,
          status = "warning",
          style = "square"
        )
      )
    ))
  })
  
  
  # Data in area (for merging)
  
  data_in_area_merging <- reactiveValues(data = data.frame(), num = -1)
  
  # Observe sea level index points in area
  
  slip_sel <-
    reactiveValues(val = c())
  
  # Update SLIP Selected
  
  slip_selected <- observe({
    selected_slip <- input$slip_selection
    slip_sel$val <- selected_slip
    return(selected_slip)
  })
  
  # Update SLIP in area for merging
  
  in_area_merging <- observeEvent(c(data_in_area,
                                    slip_sel$val),
                                  {
                                    ids_limiting <-
                                      data_in_area$data[data_in_area$data$Type.of.datapoint != 'Sea Level Indicator', 'WALIS_ID'] %>%
                                      st_drop_geometry()
                                    ids_slip <- slip_sel$val
                                    ids_selection <-
                                      c(unique(ids_limiting$WALIS_ID), ids_slip)
                                    data_in_area_merging$data <-
                                      data_in_area$data[data_in_area$data$WALIS_ID %in% ids_selection, ]
                                    
                                    
                                    data_in_area_merging$num <-
                                      nrow(unique(data_in_area$data[data_in_area$data$WALIS_ID %in% ids_selection &
                                                                      data_in_area$data$Age.calculation.from != 'Radiometric dating', 'WALIS_ID'] %>%
                                                    st_drop_geometry()))
                                    print(data_in_area_merging$num)
                                    disable('elements_merge_pointcloud')
                                    return("Change")
                                  })
  
  # Keep track of type of sampling
  
  sampling_peaks <- c()
  
  
  # Download menu
  
  output$merge_tab_download <- renderUI({
    num <- merging_point_cloud$data
    print(merging_point_cloud$data)
    
    if (nrow(num) < 1) {
      p()
    }
    else{
      fluidPage(
        p(strong('Download menu'), style = "font-size:22px;"),
        p(
          'You can directly download the point cloud in a file',
          strong(icon("file-download")),
          '
        or a docker container ',
          strong(icon("docker")),
          '. The docker container allows you to reproduce the result in your computer.'
        ),
        fluidRow(column(
          6,
          downloadButton(
            outputId = "download_pointcloud",
            icon = icon("file-download"),
            label = "Point cloud",
            style = "color: #fff; background-color: #2c3e50;border-color: #FFFFFF",
            color = "primary"
          )
        ),
        column(
          6,
          downloadButton(
            outputId = "download_docker",
            label = "Docker container",
            style = "material-flat",
            style = "color: #fff; background-color: #2c3e50;border-color: #FFFFFF",
            color = "primary",
            icon = icon("docker")
          )
        ))
      )
    }
  })
  
  # Mini map
  
  output$minimap_2 <- renderLeaflet({
    df_area <- area$coord
    matrix_area <- data.matrix(df_area)
    polygon <- st_polygon(list(matrix_area)) %>% st_sfc(crs = 4326)
    # Remove Leaflet attribution to improve the visualization of the miniature
    # Other maps display the attribution
    # Leaflet | © OpenStreetMap contributors, CC-BY-SA
    
    l <-
      leaflet(polygon,
              options = leafletOptions(zoomControl = FALSE, attributionControl =
                                         FALSE)) %>% addTiles() %>% addPolygons(color = "green", popup =
                                                                                  "Selected area")
  })
  
  #style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
  
  # Value Box
  
  output$slip <- renderValueBox(valueBox(
    value = nrow(unique(data_in_area_merging$data[data_in_area_merging$data$Type.of.datapoint ==
                                                    'Sea Level Indicator', 'WALIS_ID'])),
    subtitle = 'SLIPs (All)' ,
    icon = icon('plus-square'),
    color = 'olive'
  ))
  
  output$slip_equal <- renderValueBox(valueBox(
    value = nrow(unique(data_in_area_merging$data[data_in_area_merging$data$Type.of.datapoint ==
                                                    'Sea Level Indicator' &
                                                    data_in_area_merging$data$Timing.constraint == 'Equal to', 'WALIS_ID'])),
    subtitle = 'SLIPs (Equal to)' ,
    icon = icon('th-large')
  ))
  
  output$marine_limiting <- renderValueBox(valueBox(
    value = nrow(unique(data_in_area_merging$data[data_in_area_merging$data$Type.of.datapoint ==
                                                    'Marine Limiting', 'WALIS_ID'])),
    subtitle = 'Marine limiting' ,
    icon = icon('long-arrow-alt-up'),
    color = 'blue'
  ))
  
  output$terrestrial_limiting <- renderValueBox(valueBox(
    value = nrow(unique(data_in_area_merging$data[data_in_area_merging$data$Type.of.datapoint ==
                                                    'Terrestrial Limiting', 'WALIS_ID'])),
    subtitle = 'Terr. limiting' ,
    icon = icon('long-arrow-alt-down'),
    color = 'red'
  ))
  
  output$pointcloud <- renderValueBox({
    color <- 'orange'
    icon_value <- 'times-circle'
    message <- icon(icon_value)
    subtitle <- 'No point cloud'
    
    if (nrow(merging_point_cloud$data) > 0) {
      icon_value <- 'check-circle'
      message <- icon(icon_value)
      color <- 'olive'
      subtitle <- 'Point cloud available'
    }
    
    valueBox(value = message,
             subtitle = subtitle,
             color = color)
  })
  
  # Merging point cloud
  
  merging_point_cloud <- reactiveValues(data = data.frame())
  
  confirmation_merge <- observeEvent(input$mergeButton, {
    num_indicators <-
      nrow(unique(data_in_area_merging$data[data_in_area_merging$data$Type.of.datapoint ==
                                              'Sea Level Indicator' &
                                              data_in_area_merging$data$Timing.constraint == 'Equal to', 'WALIS_ID']))
    print(num_indicators)
    
    if (input$mergeButton != 0 & num_indicators == 0) {
      sendSweetAlert(title = 'Error: Merging sea level index',
                     text = ' Your selection does not include sea level index points with timing constraint "Equal to". ',
                     type = 'error')
    }
    
    if (input$mergeButton != 0 & num_indicators > 0) {
      confirmSweetAlert(
        inputId = 'confirmMerge',
        title = "Merging sea level index points",
        text = paste0(
          ' The merging process \U1F504 is going to include ',
          nrow(unique(data_in_area_merging$data[data_in_area_merging$data$Type.of.datapoint ==
                                                  'Sea Level Indicator' &
                                                  data_in_area_merging$data$Timing.constraint == 'Equal to', 'WALIS_ID'])),
          ' sea level index points with temporal constraint "Equal to" (out of ',
          nrow(unique(data_in_area_merging$data[data_in_area_merging$data$Type.of.datapoint ==
                                                  'Sea Level Indicator', 'WALIS_ID'])),
          ' SLIP from your selection). ',
          ' \U1F914 Are you sure you want to continue ? '
        ),
        size = 'xss',
        type = "question",
        btn_labels = c('Cancel', 'Confirm')
      )
    }
    return()
  })
  
  # Confirm merge
  
  output$confirmMerge <- renderPrint(input$confirmMerge)
  
  # Merning point cloud
  
  merge_point_cloud <- observeEvent(input$confirmMerge, {
    if (input$confirmMerge == TRUE) {
      # 1. Select data in area and input
      
      area_sli <- isolate(data_in_area_merging$data)
      n_sampling <- input$points_per_slip
      sl_peaks <- sampling_peaks$data
      
      area_sli <-
        area_sli[area_sli$Type.of.datapoint == 'Sea Level Indicator' &
                   area_sli$Timing.constraint == 'Equal to',]
      
      ## 2. Sample Sea level indicators
      
      if (nrow(area_sli) > 0) {
        # Age
        progressSweetAlert(
          id = "progress_age",
          title = "Merging age values \n from sea level index points (1/2)",
          display_pct = TRUE,
          size = 'sm',
          status = 'info',
          striped = TRUE,
          value = 0
        )
        
        #Initialize progress bar for age
        prog <- 0
        
        #Sleep time to read updateProgressBar message
        
        Sys.sleep(2)
        
        set.seed(200)
        
        age <- pblapply(unique(area_sli$WALIS_ID), function(x) {
          prog <<- prog + 1
          updateProgressBar(id = "progress_age",
                            value = prog / length(unique(area_sli$WALIS_ID)) * 100)
          extract_age(area_sli[area_sli$WALIS_ID == x,], n_samples = n_sampling, peaks = sl_peaks)
        })
        closeSweetAlert()
        
        # RSL
        
        # Create progress bar
        progressSweetAlert(
          id = "progress_rsl",
          title = "Merging relative sea level values \n from sea level index points (2/2)",
          display_pct = TRUE,
          status = 'info',
          size = 'xxs',
          striped = TRUE,
          value = 0
        )
        
        #Initialize progress bar for rsl
        prog_2 <- 0
        #Sleep time to read updateProgressBar message
        Sys.sleep(2)
        
        # Update progress bar with the rsl extraction process
        set.seed(100)
        rsl <- pblapply(unique(area_sli$WALIS_ID), function(x) {
          prog_2 <<- prog_2 + 1
          updateProgressBar(id = "progress_rsl",
                            value = prog_2 / length(unique(area_sli$WALIS_ID)) * 100)
          extract_rsl(area_sli[area_sli$WALIS_ID == x,], n_samples = n_sampling)
        })
        
        closeSweetAlert()
        
        # Join Age and RSL
        age_rsl_area <-
          lapply(1:length(age), function(x)
            join_age_rsl(age[[x]], rsl[[x]]))
        
        # 3. Extract features
        # Extract Sea level indicators
        
        sli_sample <- lapply(age_rsl_area, '[[', 'sli_sample')
        sli_sample <-
          sli_sample[!sapply(sli_sample, function(x)
            is.null(x))]
        sli_area <- bind_rows(sli_sample)
        
      }
      else{
        sli_area = data.frame()
      }
      merging_point_cloud$data <- sli_area
      enable('elements_merge_pointcloud')
      return('Change')
    }
  })
  
  observeEvent(c(input$slip_selection), {
    merging_point_cloud$data <- data.frame()
  })
  
  ## Functions to plot relative sea level information
  
  ### Reactive element to store elements to plot (e.g Limiting data & Sea level indicators)
  elements_plot_merge <- reactiveValues(elements = c())
  
  # Observe User input from 4 Buttons (Terrestrial lim. - Marine lim. - pointcloud - Density)
  observeEvent(c(
    input$elements_merge_indicators,
    input$elements_merge_pointcloud
  ),
  {
    selection_elements <-
      c(input$elements_merge_indicators,
        input$elements_merge_pointcloud)
    print(length(input$elements_merge_indicators))
    print(length(input$elements_merge_pointcloud))
    elements_plot_merge$elements <- selection_elements
  })
  
  # Function to plot sea level information ('Merge')
  output$mergePlot <- renderGirafe({
    girafe(
      ggobj = plot_sea_level(
        data_in_area_merging$data,
        elements_plot_merge$elements,
        merging_point_cloud$data
      ),
      options = list(opts_zoom(max = 5))
    )
  })
  
  # Download point cloud
  
  output$download_pointcloud <- downloadHandler(
    filename = "WALIS_pointcloud.zip",
    content = function(fname) {
      # Random code for download option
      
      random_id <-
        paste0(paste0(sample(c(0:9), 3), collapse = ''),
               paste0(sample(LETTERS, 3), collapse = ''))
      
      # Name for files
      
      name <- paste0("WALIS_",
                     Sys.Date(),
                     "_",
                     nrow(unique(data_in_area_merging$data[data_in_area_merging$data$Type.of.datapoint ==
                                                             'Sea Level Indicator' &
                                                             data_in_area_merging$data$Timing.constraint == 'Equal to', 'WALIS_ID'])),
                     "_SLIPs_ID_",
                     random_id)
      
      # Creating list of files to include in zip folder
      
      fs <- c()
      setwd(tempdir())
      
      # Writing csv file with point cloud
      
      write.csv2(
        merging_point_cloud$data,
        file = paste0(name, '.csv'),
        sep = ',',
        row.names = FALSE,
        col.names = c('WALIS_ID', 'RSL(m)', 'Age(ka)'),
      )
      
    
      # Writing geojson with pointcloud processing metadata
      
      # Extracting parameters:
      
      ## Area
      
      df_area <- area$coord
      matrix_area <- data.matrix(df_area)
      geom <- st_polygon(list(matrix_area)) %>% st_sfc(crs = 4326)
      
      ## Age parameters
      
      # Age range
      age_upp <- input$temp[1]
      age_low <- input$temp[2]
      
      # Percentile range
      upp_age <- perc_range_age$upp_age
      low_age <- perc_range_age$low_age
      
      # Dating techniques
      dating_tech <- paste(input$dating_tech, collapse = '/')
      
      ## RSL indicator
      
      # Elevation error
      low_error <- input$elev_error[1]
      upp_error <- input$elev_error[2]
      
      # Percentile RSL
      
      upp_rsl <- perc_range_rsl$upp_rsl
      low_rsl <- perc_range_rsl$low_rsl
      
      # Paleo RSL Uncertainty
      
      rsl_unc_low <- input$elev_uncert[1]
      rsl_unc_upp <- input$elev_uncert[2]
      
      # RSL filter by type
      
      rsl_filter <- paste(input$type_indicators, collapse = '/')
      
      ## Merging
      
      selected_slip <- paste(input$slip_selection, collapse = '/')
      points_per_slip <- input$points_per_slip
      sampling_strategy <- mySamplingStrategy
      
      #Create dataframe
      
      df1 <- data.frame(
        processing_id = random_id,
        date = Sys.Date(),
        age_range_low = age_low,
        age_range_upp = age_upp,
        age_percentile_low = low_age,
        age_percentile_upp = upp_age,
        dating_techniques = dating_tech,
        rsl_elevation_error_low = low_error,
        rsl_elevation_error_upp = upp_error,
        rsl_percentile_low = low_rsl,
        rsl_percentile_upp = upp_rsl,
        rsl_uncertainty_low = rsl_unc_low,
        rls_uncertainty_upp = rsl_unc_upp,
        rsl_indicator_type = rsl_filter,
        merging_slips = selected_slip,
        merging_points_per_slip = points_per_slip,
        sampling_strategy = sampling_strategy
      )
      
      metadata_merging <- st_sf(df1, geometry = geom)
      print(metadata_merging)
      st_write(metadata_merging, paste0(name, '.geojson'))
      # Saving zip file
      fs <- c(fs, paste0(name, '.csv'))
      fs <- c(fs, paste0(name, '.geojson'))
      zip(zipfile = fname, files = fs)
      if (file.exists(paste0(fname, ".zip"))) {
        file.rename(paste0(fname, ".zip"), fname)
      }
      setwd(root_path)
    },
    contentType = "application/zip"
  )
  
  # Download docker
  
  output$download_docker <- downloadHandler(
    filename = "WALIS_docker.zip",
    content = function(fname) {
      file.copy('r/docker_merging/Dockerfile_container', 'Dockerfile_container')
      file.copy('r/docker_merging/renv_container.lock', 'renv_container.lock')
      file.copy('r/docker_merging/run_analysis.R', 'r/run_analysis.R')
      file.copy('r/docker_merging/readme_container.md', 'readme_container.md')
      file.copy('r/docker_merging/spratt2016stack.bib',
                'spratt2016stack.bib')
      
      # Modify dockerfile
      
      dockerfile  <- readLines("Dockerfile_container")
      peak_sampling <- 'T'
      if (mySamplingStrategy != 'Peak sampling') {
        peak_sampling <- 'F'
      }
      dockerfile[8] <-
        paste(
          'CMD Rscript r/run_analysis.R Data/walis_merging.csv',
          input$points_per_slip,
          peak_sampling,
          collapse = ' '
        )
      writeLines(dockerfile, "Dockerfile_container")
      
      #List with files
      # Saving R scripts / docker file
      
      fs <-
        c(
          'readme_container.md',
          'Dockerfile_container',
          'renv_container.lock',
          'spratt2016stack.bib',
          'r/define_peaks_ranges.R',
          'r/extract_age.R',
          'r/extract_rsl.R',
          'r/join_age_rsl.R',
          'r/run_analysis.R'
        )
      
      
      # Saving SLIPS (WALIS) (SLIPs from merging filters)
      data <- data_in_area_merging$data
      data_name <- 'Data/walis_merging.csv'
      
      data <-
        data_in_area$data %>% dplyr::mutate(
          longitude = sf::st_coordinates(.)[, 1],
          latitude = sf::st_coordinates(.)[, 2]
        ) %>% st_drop_geometry()
      
      drops <-
        c("marker_color",
          "In_area",
          "icon",
          "marker",
          "Perc_Paleo_RSL_uncertainty")
      data <- data[,!(names(data) %in% drops)]
      data <- data[data$WALIS_ID %in% input$slip_selection, ]
      
      fwrite(data,
                  file = data_name,
                  sep = ";")
      
      # Saving Sea level stack and Walis (SLIPs)
      
      fs <- c(fs, 'Data/sea_level_stack_spratt.csv', data_name)
      
      zip(zipfile = fname, files = fs)
      if (file.exists(paste0(fname, ".zip"))) {
        file.rename(paste0(fname, ".zip"), fname)
      }
      file.remove(data_name)
      file.remove('r/run_analysis.R')
      file.remove('Dockerfile_container')
      file.remove('renv_container.lock')
      file.remove('readme_container.md')
      file.remove('spratt2016stack.bib')
    },
    contentType = "application/zip"
  )
  
  ### Functions Plot Sea level
  
  plot_sea_level <- function(data,
                             type_to_display = c("Marine Limiting",
                                                 "Terrestrial Limiting",
                                                 "Sea Level Indicator"),
                             cloud = data.frame()) {
    
    # Check if there is a Point cloud
    
    set.seed(1)
    
    pointcloud = TRUE
    if (nrow(cloud) == 0) {
      pointcloud = FALSE
    }
    
    num_plot_points <- ifelse(nrow(cloud) < 10000, nrow(cloud), 10000)
    
    # Select only the type of datapoints requested by the user
    sub_data <-
      data[data$Type.of.datapoint %in% type_to_display, ] %>% st_drop_geometry()
    
    # Retrieve upper age range (from user input: perc_range_age)
    upp_age <- perc_range_age$upp_age
    age <- age <- paste0("Age..ka..", 50, ".perc", sep = "")
    
    # Retrieve lower age range (from user input: perc_range_age)
    low_age <- perc_range_age$low_age
    
    # Retrieve upper rsl range (from user input: perc_range_rsl )
    upp_rsl <- perc_range_rsl$upp_rsl
    rsl <- paste0("RSL..m..", 50, ".perc", sep = "")
    
    # Retrieve upper rsl range (from user input: perc_range_rsl )
    low_rsl <- perc_range_rsl$low_rsl
    
    ### Ploting
    
    ## Plot message if there is no information selected
    
    if (nrow(sub_data) == 0 & pointcloud == FALSE) {
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
    
    ## Plot with information
    
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
      
      arrow_factor <- (rsl_global_max - rsl_global_min) * 0.075
      
      if (arrow_factor == 0) {
        arrow_factor <- 5
      }
      
      sl_colors <-
        c(
          "Equal to" = "cyan",
          "Older than" = "purple",
          "Younger than" = "orange"
        )
      lim_colors <-
        c(
          "Terrestrial Limiting" = "red",
          "Marine Limiting" = "blue",
          "Sea Level Indicator" = "white"
        )
      
      p <- ggplot() +
        {
          if ('Sea Level Indicator' %in% type_to_display)
            list(
              geom_rect_interactive(
                data = sea_level,
                aes(
                  xmin = eval(parse(text = low_age)),
                  xmax = eval(parse(text = upp_age)),
                  ymin = eval(parse(text = low_rsl)),
                  ymax = eval(parse(text = upp_rsl)),
                  tooltip = WALIS_ID,
                  color = Timing.constraint,
                  fill = Timing.constraint
                ),
                alpha = 0.2,
                show.legend = FALSE
              ),
              geom_errorbar(
                data = sea_level,
                aes(
                  x = eval(parse(text = age)),
                  ymin = eval(parse(text = low_rsl)),
                  ymax = eval(parse(text = upp_rsl)),
                  color = Timing.constraint
                ),
                alpha = 0.5,
                width = 0,
              ),
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
              ),
              geom_segment_interactive(
                data = sea_level_younger,
                aes(
                  y = eval(parse(text = rsl)),
                  x = end,
                  xend = start,
                  yend = eval(parse(text = rsl)),
                  tooltip = WALIS_ID,
                  group = Timing.constraint,
                  color = Timing.constraint
                ),
                arrow = arrow(length = unit(0.1, "inches"), ends = "last")
              ),
              geom_segment_interactive(
                data = sea_level_older,
                aes(
                  y = eval(parse(text = rsl)),
                  x = start,
                  xend = end,
                  yend = eval(parse(text = rsl)),
                  tooltip = WALIS_ID,
                  group = Timing.constraint,
                  color = Timing.constraint
                ),
                arrow = arrow(length = unit(0.1, "inches"), ends = "first")
              ),
              scale_color_manual(values = sl_colors),
              scale_fill_manual(values = sl_colors),
              guides(fill = 'none'),
              guides(color = 'none')
            )
        } +
        
        # Terrestrial limiting
        
        {
          if ('Terrestrial Limiting' %in% type_to_display)
            list(
              new_scale_color(),
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
              ),
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
              ),
              guides(color = "none"),
              scale_color_manual(values = lim_colors)
            )
        } +
        {
          if ('Marine Limiting' %in% type_to_display)
            #Marine limiting
            list(
              new_scale_color(),
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
              ),
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
              ),
              guides(color = "none"),
              scale_color_manual(values = lim_colors)
            )
        } +
        {
          if ('pointcloud' %in% type_to_display & pointcloud)
            list(
              new_scale_color(),
              geom_point(
                data = cloud %>% sample_n(num_plot_points),
                aes(x = AGE, y = RSL),
                alpha =
                  0.1
              ),
              guides(color = "none")
            )
        } +
        {
          if ('2ddensity' %in% type_to_display & pointcloud)
            list(
              new_scale_color(),
              new_scale_fill(),
              geom_hex(
                data = cloud,
                aes(
                  x = AGE,
                  y = RSL
                ),
                alpha = 0.7
              ),
              scale_fill_viridis_c_interactive()
            )
        } +
        scale_x_reverse() +
        theme(aspect.ratio = 1) +
        xlab("Age (ka)") +
        ylab("RSL (m)")
      p
    }
  }
}

# Run the app
shinyApp(ui = ui, server = server)
