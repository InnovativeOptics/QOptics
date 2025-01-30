#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(bslib)

# theming options
qoptics_theme <- bs_theme(version = 5,
                          base_font  = font_google("Open Sans"),
                          bg = "white",
                          fg = "#1f0900",
                          primary = "#6532c3")
# Define application UI
shinyUI(page_fluid(
  titlePanel(
    windowTitle = "Q-Optics",
    title = tags$head(tags$link(rel="shortcut icon", 
                                href="https://q-optics.com/cdn/shop/files/Q-Optics_160x.svg?v=1698263012", 
                                type="svg"))),
  theme = qoptics_theme,
  card(class="shadow p-3 mb-5 bg-body rounded",
       card_header(inverse = T,fluidRow(
         column(6,
                align = 'left',
                h5(a(img(width = "150px",
                         src = "https://q-optics.com/cdn/shop/files/Q-Optics_160x.svg?v=1698263012"), 
                     href = "https://q-optics.com/"))),
         column(6, align= 'right',
                h5("(800)858.2121 US & CANADA"),
                h5("(800)288.2669 Local"))))
       ,fluidRow(column(12,align='center',
                        h2(strong("Search eye protection by selecting a loupe style, and a laser device"))))
  ),
  fluidRow(
    column(
      3,
      align = 'center',
      selectInput(
        inputId = "loupestyle",
        label = h4(strong("Q-Optics Frame")),
        choices = sort(qoptics_data$`Q Optics Loupe`),
        selected = 1
      )
    ),
    column(
      3,
      align = 'center',
      selectInput(
        inputId = "style",
        label = h4(strong("Loupe Style")),
        choices = sort(qoptics_data$`Style`),
        selected = 1
      )
    ),
    column(
      3,
      align = 'center',
      selectInput(
        inputId = "mfg",
        label = h4(strong("Manufacturer")),
        choices = sort(dental_data$`Laser Mfg`),
        selected = 1
      )
    ),
    column(
      3,
      align = 'center',
      selectInput(
        inputId = "mod",
        label = h4(strong("Model")),
        choices = dental_data$`Laser Model`,
        selected = 1
      )
    )),
  fluidRow(
    
    column(
      12,
      align = "center",
      br(),
      actionButton("run",
                   icon = icon("magnifying-glass"),
                   style='padding-left:50px;padding-right:50px;padding-top:1px;padding-bottom:1px; font-size:80%',
                   h5(strong("Search")),
                   class = "btn-primary"))
  ),
  br(),
  fluidRow(
    column(12,
           p("Your information not available in the dropdowns? Contact Innovative Optics at (763) 425-7789"))
  ),
  conditionalPanel(
    condition = "input.run",
    card(class="shadow p-3 mb-5 bg-body rounded",
         fluidRow(column(12, align = "center",
                         h3(em("Device Information")),
                         tableOutput("userInfo"))),
         fluidRow(column(12,
                         align = "center", 
                         h3(em("Compatible Innovative Optics Product")),
                         tableOutput("tableInfo"))),
         fluidRow(column(12, align = 'center',
                         imageOutput("productImage",
                                     height = "75%", width = '50%'))),
         
         fluidRow(column(12,
                         align = 'center',
                         h3(style = {
                           "color: #004793;"
                         },
                         em("Frequently Purchased Together")))),
         fluidRow(
           column(4,
                  align = 'center',
                  card(class="shadow p-3 mb-5 bg-body rounded",
                       h5("Fit-over Glasses"),
                       imageOutput("rec1",
                                   height = "100%"),
                       tableOutput("tableRec1"))),
           column(4,
                  align = 'center',
                  card(class="shadow p-3 mb-5 bg-body rounded",
                       h5("Form-fit Glasses"),
                       imageOutput("rec2",
                                   height = "100%"),
                       tableOutput("tableRec2"))),
           column(4,
                  align = 'center',
                  card(class="shadow p-3 mb-5 bg-body rounded",
                       h5("Patient Goggles"),
                       imageOutput("rec3",
                                   height = "100%"),
                       tableOutput("tableRec3")))
         ),
         fluidRow(column(12,
                         align = 'left',
                         h1(
                           htmlOutput("mailToLink"))
         ))
    )

  ),
  #)),
  card(class="shadow p-3 mb-5 bg-body rounded",
       card_footer(h5(
         style = {
           "color: #0FE410;
                         text-shadow: 1px 1px 1px black;"
         },
         "Powered by Innovative Optics"))
  )
)
)


# sidebarLayout(
# sidebarPanel(
#   width = 4,
#   selectInput(
#     inputId = "loupestyle",
#     label = "First, select your style of QOptics loupes!",
#     choices = andau_data$`QOptics Frame`,
#     selected = NULL
#   ),
#   selectInput(
#     inputId = "mfg",
#     label = "Next, select your laser's manufacturer!",
#     choices = dental_data$`Laser Mfg`,
#     selected = NULL
#   ),
#   selectInput(
#     inputId = "mod",
#     label = "Finally, select your laser model!",
#     choices = dental_data$`Laser Model`,
#     selected = NULL
#   )
# ),
# mainPanel(width = 8,
#           tableOutput("tableInfo"))





# Define UI for application that suggest eyewear based on loupe type and laser type
# shinyUI(fluidPage(
#     # Application title
#     h3("Welcome to the interactive dashboard"),
#     h4("Where you can easily find which inserts fit your loupes and which lens protects against your laser!"),
#     hr(),
#     sidebarLayout(sidebarPanel(
#     selectInput("frame",
#                        "First, select the style of loupes!",
#                        choices = andau_data$`Andau Frame`,
#                        selected = NULL),
#     h5("Next, select if you want to search by laser Category /n or by using our Dental or Medical laser lists!"),
#     tabsetPanel(
#       id = "profession",
#       type = "pills",
#       tabPanel("Category",
#         h5("Finally, select your laser category!"),       
#         selectInput("category",
#                     "Laser Category",
#                     choices = category_list,
#                     selected = NULL)),
#       tabPanel("Dental", 
#                h5("Finally, select your laser manufacturer and model!"),
#                selectInput("dentman",
#                            "Dental laser manufacturer",
#                            choices = dental_data$`Laser Mfg`,
#                            selected = NULL),
#                selectInput("dentmod",
#                            "Dental laser model",
#                            choices = dental_data$`Laser Model`,
#                            selected = NULL)),
#       tabPanel("Medical", 
#                h5("Finally, select your laser manufacturer and model!"),
#                                selectInput("medman",
#                                            "Medical laser manufacturer",
#                                            choices = medical_data$`Laser Mfg`,
#                                            selected = NULL),
#                                selectInput("medmod",
#                                            "Medical laser model",
#                                            choices = medical_data$`Laser Model`,
#                                            selected = NULL))
#                         )
#     ),
#     mainPanel(tableOutput("suglensCategory"),
#               tableOutput("sugdent"),
#               tableOutput("sugmed"),
#               imageOutput("Loupe"))
#     )
#   )
#)

