library(shiny)
library(shinydashboard)
library(dplyr)
library(shinythemes)

ui <- dashboardPage(
  dashboardHeader (title = "Databrew Dashboard"),
  dashboardSidebar(
    sidebarMenu(
    menuItem(
      text="Main",
      tabName="main"),
    menuItem(
      text="Charts",
      tabName="charts"),
    menuItem(
      text = 'Widgets',
      tabName = 'widgets'),
    menuItem(
      text = 'About',
      tabName = 'about')
  )),
 dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(
      tabName="main",
      navbarPage(title = '',
                 collapsible = TRUE,
                 tabPanel(title = "Xing's test page",
                          fluidRow(
                            shinydashboard::box(title = 'This is another box',
                                                width = 6,
                                                status = 'warning',
                                                collapsible = TRUE,
                                                footer = 'This is a footer',
                                                plotOutput('plot1')),
                            column(6,
                                   h1('Big heading (h1)'),
                                   h2('Less big heading (h2)'),
                                   h3('Sort of big heading (h3)'),
                                   h4('Not so big heading (h4)'),
                                   h5('Small heading (h5)'),
                                   h6('Heading w/ background (h6)'))
                          ),
                          fluidRow(
                            column(4,
                                   h4('A bunch of inputs'),
                                   p(selectInput('abc', 'Pick a place', choices = c('Home', 'Away', 'In-between')),
                                     radioButtons('xyz', 'What do you like?', choices = c('Ice cream', 'Pizza', 'Both', 'Neither', 'Ice pizza')),
                                     dateRangeInput('aslk', 'Date range', start = Sys.Date() - 20, end = Sys.Date() - 5),
                                     actionButton('action', 'This is a button', icon = icon('download')),
                                     sliderInput('lakjaasa', 'This is a slider', min = 0, max = 100, value = 25),
                                     textInput('qwer', 'This is some text input'))),
                            column(4,
                                   h4('Here is some regular text'),
                                   p('This is normal (ie, p) text, This is normal (ie, p) text, This is normal (ie, p) text,
                                     This is normal (ie, p) text, This is normal (ie, p) text, This is normal (ie, p) text'),
                                   helpText('This is "help text"')),
                            shinydashboard::box(title = 'This is another box',
                                                width = 4,
                                                status = 'info',
                                                collapsible = TRUE,
                                                footer = 'This is a footer',
                                                leaflet::leafletOutput('l1')
                            )
                            )
                          ),
                 navbarMenu("Tab A",
                            tabPanel("Dropdown A"),
                            tabPanel("Dropdown B")),
                 tabPanel('Tab B'),
                 tabPanel('Tab C'),
                 tabPanel('Tab D'),
                 navbarMenu("Tab E",
                            tabPanel("Dropdown 1"),
                            tabPanel("Dropbdown 2"))

    )
    ),
    tabItem(
      tabName = 'about',
      fluidPage(
        fluidRow(
          div(img(src='logo_clear.png', align = "center"), style="text-align: center;"),
          h4('Built in partnership with ',
             a(href = 'http://databrew.cc',
               target='_blank', 'Databrew'),
             align = 'center'),
          p('Empowering research and analysis through collaborative data science.', align = 'center'),
          div(a(actionButton(inputId = "email", label = "info@databrew.cc",
                             icon = icon("envelope", lib = "font-awesome")),
                href="mailto:info@databrew.cc",
                align = 'center')),
          style = 'text-align:center;'
        )
      )
    )
  )
)
)

# Server
server <- function(input, output) {

  output$box1 <- renderUI({
    valueBox(
      "12345", "Some information 1", icon = icon("bullseye"),
      color = 'blue'
    )})

  output$box2 <- renderUI({
    valueBox(
      5, "Some information 2", icon = icon("tachometer"),
      color = 'orange'
    )})
}

shinyApp(ui, server)
