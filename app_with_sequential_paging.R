library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyjs)

# Create a dictionary of tab names / numbers
tab_dict <- data_frame(number = 1:2,
                       name = c('main',
                                'about'))
n_tabs <- nrow(tab_dict)


header <- dashboardHeader(title="Benchmarking tool")
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'tabs',
    menuItem(
      text="Main",
      tabName="main",
      icon=icon("eye")),
    menuItem(
      text = 'About',
      tabName = 'about',
      icon = icon("cog", lib = "glyphicon"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  useShinyjs(),
  hidden(
    lapply(seq(n_tabs), function(i) {
      div(
        class = "page",
        id = paste0("step", i),
        "Step", i
      )
    })
  ),
  br(),
  actionButton("prevBtn", "< Previous"),
  actionButton("nextBtn", "Next >"),
  tabItems(
    tabItem(
      tabName="main",
      fluidPage(
        fluidRow()
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

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output, session) {
  
  # Define a reactive value which is the currently selected tab number
  rv <- reactiveValues(page = 1)
  
  observe({
    toggleState(id = "prevBtn", condition = rv$page > 1)
    toggleState(id = "nextBtn", condition = rv$page < n_tabs)
    hide(selector = ".page")
    show(paste0("step", rv$page))
  })
  
  # Define function for changing the tab number in one direction or the 
  # other as a function of forward/back clicks
  navPage <- function(direction) {
    rv$page <- rv$page + direction
  }
  
  # Observe the forward/back clicks, and update rv$page accordingly
  observeEvent(input$prevBtn, {
    # Update rv$page
    navPage(-1)
  })
  observeEvent(input$nextBtn, {
    # Update rv$page
    navPage(1)
  })
  
  # Observe any changes to rv$page, and update the selected tab accordingly
  observeEvent(rv$page, {
    tab_number <- rv$page
    tab_name <- tab_dict %>% filter(number == tab_number) %>% .$name
    updateTabsetPanel(session, inputId="tabs", selected=tab_name)
  })
  
  # Observe any click on the left tab menu, and update accordingly the rv$page object
  observeEvent(input$tabs, {
    tab_name <- input$tabs
    tab_number <- tab_dict %>% filter(name == tab_name) %>% .$number
    message(paste0('Selected tab is ', input$tabs, '. Number: ', tab_number))
    rv$page <- tab_number
  })
}

shinyApp(ui, server)