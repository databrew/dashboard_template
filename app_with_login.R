library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(RPostgreSQL)

# source scripts
source('credentials_connect.R')
source('credentials_extract.R')

# define whether creating locally or remotely 
local <- TRUE
if(local){
  credentials <- credentials_extract(credentials_file = 'credentials/credentials_local.yaml', 
                                     all_in_file = TRUE)
} else {
  credentials <- credentials_extract(credentials_file = 'credentials/credentials.yaml', 
                                     all_in_file = TRUE)
}

# make sure a db is created in psql called dashboarddb
# created by CREATE DATABASE dashboarddb

# create a connection object with credentials
co <- credentials_connect(options_list = credentials)
# Function for checking log-in
check_password <- function(user, password){
  message('User/password combination is correct')
  if(user == 'joe'){
    return(TRUE)
  } else {
    return(FALSE)
  }
  # Replace code above with password validation
}

# Function for adding new user
add_user <- function(user, password){
  message('Account just created with the following credentials')
  message('---User: ', user)
  message('---Password: ', password)
  # Add code here to add user to database
}

header <- dashboardHeader(title="App title")
sidebar <- dashboardSidebar(
  sidebarMenu(
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
  tabItems(
    tabItem(
      tabName="main",
      fluidPage(
        h1('Main page'),
        DT::dataTableOutput('user_data')
      )
    ),
    tabItem(
      tabName = 'about',
      fluidPage(
        fluidRow(
          div(img(src='logo_clear.png', align = "center"), style="text-align: center;"),
          h4('Hosted by ',
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
server <- function(input, output) {
  
  # read from database only for user selected
  data <- reactiveValues(user_data = data.frame())
  
  # observe log in and get data from database
  observeEvent(input$submit, {
    data$user_data <- 
      # dbReadTable(conn = co, 
      #             name = 'table1')
      dbGetQuery(conn = co, 
                  statement = paste0("SELECT * FROM table1 WHERE person='", 
                                     input$user, #input$user, 
                                     "'"))
  })
  
  # Reactive values
  logged_in <- reactiveVal(value = FALSE)
  modal_text <- reactiveVal(value = '')
  # Log in modal
  showModal(
    modalDialog(
      uiOutput('modal_ui'),
      footer = NULL
    )
  )
  
  
  # See if log-in worked
  observeEvent(input$submit, {
    cp <- check_password(user = input$user,
                         password = input$password)
    logged_in(cp)
  })
  
  # When OK button is pressed, attempt to log-in. If success,
  # remove modal.
  observeEvent(input$submit, {
    # Did login work?
    li <- logged_in()
    if(li){
      # Update the reactive modal_text
      modal_text(paste0('Logged in as ', input$user))
      removeModal()
    } else {
      # Update the reactive modal_text
      modal_text(paste0('That user/password combination is not correct.'))
    }
  })
  
  # Make a switcher between the log in vs. create account menus
  create_account <- reactiveVal(FALSE)
  observeEvent(input$create_account,{
    currently <- create_account()
    nowly <- !currently
    create_account(nowly)
  })
  observeEvent(input$submit_create_account,{
    currently <- create_account()
    nowly <- !currently
    create_account(nowly)
  })
  observeEvent(input$back,{
    currently <- create_account()
    nowly <- !currently
    create_account(nowly)
  })
  
  output$modal_ui <- renderUI({
    # Capture the modal text.
    mt <- modal_text()
    # See if we're in account creation vs log in mode
    account_creation <- create_account()
    if(account_creation){
      fluidPage(
        fluidRow(
          column(12,
                 align = 'right',
                 actionButton('back',
                              'Back'))
        ),
        h3(textInput('create_user', 'Create username'),
           textInput('create_password', 'Create password')),
        fluidRow(
          column(12, align = 'right',
                 actionButton('submit_create_account',
                              'Create account'))
        )
      )
    } else {
      fluidPage(
        h3(textInput('user', 'Username',
                     value = 'joe'),
           passwordInput('password', 'Password')),
        fluidRow(
          column(6,
                 actionButton('submit',
                              'Submit')),
          column(6, align = 'right',
                 actionButton('create_account',
                              'Create account'))
        ),
        p(mt)
      )}
  })
  
  # Observe account creation
  observeEvent(input$submit_create_account,{
    add_user(user = input$create_user,
             password = input$create_password)
  })
  
  output$user_data <- DT::renderDataTable({
    data$user_data
  })
  
}

shinyApp(ui, server)