#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$ui <- renderUI({
    
    if(user_input$authenticated == FALSE) {
      fluidPage(
        fluidRow(
          column(width = 3, offset = 5,
                 br(), br(), br(), br(),
                 uiOutput("uiLogin"))
        )
      )
    }
    
    else{
      # Application title
      titlePanel("Old Faithful Geyser Data")
      
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          sliderInput("bins",
                      "Number of bins:",
                      min = 1,
                      max = 50,
                      value = 30)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("distPlot")
        )
      )
    } # else
  })
  
  user_input <- reactiveValues(authenticated = FALSE)
  
  output$uiLogin <- renderUI({
    wellPanel(
      textInput("username", "Username: "),
      passwordInput("password", "Password: "),
      actionButton("login_button", "Log In")
    )
  })
  
  observeEvent(input$login_button, {
    username <- input$username
    password <- input$password
    
    tryCatch({
      if(username == "test" & password == "test"){
        user_input$authenticated <- TRUE
      }
    }, error = function(e){
      user_input$authenticated <- FALSE
    })
  })


  output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)

      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white',
           xlab = 'Waiting time to next eruption (in mins)',
           main = 'Histogram of waiting times')
  })
}

