library(shiny)
library(bslib)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "lumen"),
  titlePanel(
    h1("Loot Box Opening Simulation", align = "center")
  ),
  div(
    fluidRow(
      column(10,
             align="left",
             span("A common loot box scheme in games involves obtaining a collection of collectibles to earn the final reward. To acquire these collectibles, players must open cases that drop one collectible at a time, with the possibility of receiving duplicates. An exchange system is often available, allowing players to trade duplicate collectibles for new ones at a specified rate. The question then arises: what is the probability of obtaining the final reward after opening a certain number of boxes?")
      ),
      column(2,
             align="center",
             HTML("<span>Created by <br><a href='https://zehuiyin.github.io/' target='_blank'>Zehui Yin</a></span>")
      ),
      style = 'border-bottom: 1px solid'
    ),
    fluidRow(
      column(12,
             align="center",
             HTML("&nbsp;"))
    ),
    fluidRow(
      column(4,
             align="center",
             numericInput("collect", 
                          "How Many Collectibles Are There In The Collection", 
                          min = 1, 
                          step = 1, 
                          value = 60)
      ),
      column(4,
             align="center",
             numericInput("box_opened", 
                          HTML("Total Number of Cases Opened <br> &nbsp;"), 
                          min = 1, 
                          step = 1,
                          value = 80)
      ),
      column(4,
             align="center",
             numericInput("exrate", 
                          "How Many Duplicates Required to Trade A New One", 
                          min = 1, 
                          step = 1,
                          value = 2)
      ),
      style = 'border-bottom: 1px solid'
    ),
    fluidRow(
      column(12,
             align="center",
             HTML("&nbsp;"))
    ),
    fluidRow(
      column(12,
             align="center",
             span("The optimal strategy is to refrain from exchanging any duplicates until you have accumulated enough to complete the entire collection.")
      ),
    ),
    fluidRow(
      column(12,
             align="center",
             htmlOutput("prob")
      )
    ), style = "max-width: 1200px; margin: auto;")
)

server <- function(input, output, session) {
  output$prob <- renderText({
    req(input$collect)
    req(input$box_opened)
    req(input$exrate)
    
    sims <- replicate(1000, {
      s <- sample(1:input$collect, 
                  size = input$box_opened,
                  replace = TRUE)
      s_no_dup <- unique(s)
      
      dup <- length(s) - length(s_no_dup)
      uniq <- length(s_no_dup)
      
      dup >= (input$collect - uniq) * input$exrate
    }, simplify = TRUE)
    
    res <- sum(sims)/length(sims)*100
    
    paste0("<span style='font-size:1.2em'>Based on a simulation of 1,000 runs, the probability of obtaining the final reward is <b>", 
           res, "%</b>.</span>")
  })
}

shinyApp(ui, server)
