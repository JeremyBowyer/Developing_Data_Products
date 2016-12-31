library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(navbarPage(title = "Text Prediction Application",
                   theme = shinytheme("darkly"),
                   fluid = TRUE,
                   tabPanel("About",
                            mainPanel(includeHTML("About.HTML"))),
                   tabPanel("Application",
  div(
      div(
        textInput("text", label = h1("Text Prediction", style = "color: #778899"), value = "", placeholder = "Type a phrase..."),
        textOutput("Prediction", inline = TRUE),
        style = "display: inline-block;"
      ),
      style = "height:175px;text-align:center;"
  ),
  hr(),
  
  conditionalPanel(condition="output.condition != '0' ",
    div(
      div(
      tableOutput("Table"),
      style = "display: inline-block; vertical-align: top;"
      ),
      
      div(
      plotOutput("WordCloud", width = "100%"),
      style = "display: inline-block; vertical-align: top;"
      ),
      style = "width: 70%; margin: 0 auto;"
    )
  )
)))
