library(shiny)
library(rvest)
library(zoo)
library(ggplot2)
library(reshape2)
library(markdown)

# Define UI for application that plots random distributions 
shinyUI(navbarPage("Historical Polling Data",
          tabPanel("Application",
  				# Sidebar with a slider input for number of observations
  				sidebarPanel(
  						dateRangeInput("date", 
  								"Date Range:", 
  								start = as.Date("2016-05-12")-60,
  								end = "2016-05-01", 
  								min = "2015-01-01",
  								max = "2016-05-12"),
  						radioButtons("party",
  								"Party:",
  								choices = c("Republican",
  										"Democrat")),
  						uiOutput("candidatelist")
  				),
  				
  				# Show a plot of the generated distribution
  				mainPanel(
  						tabPanel("Plot", plotOutput("pollPlot"))
  				)
    ),
    
          tabPanel("About",
            mainPanel(includeHTML("About.html")))
  )
)