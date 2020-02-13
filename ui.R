library(shiny)
library(rtweet)
library(tweetbotornot2)
library(ggplot2)
library(httr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  includeHTML("html.html"),
  includeCSS("css.css"),
  div(style = "min-width:450px;margins:0 auto",

  # Application title
  HTML('<center><img style="margin-top:10px" src="logo.png" width="160px"></center>'),
  titlePanel("{TweetBotOrNot2}"),
  br(),

  # Sidebar with a slider input for number of bins
    fluidRow(div(style = "font-size:11px",
      column(12,
        textInput("user", "Screen name:", value = ""),
        actionButton("go", "Go", width = "30px"),
        align = "center"))
    ),
  # Show a plot of the generated distribution
  fluidRow(
    div(style = "max-width:650px;margin:0 auto;min-width:450px;min-height:350px;max-height:500px",
    column(12, plotOutput("probPlot", width = "100%", height = "425px"),
      align = "center"))), br(), br(), br(), br(), br(), br(),
  div(style = "font-size:11px", fluidRow(
    column(12, includeHTML("footer.html"),
      align = "center"))
  ), br(), br(), br(), br(),
  fluidRow(div(style = "width:95%;min-width:450px;max-width:850px;margin:0 auto;color:#666;font-size:10px",
    column(12, includeHTML("footer-about.html"),
      style = 'text-align:justify')
  )),
  br(), br())
)


uiFunc <- function(req) {
  ui
}
