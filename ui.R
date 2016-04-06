#gcsetwd()
#runApp("C64Sprite")#, display.mode="showcase") 

library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("spacelab"),
  titlePanel(h1("C64 Sprite")),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("oneMlt", label=h4("Hi/Low resolution"),
        choices = list("One color"=1, "Multicolor"=2), selected=1, inline=TRUE),
      selectInput("mainColor", "Choose main color", 
        choices = c("black", "white", "red", "cyan", "purple", "green", "blue",
        "yellow", "orange", "brown", "indianred1", "gray", "gray25", 
        "lightgreen", "lightblue", "gray50"), selected="white"),
      selectInput("backColor", "Choose background color", 
        choices = c("black", "white", "red", "cyan", "purple", "green", "blue",
        "yellow", "orange", "brown", "indianred1", "gray", "gray25", 
        "lightgreen", "lightblue", "gray50"), selected="black"),
      selectInput("mlt1Color", "Choose color 1 (mlt. color)", 
        choices = c("black", "white", "red", "cyan", "purple", "green", "blue",
        "yellow"), selected="green"),
      selectInput("mlt2Color", "Choose color 2 (mlt. color)", 
        choices = c("black", "white", "red", "cyan", "purple", "green", "blue",
        "yellow"), selected="blue"),
      actionButton("reverse", "Reverse"),
      actionButton("clear", "Clear all"),
      width=2
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Sprite",
          column(5, 
            plotOutput("plotSprite", click = "plot_click"),
            textOutput("tip", inline=TRUE)
          )),
        tabPanel("3 x 8bits",
          column(5, 
             tableOutput("c64code")
          )),
        tabPanel("c64 data",
          column(5, 
             tableOutput("c64data"),
             numericInput("startLine", "Start line number", 
               value=100, min=1, width="35%")
          ))
      ),
      column(5,
        imageOutput("image"),
        fileInput("imageFile", "", accept=".png")
      )
    )
  ) # sidebarLayout
)) # shinyUI