
# Load R packages
library(dplyr)
library(ggplot2)
library(DT)
library(shiny)
library(shinythemes)

setwd("E://r_rstudio//AppWeb")
df <- read.csv("dataset/video_game.csv")
# Giai thich bien :   15 bien
# Name :              ten tro choi
# Platform :          nha phat trien tro choi 
# Year_of_Release :   nam phat hanh tro choi
# Genre  :            the loai tro choi
# Publisher  :        nha xuat ban tro choi
# NA_Sales :          doanh thu ban san pham o bac mi(trieu do)
# EU_Sales :          doanh thu ban san pham o chau au
# JP_Sales :          doanh thu ban san pham 0 nhat ban
# Other_Sales :       doanh thu ban san pham 0 cac nc khac
# Global_Sales :      doanh thu ban san pham tren the gioi
# Critic_score :      diem so do nhan vien tong hop
# Critic_Count :      so luong nhan vien binh chon 
# User_Score :        diem so do nguoi choi danh gia
# User_Count :        so luong binh chon cua nguoi choi
# Rating :            xep hang 

# tien xu li du lieu
# chuyen NA sang 0
df[is.na(df)] = 0
# them bien total_Critic tong hop luong  
df[["total_Critic"]] = df[["Critic_Score"]] + df[["User_Score"]]
# xoa bien rating, cua dataframe
df["Rating"] = NULL

# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
      tags$head(
        tags$style(HTML("
            @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
            body {
              background-color: #ffff;
              color: white;
            }
            h2 {
              font-family: 'Yusei Magic', sans-serif;
            }
            .shiny-input-container {
              color: #474747;
            },
          ")
          )
      ),
      navbarPage(
        "Top Game",
        tabPanel("Home",
           fluidRow(
             column(12,
                h2(" Show data in table" , style = ("color:red")),
             )
           ),
           fluidRow(
             column(4,
                    selectInput("Name",
                                "Name:",
                                c("All",
                                  unique(as.character(df$Name))))
             ),
             column(4,
                    selectInput("Platform",
                                "Platform:",
                                c("All",
                                  unique(as.character(df$Platform))))
             ),
             column(4,
                    selectInput("Year_of_Release",
                                "Year of Release:",
                                c("All",
                                  unique(as.character(df$Year_of_Release))))
             ),
             
             DTOutput("table")
           ),
        ), # Navbar 1, tabPanel
        tabPanel("Navbar 2", 
                 sidebarLayout(
                   sidebarPanel(
                     tags$h3("Input:"),
                     textInput("txt1", "Given Name:", ""),
                     textInput("txt2", "Surname:", ""),
                     
                   ), # sidebar Panel
                   mainPanel(
                     h1("Header 1"),
                     
                     h4("Output 1"),
                     verbatimTextOutput("txtout"),
                     
                   ) # main Panel
                 )
                 
                ), #end tab panel
        tabPanel("Navbar 3", "This panel is intentionally left blank")
        
      ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output, session) {
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " " )
  })
  
  output$text <- renderText({ 
    "Hello friend!" 
  })
  
  # Filter data based on selections
  output$table <- renderDT(DT::datatable(
    options = list(
      className = 'dt-center',
      pageLength = 5,
      scrollX = TRUE,
      scrollY = 300,
      scroller = TRUE
    ),
    editable=T,
    selection = 'none',
    class = 'cell-border strip hover',
    {
    data <- df
    if (input$Name != "All") {
      data <- data[data$Name == input$Name,]
    }
    if (input$Platform != "All") {
      data <- data[data$Platform == input$Platform,]
    }
    if (input$Year_of_Release != "All") {
      data <- data[data$Year_of_Release == input$Year_of_Release,]
    }
    
    data
  })
    
  )
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
