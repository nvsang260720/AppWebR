## add library ##
library(dplyr)
library(ggplot2)
library(DT)
library(shiny)
library(shinythemes)
library(shinydashboard)

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
# JP_Sales :          doanh thu ban san pham o nhat ban
# Other_Sales :       doanh thu ban san pham o cac nc khac
# Global_Sales :      doanh thu ban san pham tren the gioi
# Critic_score :      diem so do nhan vien tong hop
# Critic_Count :      so luong nhan vien binh chon 
# User_Score :        diem so do nguoi choi danh gia
# User_Count :        so luong binh chon cua nguoi choi
# Rating :            xep hang 

# Tien xu li du lieu
# chuyen NA sang 0
df[is.na(df)] = 0
# them bien total_Critic tong hop luong  danh gia
df[["total_Critic"]] = df[["Critic_Score"]] + df[["User_Score"]]
# xoa bien rating, cua dataframe
df["Rating"] = NULL


ui <- dashboardPage(
  
  dashboardHeader(title = "Game Statistics"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dataset", tabName = "Dataset", icon = icon("dashboard")),
      menuItem("Top Game", tabName = "Topgame", icon = icon("th"))
    )
  ),
  ## Body content
  dashboardBody(
    #custom css
    tags$head(
      tags$style(HTML("
            @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
            body {
              background-color: #ffff;
            }
            h2, h3, h4 {
              font-family: 'Yusei Magic', sans-serif;
              color: #000000;
            }
            p {
              color: #000000;
            }
            .h2-title {
              color:red;
            }
            
          ")
      )
    ), #end cusstom css
    tabItems(
      
      # Second tab content
      tabItem(tabName = "Dataset",
              fluidRow(
                column(12,
                       box(
                         title = "Show data in table",
                         status = "primary",
                         solidHeader = TRUE, 
                         collapsible = TRUE,
                         style = (" color: White "),
                         div( 
                            div( 
                                 p("Filter information from data."),
                                 p("We can search the game name through the information filter and the search bar.",
                                   style = (""))
                            )
                         )
                       )   
                )
              ),#end fluidRow
              fluidRow(
                box( width = ("null"),
                
                column(4,
                       selectInput("Platform",
                                   "Platform:",
                                   c("All",
                                     unique(as.character(df$Platform)))
                                   )
                ),
                column(4,
                       selectInput("Year_of_Release",
                                   "Year of Release:",
                                   c("All",
                                     sort(decreasing = TRUE,unique(as.character(df$Year_of_Release)))
                                     )
                                   )
                ),
                column(4,
                       selectInput("Genre",
                                   "Genre Game:",
                                   c("All",
                                     unique(as.character(df$Genre)))
                                   )
                ),
                column(12,
                       DTOutput("table")   
                )
                )
              ), #end fluidRow
      ), #end tab item 
      # First tab content
      tabItem(tabName = "Topgame",
              fluidRow(
                column(12,
                       box(
                          title = "Top Sales in the world",
                          status = "primary",
                          solidHeader = TRUE, 
                          collapsible = TRUE,
                          width = ("null"),
                          div(
                            selectInput("Years",
                                        label = "Choose Sales Year",
                                        c("All",
                                          sort(decreasing = TRUE,unique(as.character(df$Year_of_Release)))
                                        )
                                        ),
                            plotOutput("plot",
                                        width = "100%",
                                        height = "400px",
                                        click = "plot_click",
                                        dblclick = NULL,
                                        hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
                                        brush = brushOpts(id = "plot_brush"),
                                        inline = FALSE
                                        
                                      )
                              )#end div
                           )#end box
                       ) # end colum 
              ) # end fluidRow
      ) #end tab item 
      
    )
  )
)

server <- function(input, output, session) {
  data <-reactive({
    df %>%filter(Year_of_Release %in% input$Years) %>% group_by(Platform) %>%  summarise(a_sum=sum(Other_Sales))
  })
  output$plot <- renderPlot({
    g <- ggplot(data(), aes( y = a_sum, x = Platform))
    g + geom_bar(stat = "sum")
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
      if (input$Platform != "All") {
        data <- data[data$Platform == input$Platform,]
      }
      if (input$Year_of_Release != "All") {
        data <- data[data$Year_of_Release == input$Year_of_Release,]
      }
      if (input$Genre != "All") {
        data <- data[data$Genre == input$Genre,]
      }
      
      data
    })
    
  )
} # server

shinyApp(ui, server)