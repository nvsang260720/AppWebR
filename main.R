## add library ##
library(dplyr)
library(ggplot2)
library(DT)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(fontawesome)
library(devtools) 

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


ui = dashboardPage(skin = "black",
  
  dashboardHeader(
    title = "Game Statistics"
                  
                  ),
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
              color: #ffff;
            }
            p {
              color: #000000;
            }
            .h2-title {
              color:red;
            }
            .box.box-solid.box-primary>.box-header {
                    color:#fff;
                    background:#666666
                }

            .box.box-solid.box-primary{
                  border-bottom-color:#666666;
                  border-left-color:#666666;
                  border-right-color:#666666;
                  border-top-color:#666666;
            }
            
            
          ")
      )
    ), #end cusstom css
    tabItems(
      
      # Second tab content
      tabItem(tabName = "Dataset",
              fluidRow(
                 box(
                   
                   with =4,
                   title = "Show data in table",
                   status = "info",
                   solidHeader = TRUE,
                   div( 
                      div( 
                           p("Filter information from data."),
                           p("We can search the game name through the information filter and the search bar.",
                             )
                      )
                   )
                )
              ),#end fluidRow
              fluidRow(
                box( width = 12,
                
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
                )#end box
              ), #end fluidRow
      ), #end tab item 
      # First tab content
      tabItem(tabName = "Topgame",
              fluidRow(
                box(width = 3,
                    class = (""),
                    title = "Number Games",
                    status = "success", 
                    solidHeader = TRUE,
                    div(
                      span(textOutput("text_name"))
                      
                    )
                ),
                box(width = 3,
                    class = (""),
                    title = "Number Platform",
                    status = "info", 
                    solidHeader = TRUE,
                    div(
                      span(textOutput("text_platform"))
                      
                    )
                ),
                box(width = 3,
                    class = (""),
                    title = "Number Genre",
                    status = "warning", 
                    solidHeader = TRUE,
                    div(
                      span(textOutput("text_genre"))
                      
                    )
                ),
                box(width = 3,
                    class = (""),
                    title = "Number Publisher",
                    status = "danger", 
                    solidHeader = TRUE,
                    div(
                      span(textOutput("text_publisher"))
                      
                    )
                )
              ),
              fluidRow(
                 box(
                    title = "Top Sales in the world",
                    status = "info",
                    solidHeader = TRUE, 
                    collapsible = TRUE,
                    width = 12,
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
                ), #end fluidRow
                fluidRow(
                   box(
                     title = "Top Genre in the world",
                     status = "info",
                     solidHeader = TRUE, 
                     collapsible = TRUE,
                     column(12,
                     div(
                        
                        plotOutput("Plot_1",
                                   width = "100%",
                                   height = "400px",
                                   click = "plot_click",
                                   dblclick = NULL,
                                   inline = FALSE
                                   ),
                      
                     )#end div
                   )#end column
                )#end box
                ) # end fluidRow
      ) #end tab item 
      
    )#end dashboardBody
  )#end dashboardPage
)#end ui

server = function(input, output, session) {
  group_name = df %>% group_by(Name) %>% summarise(Sales=sum(as.numeric(total_Critic)))
  group_platform = df %>% group_by(Platform)
  group_genre = df %>% group_by(Genre)
  group_publisher = df %>% group_by(Publisher)
  
  select_name= c(unique(as.character(group_name$Name)))
  select_platform= c(unique(as.character(group_platform$Platform)))
  select_genre= c(unique(as.character(group_genre$Genre)))
  select_publisher= c(unique(as.character(group_publisher$Publisher)))
  
  leng_name = round(as.numeric(length(select_name)))
  leng_platform = round(as.numeric(length(select_platform)))
  leng_genre = round(as.numeric(length(select_genre)))
  leng_publisher = round(as.numeric(length(select_publisher)))
  

  data2 = na.omit(group_name)
  #df6 = data2[order(data2$Sales, decreasing = TRUE)]
  
  print(data2)

  output$text_name = renderText(
    { 
      leng_name
    }
  )
  output$text_platform = renderText(
    { 
      leng_platform
    }
  )
  output$text_genre = renderText(
    { 
      leng_genre
    }
  )
  output$text_publisher = renderText(
    { 
      leng_publisher
    }
  )
  
  output$Plot_1 = renderPlot({
    df2 = df %>% group_by(Genre) %>%  summarise(Sales=sum(total_Critic))
    df3 = c(unique(as.character(df2$Sales)))
    title= c(unique(as.character(df2$Genre)))
    pct = round(as.numeric(df3)/sum(as.numeric(df3))*100)

    lbls = paste(pct) # add percents to labels
    lbls = paste(lbls,"%",sep="") # ad % to labels test pull
    # Render a barplot
    pie(pct, 
        main="Years (1976-2017)",
        col=rainbow(12),
        labels = lbls)
    legend("topright", title , cex=1,fill=rainbow(length(pct)))
  })
  output$plot = renderPlot({
    if (input$Years != "All") {
      data = reactive({
        df %>%filter(Year_of_Release %in% input$Years) %>% group_by(Platform) %>%  summarise(Sales=sum(Global_Sales))
      })
    }else{
      data = reactive({
        df %>% group_by(Platform) %>%  summarise(Sales=sum(Global_Sales))
      })
    }

    color = c("blue", "red")
    
    g = ggplot(data(), aes( y = Sales, x = Platform, fill = "Global Sales"), col=color)
    g + geom_bar(stat = "identity")
  })
  
  # Filter data based on selections
  output$table =  renderDT(DT::datatable(
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
      data = df
      if (input$Platform != "All") {
        data = data[data$Platform == input$Platform,]
      }
      if (input$Year_of_Release != "All") {
        data = data[data$Year_of_Release == input$Year_of_Release,]
      }
      if (input$Genre != "All") {
        data = data[data$Genre == input$Genre,]
      }
      
      data
    })
    
  )
} # end server

shinyApp(ui, server)