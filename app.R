library(shiny)
library(shinydashboard)

source("plot_contribR.R")
source("get_totalR.R")
source("get_newR.R")
source("get_oldestR.R")
source("plot_subjectsR.R")
source("plot_groupR.R")
source("helper_functions.R")

#UI
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Operation ServCat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(selectInput(
        inputId = "dropdown",
        label = "Select Your Refuge:",
        return_refuge_df()$names
        )
      )
    )
  ),
  dashboardBody(
    fluidRow(
      column(8,
             #box(title="References Added through Operation ServCat", plotOutput("contrib"), background = "olive", width = NULL)
             box(h3(strong("References Pertaining to Each Refuge"), style = "margin-bottom: 20px;"),
                 radioButtons(
                   inputId = "selectplot",
                   label = NULL,
                   choices = c("Operation ServCat Contributions (uploaded by ARLIS)" = "arlis", "All References in ServCat" = "all")
                 ),
                 plotOutput("contrib"),
                 background = "olive", width = NULL)
      ),
      column(4,
             valueBox(textOutput("total"), "Total Count of References in ServCat", color = "olive", icon = icon("stats", lib = "glyphicon"), width = NULL),
             valueBox(textOutput("new"), "Number of References Added This Year", color = "olive", icon = icon("certificate", lib = "glyphicon"), width = NULL),
             # h5(strong("Fun Fact!")),
             box(title = strong("History Preserved"),infoBox("Oldest Record:", textOutput("oldest"), em("(Earliest Date of Issue in ServCat)"), color = "olive", icon = icon("time", lib = "glyphicon"), width=12), color="olive", solidHeader = TRUE, width=NULL)   
             )
    ),
    fluidRow(
      box(h3(strong("Information Gained: Subjects")), p("Top 10 Biological Subject Categories Preserved"), plotOutput("subjects"), background = "olive"),
      box(h3(strong("Information Gained: Types")), p("Non-Document References in ServCat"), plotOutput("group"), background = "olive")
    )
  )
)

#Server
server <- function(input, output) {
  output$contrib <- renderPlot({
    plot_contribR(input$dropdown, input$selectplot)
  })
  
  output$total <- renderText({
    get_totalR(input$dropdown)
  })
  
  output$new <- renderText({
    get_newR(input$dropdown)
  })
  
  output$oldest <- renderText({
    get_oldestR(input$dropdown)
  })
  
  output$subjects <- renderPlot({
    plot_subjectsR(input$dropdown)
  })
  
  output$group <- renderPlot({
    plot_groupR(input$dropdown)
  })
}

#Run
shinyApp(ui, server)
