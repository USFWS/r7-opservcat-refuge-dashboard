library(shiny)
library(shinydashboard)
library(jsonlite)
library(httr)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(dipsaus)
library(bootstrap)
library(plotly)
library(ggplot2)
library(shinyscroll)
library(shinyBS)
library(ggrepel)
library(data.tree)
library(tidyverse)

source("helper_functions.R")
source("get_dataframes.R")
source("filter_dataframes.R")
source("global.R")
source("ui_outputs.R")

#UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Operation ServCat"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(selectInput(
        inputId = "dropdown",
        label = "Select Your Refuge:",
        c(" ", return_refuge_df()$names),
        selected = NULL,
        multiple = FALSE
        )
      ),
      menuItem("Stats", tabName = "stats", icon = icon("chart-simple")),
      menuItem("Explore Topics", tabName = "topics", icon = icon("list-check"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "stats",
              tags$head(tags$style(
                HTML("
                  .wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}
                  .small-box {height:120px; background-color: #377CA4 !important;}
                  .info-box {height:120px; color: black;}
                  .info-box .info-box-icon {background-color: #377CA4 !important;}
                  .box{background-color: #377CA4 !important;}
                  .skin-blue .main-header .logo {background-color: #2A5F7E !important;}
                  .skin-blue .main-header .navbar {background-color: #2A5F7E !important;}
                  ")
                )
              ),
              div(id = "message1", h1("Instructions"), h3("To view content, ", strong("select your refuge"), " on the menu to the left."), br(), h3(tags$u("NOTE")), h3(em("There are two pages to navigate to: \"Stats\" and \"Explore Topics\". You can toggle between the pages by clicking on their names in the menu. You are currently viewing the ", strong("Stats"), " page.")), h3(em("To collapse or expand the menu, click the three bars at the top of the screen."))),
              hidden(div(id = "page1",
              fluidRow(
                useShinyjs(),
                use_shinyscroll(),
                column(3, valueBox(withSpinner(textOutput("total")), h4("Total References in ServCat"), color = "light-blue", icon = icon("stats", lib = "glyphicon"), width = NULL)),
                column(3, valueBox(withSpinner(textOutput("new")), h4("References Added This Year"), color = "light-blue", icon = icon("certificate", lib = "glyphicon"), width = NULL)),
                column(3, withSpinner(uiOutput("projectCode"))),
                column(3, withSpinner(uiOutput("recentCode"))),
                #bsPopover(id = "projectbox", title = NULL, content = "Click on box to view in ServCat.", placement = "top", trigger = "hover", options = NULL)
                bsPopover(id = "recentbox", title = NULL, content = "Click on box to view in ServCat.", placement = "top", trigger = "hover", options = NULL) #neither popover works if this is deleted, even though it is not being used
              ),
              fluidRow(
                column(9,
                       box(h3(strong("Cumulative Total of References in ServCat Over Time"), style = "margin-bottom: 20px;"),
                           withSpinner(plotlyOutput("contrib")),
                           background = "light-blue", width = NULL, height = 640)
                       ),
                column(3,
                       fluidRow(
                         box(h1(strong(withSpinner(textOutput("remaining")))), h4("References Left to Add to Beat Last Year's Effort"), withSpinner(plotOutput("plotremaining")), height = 310, width = 12, background = "light-blue")
                                ),
                       fluidRow(
                         box(h1(strong(withSpinner(textOutput("arlis")))), h4("Operation ServCat Contributions"), h5("(", em("Inputted by ARLIS", .noWS = c('before','after')), ")"), withSpinner(plotlyOutput("plotarlis")), background = "light-blue", width = 12, height = 310)
                                )
                        )
              )))
      ), tabItem(tabName = "topics",
                 tags$head(tags$style(
                   HTML("
              
                  .rowtitle{height:100px;}
                  .rowresults{height:130px; vertical-align:top;}
                  .rowdownloads{height:70px;}
                  
                  .picker1 {
                    border: 2px solid #0072B2;
                    padding: 5px;
                    border-radius: 20px;
                    display: inline-block;
                    margin-top: 10px;
                  }
                  
                  .picker2 {
                    border: 2px solid #E69F00;
                    padding: 5px;
                    border-radius: 20px;
                    display: inline-block;
                    margin-top: 10px;
                  }
                  
                  .picker3 {
                    border: 2px solid #009E73;
                    padding: 5px;
                    border-radius: 20px;
                    display: inline-block;
                    margin-top: 10px;
                  }
                  
                  .picker4 {
                    border: 2px solid #CC79A7;
                    padding: 5px;
                    border-radius: 20px;
                    display: inline-block;
                    margin-top: 10px;
                  }
                  
                  .triangle1 {
                    color: #0072B2;
                    margin-top: 5px;
                    font-size: 20px;
                    transform: scale(2, 1);
                  }
                  
                  .btn-classFilter {
                    background-color:#377CA4;
                    border-color:#D3D3D3;
                    border-width: 1px;
                    color:#FFFFFF;
                    font-size:17px;
                    border-radius: 20px;
                    height: 50px;
                    text-align: center;
                    display:table-cell;
                    vertical-align:middle;
                  }
                  "),
                  #.rowchart{height:380px;}
                  )
                 ),
                 div(id = "message2", h1("Instructions"), h3("To view content, ", strong("select your refuge"), " on the menu to the left."), br(), h3(tags$u("NOTE")), h3(em("There are two pages to navigate to: \"Stats\" and \"Explore Topics\". You can toggle between the pages by clicking on their names in the menu. You are currently viewing the ", strong("Explore Topics"), " page.")), h3(em("To collapse or expand the menu, click the three bars at the top of the screen."))),
                 hidden(div(id = "page2",
                 fluidRow(class = "rowchart",
                   column(6, offset = 3, align="center", div(id = "topicPlotDiv", plotOutput("topicPlot") %>% withSpinner(), style = "height: 380px;")) #margin-bottom:-190px; 
                 ),
                 fluidRow(
                   column(12, align="center", checkboxInput("checkbox", "Show/hide plot", value = TRUE))
                 ),
                 fluidRow(class = "rowbar",
                   column(6, offset = 3, align = "center", div(id = "bar", tags$hr(style="border-color: black;")))
                 ),
                 fluidRow(class = "rowtitle",
                   column(12,        
                     h1(style="text-align: center; font-size: 35px;", "Select a Topic to Explore More Below"), br())
                 ),
                 fluidRow(class = "rowbuttons",
                   column(3, align="center",
                          actionButtonStyled("mam", "Mammals", icon("paw"), class="btn-lg", style="background-color:#0072B2; border-color:#0072B2; height:70px; font-size:25px; width:170px;"),
                          hidden(div(id="mamSet",
                              #div(class = "triangle1", span("▼")),
                              div(class="picker1", pickerInput("mamSelect", choices = c("Furbearers", "Ungulates", "Cetaceans and Pinnipeds"), label = "Select a mammal category", options = pickerOptions(maxOptions = 1), selected = NULL, multiple = TRUE, width = "170px"))
                          )),
                          hidden(div(id="furSet",
                              #div(class = "triangle1", span("▼")),
                              div(class="picker1", pickerInput("furSelect", choices = c("Bears", "Beavers", "Hares", "Lynx", "Otters", "Weasels, Marten, Ermine, and Mink", "Wolverines", "Wolves"), label = "Select a furbearer", options = pickerOptions(maxOptions = 1), selected = NULL, multiple = TRUE, width = "170px"))
                          )),
                          hidden(div(id="ungSet",
                              #div(class = "triangle1", span("▼")),
                              div(class="picker1", pickerInput("ungSelect", choices = c("Bison", "Caribou", "Deer", "Elk", "Goats", "Moose", "Muskoxen", "Sheep"), label = "Select an ungulate", options = pickerOptions(maxOptions = 1), selected = NULL, multiple = TRUE, width = "170px"))
                          )),
                          hidden(div(id="cetSet",
                              #div(class = "triangle1", span("▼")),
                              div(class="picker1", pickerInput("cetSelect", choices = c("Seals and Sea Lions", "Walruses", "Whales"), label = "Select a cetacean or pinniped", options = pickerOptions(maxOptions = 1), selected = NULL, multiple = TRUE, width = "170px"))
                          ))
                          ),
                   column(3, align="center",
                          actionButtonStyled("bird", "Birds", icon("crow"), class="btn-lg", style="background-color:#E69F00; border-color:#E69F00; height:70px; font-size:25px; width:170px;"),
                          hidden(div(class="picker2", id="birdSet",
                              pickerInput("birdSelect", choices = c("Landbirds", "Waterbirds"), label = "Select a bird category", options = pickerOptions(maxOptions = 1), selected = NULL, multiple = TRUE, width = "170px")
                          )),
                          hidden(div(class="picker2", id="landSet",
                              pickerInput("landSelect", choices = c("Landfowl", "Passerine", "Raptors and Owls"), label = "Select a landbird group", options = pickerOptions(maxOptions = 1), selected = NULL, multiple = TRUE, width = "170px")
                          )),
                          hidden(div(class="picker2", id="waterSet",
                              pickerInput("waterSelect", choices = c("Waterfowl", "Seabirds", "Shorebirds/Wading Birds", "Cormorants and Loons"), label = "Select a waterbird group", options = pickerOptions(maxOptions = 1), selected = NULL, multiple = TRUE, width = "170px")
                          ))
                          ),
                   column(3, align="center",
                          actionButtonStyled("fish", "Fish", icon("fish"), class="btn-lg", style="background-color:#009E73; border-color:#009E73; height:70px; font-size:25px; width:170px;"),
                          hidden(div(class="picker3", id="fishSet",
                              pickerInput("fishSelect", choices = c("Salmon", "Other Salmonids", "Non-Salmonids"), label = "Select a fish category", options = pickerOptions(maxOptions = 1), selected = NULL, multiple = TRUE, width = "170px")
                          )),
                          ),
                   column(3, align="center",
                          actionButtonStyled("more", "More", icon("snowflake"), class="btn-lg", style="background-color:#CC79A7; border-color:#CC79A7; height:70px; font-size:25px; width:170px;"),
                          hidden(div(class="picker4", id="moreSet",
                              pickerInput("moreSelect", choices = c("Amphibians", "Climate/Climate Change", "Contaminants", "Disturbance", "Fire", "Harvest", "Invasive Species", "Invertebrates", "Plants", "Snow", "Wilderness"), label = "Select a category", options = pickerOptions(maxOptions = 1), selected = NULL, multiple = TRUE, width = "170px")
                          ))
                          )
                 ),
                 fluidRow(class = "rowresults",
                   hidden(div(id = "topicResults",
                       p(br(),style="text-align: center; font-size: 25px;","Your refuge has", strong(textOutput("topicCount",inline = TRUE)),"items preserved", br(), "with the subject of", strong(textOutput("topicName", inline = TRUE)))
                   ))
                 ),
                 fluidRow(class = "rowdownloads",
                          column(4, align="left",
                                 div(style = "display:inline-block; vertical-align:top;", dropdownButton(status = "classFilter", inputId = "filterbutton", label = "Select Types of References to Display", circle = FALSE, checkboxGroupInput(inputId = "filter", label = NULL, choices = c("Document", "Project", "Data", "Media"), selected = c("Document", "Project", "Data", "Media"))))
                                 ),
                          column(6, align="right", offset=2,
                                 div(style = "display:inline-block; vertical-align:top;", hidden(downloadButton("downloadExcel", "Download Table (.csv)", icon = shiny::icon("download"), width = 12, style = "background-color:#377CA4; border-color:#D3D3D3; border-width: 1px; color:#FFFFFF; font-size:17px; border-radius: 20px; height: 50px; text-align: center; display:table-cell; vertical-align:middle;"))),
                                 div(style = "display:inline-block; vertical-align:top;", hidden(downloadButton("downloadCodes", "Download Reference Codes (.txt)", icon = shiny::icon("download"), width = 12, style = "background-color:#377CA4; border-color:#D3D3D3; border-width: 1px; color:#FFFFFF; font-size:17px; border-radius: 20px; height: 50px; text-align: center; display:table-cell; vertical-align:middle;")))
                                 )
                 ),
                 fluidRow(class = "rowtable",
                          column(12,
                                 hidden(div(id = "table", withSpinner(DT::dataTableOutput("titleTable"))))
                                 )
                 )))
      )
    )
  )
)

#Server
server <- function(input, output, session) {
  
  #Reactives
  getCodes <- reactiveVal(NULL)
  getExcel <- reactiveVal(NULL)
  getDropdown <- reactiveVal(NULL)
  getSelection <- reactiveVal(NULL)
  clickNum <- reactiveVal(0)
  begin <- reactiveVal(0)
  
  output$contrib <- renderPlotly({
    plot_contrib_sort(input$dropdown)
  })
  #}, height = 550, bg = "transparent")
  
  output$total <- renderText({
    get_total_sort(input$dropdown)
  })
  
  output$new <- renderText({
    get_new_sort(input$dropdown)
  })
  
  output$arlis <- renderText({
    get_arlis_sort(input$dropdown)
  })
  
  output$plotarlis <- renderPlotly({
    plot_arlis_sort(input$dropdown)
  })
  #}, height = 180, width = 250, bg = "transparent")
  
  output$project <- renderText({
    get_project_count(input$dropdown)
  })
  
  output$projectCode <- renderUI({
    tags$a(id = "projectbox", href = paste("https://ecos.fws.gov/ServCat/SavedSearch/Profile/", get_savedsearch_link(getDropdown()), sep=""), target = "blank", valueBox(withSpinner(textOutput("project")), h4("Projects/Surveys"), color = "light-blue", icon = icon("edit", lib = "glyphicon"), width = NULL))
  })
  
  output$recent <- renderText({
    get_most_recent(input$dropdown)
  })
  
  output$recentCode <- renderUI({
    tags$a(id = "recentbox", href = paste("https://ecos.fws.gov/ServCat/Reference/Profile/", get_recent_link(getDropdown()), sep=""), target = "blank", infoBox(h5("Most Recent Addition:"), withSpinner(textOutput("recent")), color = "light-blue", icon = icon("calendar", lib = "glyphicon"), width = NULL))
  })
  
  output$topicPlot <- renderPlot({
    plot_topics(input$dropdown)
  }, height = 400, bg = "transparent")
  
  output$remaining <- renderText({
    get_remaining(input$dropdown)
  })
  
  output$plotremaining <- renderPlot({
    plot_remaining(input$dropdown)
  }, height = 180, width = 250, bg = "transparent")
  
  #Functions for topic querying
  clickActions <- function(refuge, selection){
    count <- return_title_count(refuge, selection)
    output$topicCount <- renderText({count})
    output$topicName <- renderText({selection})
    output$titleTable <- DT::renderDataTable({
      if (count != 0){
        getCodes(return_unformatted_df(refuge, selection))
        formatteddf <- return_title_table(refuge, selection)
        getExcel(return_title_table_download(refuge, selection))
        formatteddf <- formatteddf[which(formatteddf$Type %in% input$filter),]
        if(nrow(formatteddf)>0){
          formatteddf$Title <- paste0("<b>", formatteddf$Title, "</b>")
          formatteddf$Type <- paste0("<em>", formatteddf$Type, "</em>")
        }
        DT::datatable(formatteddf, filter = "none", escape = FALSE, selection = 'none', rownames = FALSE, options = list(searchHighlight = TRUE))
      }
    }, server=FALSE)
    if(count != 0){
      shinyjs::show("filterbutton")
      shinyjs::show("downloadCodes")
      shinyjs::show("downloadExcel")
      shinyjs::show("table")
    }else{
      shinyjs::hide("filterbutton")
      shinyjs::hide("downloadCodes")
      shinyjs::hide("downloadExcel")
      shinyjs::hide("table")
    }
  }
  
  resetButtons <- function(){
    updatePickerInput(session, inputId = "mamSelect", selected = character(0))
    updatePickerInput(session, inputId = "furSelect", selected = character(0))
    updatePickerInput(session, inputId = "ungSelect", selected = character(0))
    updatePickerInput(session, inputId = "cetSelect", selected = character(0))
    updatePickerInput(session, inputId = "birdSelect", selected = character(0))
    updatePickerInput(session, inputId = "landSelect", selected = character(0))
    updatePickerInput(session, inputId = "waterSelect", selected = character(0))
    updatePickerInput(session, inputId = "fishSelect", selected = character(0))
    updatePickerInput(session, inputId = "moreSelect", selected = character(0))
    shinyjs::hide("mamSet")
    shinyjs::hide("furSet")
    shinyjs::hide("ungSet")
    shinyjs::hide("cetSet")
    shinyjs::hide("birdSet")
    shinyjs::hide("landSet")
    shinyjs::hide("waterSet")
    shinyjs::hide("fishSet")
    shinyjs::hide("moreSet")
  }
  
  #Refuge Dropdown
  observeEvent(input$dropdown, {
    if(begin() > 0){
      shinyjs::show("page1")
      shinyjs::hide("message1")
      shinyjs::show("page2")
      shinyjs::hide("message2")
      getDropdown(input$dropdown)
      #runjs("$(document).ready(function(){$('#recentbox').click(function(){window.open('https://ecos.fws.gov/ServCat/Reference/Profile/154365', '_blank');});});")
      delay(100, addPopover(session, id = "projectbox", title = NULL, content = "Click on box to view in ServCat.", placement = "top", trigger = "hover"))
      delay(100, addPopover(session, id = "recentbox", title = NULL, content = "Click on box to view in ServCat.", placement = "top", trigger = "hover"))
      if(clickNum() > 0){
        clickActions(input$dropdown, getSelection())
      }
    }
    begin(begin() + 1)
  })
  
  #Checkbox
  observeEvent(input$checkbox, {
    if(input$checkbox == FALSE){
      shinyjs::hide("topicPlotDiv")
    }else{
      shinyjs::show("topicPlotDiv")
    }
  })
  
  #Mammals
  observeEvent(input$mam, {
    selection <- "Mammals"
    getSelection(selection)
    clickActions(getDropdown(), selection)
    resetButtons()
    shinyjs::show("topicResults")
    shinyjs::show("mamSet")
    #if(clickNum() == 0){
      delay(500, scroll("bar"))
    #}
    clickNum(clickNum() + 1)
  })
  
  observeEvent(input$mamSelect,{
    selection <- input$mamSelect
    getSelection(selection)
    if(!is.null(selection)){
      clickActions(input$dropdown, selection)
      dict <- c("Furbearers" = "furSet", "Ungulates" = "ungSet", "Cetaceans and Pinnipeds" = "cetSet")
      shinyjs::show(as.character(dict[selection]))
      for (key in names(dict)){
        if(key != selection){
          shinyjs::hide(as.character(dict[key]))
        }
      }
    }
  })
  
  observeEvent(input$furSelect,{
    selection <- input$furSelect
    getSelection(selection)
    if(!is.null(selection)){
      clickActions(input$dropdown, selection)
    }
  })
  
  observeEvent(input$ungSelect,{
    selection <- input$ungSelect
    getSelection(selection)
    if(!is.null(selection)){
      clickActions(input$dropdown, selection)
    }
  })
  
  observeEvent(input$cetSelect,{
    selection <- input$cetSelect
    getSelection(selection)
    if(!is.null(selection)){
      clickActions(input$dropdown, selection)
    }
  })
  
  #Birds
  observeEvent(input$bird, {
    selection <- "Birds"
    getSelection(selection)
    clickActions(input$dropdown, selection)
    resetButtons()
    shinyjs::show("birdSet")
    shinyjs::show("topicResults")
    #if(clickNum() == 0){
      delay(500, scroll("bar"))
    #}
    clickNum(clickNum() + 1)
  })
  
  observeEvent(input$birdSelect,{
    selection <- input$birdSelect
    getSelection(selection)
    if(!is.null(selection)){
      clickActions(input$dropdown, selection)
      dict <- c("Landbirds" = "landSet", "Waterbirds" = "waterSet")
      shinyjs::show(as.character(dict[selection]))
      for (key in names(dict)){
        if(key != selection){
          shinyjs::hide(as.character(dict[key]))
        }
      }
    }
  })
  
  observeEvent(input$landSelect,{
    selection <- input$landSelect
    getSelection(selection)
    if(!is.null(selection)){
      clickActions(input$dropdown, selection)
    }
  })
  
  observeEvent(input$waterSelect,{
    selection <- input$waterSelect
    getSelection(selection)
    if(!is.null(selection)){
      clickActions(input$dropdown, selection)
    }
  })
  
  #Fish
  observeEvent(input$fish, {
    selection <- "Fish"
    getSelection(selection)
    clickActions(input$dropdown, selection)
    resetButtons()
    shinyjs::show("fishSet")
    shinyjs::show("topicResults")
    #if(clickNum() == 0){
      delay(500, scroll("bar"))
    #}
    clickNum(clickNum() + 1)
  })
  
  observeEvent(input$fishSelect,{
    selection <- input$fishSelect
    getSelection(selection)
    if(!is.null(selection)){
      clickActions(input$dropdown, selection)
    }
  })
  
  #More
  observeEvent(input$more, {
    selection <- "More"
    getSelection(selection)
    clickActions(input$dropdown, selection)
    resetButtons()
    shinyjs::show("moreSet")
    shinyjs::show("topicResults")
    #if(clickNum() == 0){
      delay(500, scroll("bar"))
    #}
    clickNum(clickNum() + 1)
  })
  
  observeEvent(input$moreSelect,{
    selection <- input$moreSelect
    getSelection(selection)
    if(!is.null(selection)){
      clickActions(input$dropdown, selection)
    }
  })

  #Extra Buttons
  output$downloadExcel <- downloadHandler(
    filename = "ref_table.csv",
    content = function(file){
      write.csv(getExcel(), file, row.names = FALSE)
    }
  )
  
  output$downloadCodes <- downloadHandler(
    filename = "ref_codes.txt",
    content = function(file){
      make_refcode_file(getCodes(), file)
    }
  )
  
}

#Run
shinyApp(ui, server)
