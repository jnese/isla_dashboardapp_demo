## app.R ##
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(DT)
library(reactable)
library(htmltools)
library(shinyWidgets)

source(here::here("functions_stu.R"))
source(here::here("functions_sch.R"))
source(here::here("data_prep_anon.R"))

# https://stackoverflow.com/questions/33503119/use-infobox-from-shinydashboard-into-shiny

addDeps <- function(x) {
  if (getOption("shiny.minified", TRUE)) {
    adminLTE_js <- "app.min.js"
    adminLTE_css <- c("AdminLTE.min.css", "_all-skins.min.css")
  } else {
    adminLTE_js <- "app.js"
    adminLTE_css <- c("AdminLTE.css", "_all-skins.css")
  }
  
  dashboardDeps <- list(
    htmlDependency("AdminLTE", "2.0.6",
                   c(file = system.file("AdminLTE", package = "shinydashboard")),
                   script = adminLTE_js,
                   stylesheet = adminLTE_css
    ),
    htmlDependency("shinydashboard",
                   as.character(utils::packageVersion("shinydashboard")),
                   c(file = system.file(package = "shinydashboard")),
                   script = "shinydashboard.js",
                   stylesheet = "shinydashboard.css"
    )
  )
  
  shinydashboard:::appendDependencies(x, dashboardDeps)
}


ui <- navbarPage("ISLA Dashboard 2.0", theme = shinytheme("flatly"),
        tabPanel("School", icon = icon("school"),
          sidebarPanel(width = 3,
            radioButtons("grade_level", "Select grade:", choices = c("All" = "All",
                                                                     "Grade 6" = "6",
                                                                     "Grade 7" = "7",
                                                                     "Grade 8" = "8")),
            dateRangeInput("sch_daterange", "Select the date range:", 
                           start = min_date, end = max_date, min = min_date, max = max_date)
          ),
          mainPanel(
            tabsetPanel(type = "pills",
              tabPanel("ISLA", icon = icon("check-square-o"),
                fluidRow(
                  box(title = "ISLA Fidelity",
                      plotOutput("sch_fidelity_plot"),
                      downloadButton(outputId = "sch_fidelity_dl", ####
                                     label = "Download the plot"), ###########
                      width = 12),
                  box(title = "Did the student Re-enter the Classroom?",
                      plotOutput("sch_reenter_plot"),
                      downloadButton(outputId = "sch_reenter_dl", ####
                                     label = "Download the plot"), ###########
                      width = 12),
                  box(title = "If No Re-entry, Why?",
                      plotOutput("sch_whynoreentry_plot"),
                      downloadButton(outputId = "sch_whynoreentry_dl", ####
                                     label = "Download the plot"), ###########
                      width = 12),
                  box(title = "",
                      plotOutput("sch_adminsafety_plot"),
                      downloadButton(outputId = "sch_adminsafety_dl", ####
                                     label = "Download the plot"), ###########
                      width = 12)
                )
              ),          
              tabPanel("Incidents", icon = icon("list-ol"),
                fluidRow(
                  valueBoxOutput("sch_totalincidents_box", width = 6),
                  valueBoxOutput("sch_totalstudents_box", width = 6),
                  shinyWidgets::radioGroupButtons("sch_incidents_plotselect", "Select Plot:",
                                                  choices = c("Monthly", "Daily", "Time of day", "Day of week"),
                                                  selected = "Monthly",
                                                  direction = "horizontal",
                                                  justified = TRUE,
                                                  individual = TRUE,
                                                  checkIcon = list(
                                                    yes = icon("check-square")),
                                                  #inline = TRUE, 
                                                  width = '100%', 
                               ),#
                  box(title = "Number of Student in Room",
                      plotOutput("sch_incidents_time_plot"),
                      downloadButton(outputId = "sch_incidents_time_dl", ####
                                     label = "Download the plot"), ###########
                      width = 12)
                )
              ),
              tabPanel("Instructional Time Lost", icon = icon("stopwatch"),
                fluidRow(
                  valueBoxOutput("sch_totalhourslost_box", width = 6),
                  valueBoxOutput("sch_aveminslost_box", width = 6),
                  shinyWidgets::radioGroupButtons("sch_minslost_plotselect", "Select Plot:",
                                                  choices = c("Monthly", "Daily", "Time of day", "Day of week"),
                                                  selected = "Monthly",
                                                  direction = "horizontal",
                                                  justified = TRUE,
                                                  individual = TRUE,
                                                  checkIcon = list(
                                                    yes = icon("check-square")),
                                                  #inline = TRUE, 
                                                  width = '100%', 
                  ),#
                  box(title = "Average Minutes in Room",
                      plotOutput("sch_minslost_time_plot"),
                      downloadButton(outputId = "sch_minslost_time_dl", ####
                                     label = "Download the plot"), ###########
                      width = 12)
                )
              ),
              tabPanel("People", icon = icon("hands-helping"),
                fluidRow(
                   box(title = "Most Incidents by Student",
                       plotOutput("sch_incidstu_plot"),
                       width = 6),
                   box(title = "Most Incidents by Teacher",
                       plotOutput("sch_incidtch_plot"),
                       width = 6)
                ),
                fluidRow(
                   box(title = "Relationships",
                       DTOutput("sch_relationships_table"),
                       width = 12)
                )
              )
            )
          )
        ),
        tabPanel("Student", icon = icon("user"),
          sidebarPanel(width = 3,
            selectInput("student_name", "Student Name:", choices = student_name),
            dateRangeInput("stu_daterange", "Select the date range", 
                           min = min_date, max = max_date,
                           start = min_date, end = max_date),
            dateInput("intervention1", "Select intervention start date")
          ),
          mainPanel(
            tabsetPanel(type = "pills",
              tabPanel("Time out of class", icon = icon("stopwatch"),
                       fluidRow(
                         valueBoxOutput("stu_totalincidents_box", width = 6), 
                         valueBoxOutput("stu_totalminslost_box", width = 6)
                       ),
                       fluidRow(
                         box(title = "Minutes in Room by Date",
                             plotOutput("stu_timelost_plot"),
                             downloadButton(outputId = "stu_timelost_plot_dl", ####
                                            label = "Download the plot"), ###########
                             width = 12)
                       ),
                       fluidRow(
                         box(DTOutput("stu_timelost_table"),
                             width = 12)
                       )
              ),
              tabPanel("ISLA Fidelity", icon = icon("check-square-o"),
                       fluidRow(
                         box(plotOutput("stu_fidelity_plot"),
                             downloadButton(outputId = "stu_fidelity_plot_dl", ####
                                            label = "Download the plot"), ###########
                             width = 12)
                       ),
                       fluidRow(
                         box(DTOutput("stu_fidelity_table"),
                             width = 12)
                       )
              )
            )
          )
        ),
        tabPanel("Data", icon = icon("table"),
                 mainPanel(
                   fluidRow(
                     column(title = "Data from Google Sheet",
                         reactableOutput("completedata_table"),
                         width = 12)
                   )
                 )
        )
)
   
ui <- addDeps(
  tags$body(shiny::fluidPage(ui)
  )
)

server <- function(input, output) {
# SCHOOL
  output$sch_totalincidents_box <- renderValueBox({valueBox(
    sch_totalincidents_box_fx(input), 
    "Total Incidents", 
    icon = icon("door-open"),
    color = "yellow",
  )})
  output$sch_totalstudents_box <- renderValueBox({valueBox(
    sch_totalstudents_box_fx(input), 
    "Total Students", 
    icon = icon("users"),
    color = "orange",
  )})
  output$sch_incidents_time_plot <- renderPlot(sch_incidents_time_plot_fx(input)) 
  #### new  
  output$sch_incidents_time_dl <- downloadplot_sch_fx("Number_of_Student_in_Room",
                                                input, 
                                                sch_incidents_time_plot_fx,
                                                input$grade_level,
                                                input$sch_daterange)            ##############################
  output$sch_totalhourslost_box <- renderValueBox({valueBox(
    sch_totalhourslost_box_fx(input), 
    "Total Hours Lost", 
    icon = icon("clock"),
    color = "light-blue",
  )})
  output$sch_aveminslost_box <- renderValueBox({valueBox(
    sch_aveminslost_box_fx(input),
    "Average Mins in Room",
    icon = icon("stopwatch"),
    color = "blue",
  )})
  output$sch_minslost_time_plot <- renderPlot(sch_minslost_time_plot_fx(input)) 
  #### new  
  output$sch_minslost_time_dl <- downloadplot_sch_fx("Average_Minutes_in_Room",
                                                input, 
                                                sch_minslost_time_plot,
                                                input$grade_level,
                                                input$sch_daterange)            ##############################
  output$sch_incidstu_plot <- renderPlot(sch_incidstu_plot_fx(input))
  output$sch_incidtch_plot <- renderPlot(sch_incidtch_plot_fx(input)) 
  output$sch_relationships_table <- renderDT(sch_relationships_table_fx(input))
  output$sch_fidelity_plot <- renderPlot(sch_fidelity_plot_fx(input))
  #### new  
  output$sch_fidelity_dl <- downloadplot_sch_fx("ISLA_Fidelity",
                                                input, 
                                                sch_fidelity_plot_fx,
                                                input$grade_level,
                                                input$sch_daterange)            ##############################
  output$sch_reenter_plot <- renderPlot(sch_reenter_plot_fx(input))
  #### new  
  output$sch_reenter_dl <- downloadplot_sch_fx("Did_the_student_Re-enter_the_Classroom",
                                                input, 
                                                sch_reenter_plot_fx,
                                                input$grade_level,
                                                input$sch_daterange)            ##############################
  output$sch_whynoreentry_plot <- renderPlot(sch_whynoreentry_plot_fx(input))
  #### new  
  output$sch_whynoreentry_dl <- downloadplot_sch_fx("If_No_Re-entry_Why",
                                               input, 
                                               sch_whynoreentry_plot_fx,
                                               input$grade_level,
                                               input$sch_daterange)            ##############################
  output$sch_adminsafety_plot <- renderPlot(sch_adminsafety_plot_fx(input))
  #### new  
  output$sch_adminsafety_dl <- downloadplot_sch_fx("Sent_to_Admin_and-or_Safety_Risk",
                                                    input, 
                                                    sch_adminsafety_plot_fx,
                                                    input$grade_level,
                                                    input$sch_daterange)            ##############################
# STUDENT  
  output$stu_totalincidents_box <- renderValueBox({valueBox(
    stu_totalincidents_box_fx(input), 
    "Total Incidents", 
    icon = icon("door-open"),
    color = "yellow"
  )})
  output$stu_totalminslost_box <- renderValueBox({valueBox(
    stu_totalminslost_box_fx(input), 
    "Total Minutes Lost", 
    icon = icon("stopwatch"),
    color = "orange"
  )})
  output$stu_timelost_plot <- renderPlot(stu_timelost_plot_fx(input)) #stu_timelost_plot
  #### new  
  output$stu_timelost_plot_dl <- downloadplot_stu_fx("Minutes_in_Room_by_Date",
                                                   input, 
                                                   stu_timelost_plot_fx,
                                                   input$student_name,
                                                   input$stu_daterange) ##############################
  output$stu_timelost_table <- renderDT(stu_timelost_table_fx(input)) 
  output$stu_fidelity_plot <- renderPlot(stu_fidelity_plot_fx(input)) #stu_fidelity_plot_dl
  #### new  
  output$stu_fidelity_plot_dl <- downloadplot_stu_fx("ISLA_Fidelity",
                                                     input, 
                                                     stu_fidelity_plot_fx,
                                                     input$student_name,
                                                     input$stu_daterange) ##############################
  output$stu_fidelity_table <- renderDT(stu_fidelity_table_fx(input))
# Data
  output$completedata_table <- renderReactable(completedata_table)
}

shinyApp(ui, server)
