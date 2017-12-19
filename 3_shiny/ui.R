library(shinydashboard)
library(leaflet)
library(DT)

header <- dashboardHeader(
  title = "EvalTool ᵃˡᵖʰᵃ",titleWidth =200
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("General overview", tabName = "overview", icon = icon("dashboard")),
    menuItem("Barrio", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Variable", tabName = "widgets", icon = icon("th"))
  )
)

body <- dashboardBody(tabItems(
  tabItem(tabName = "overview",
          fluidRow(div(style = "height:0px", ""),
                   column(12,h2("General overview"))),
          fluidRow(div(style = "height:25px", ""),
                   valueBoxOutput("barriosnBox",width = 4),
                   valueBoxOutput("ciudadesnBox",width = 4),
                   valueBoxOutput("varsnBox",width = 4))
          ),
  tabItem(tabName = "dashboard",
  fluidRow(div(style = "height:0px", ""),column(12,h2("Barrio overview")),
    column(6,div(style = "height:25px", ""),uiOutput("Variable")),
    column(6,div(style = "height:25px", ""),uiOutput("Barrio")),
    column(width = 6, plotOutput("plot1", height=300, width = "100%")),
    column(width = 6, plotOutput("plot2", height=300, width = "100%"))
),
fluidRow(div(style = "height:25px", ""),
         valueBoxOutput("samplesizeBox",width = 4),
         valueBoxOutput("skewnessBox",width = 4),
         valueBoxOutput("kurtosisBox",width = 4),
         valueBoxOutput("medianBox",width = 4),
         valueBoxOutput("meanBox",width = 4),
         valueBoxOutput("varianceBox",width = 4))
),
tabItem(tabName = "widgets",
        fluidRow(div(style = "height:0px", ""),
                 column(12,h2("Variable overview"),uiOutput("Variable2"),
                 div(style = "height:25px", ""),
                 DT::dataTableOutput('tbl'))
                 
        )
)
))

dashboardPage(
  skin = "purple",
  header,
  sidebar,
  body
)