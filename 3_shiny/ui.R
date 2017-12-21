library(shinydashboard)
library(leaflet)
library(DT)

header <- dashboardHeader(
  title = "EvalTool ᵃˡᵖʰᵃ",titleWidth =200
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("General overview", tabName = "overview", icon = icon("dashboard")),
    menuItem("Barrio", tabName = "none", icon = icon("home"),
    menuSubItem("Graphical", tabName = "dashboard", icon = icon("bar-chart-o")),
    menuSubItem("Table", tabName = "widgets", icon = icon("th")),
    menuSubItem("Clustering", tabName = "clustering", icon = icon("object-group"))),
    menuItem("Context", tabName = "none", icon = icon("home"),
    menuSubItem("Graphical", tabName = "dashboard2", icon = icon("bar-chart-o")),
    menuSubItem("Table", tabName = "widgets2", icon = icon("th")),
    menuSubItem("Tests", tabName = "tests1", icon = icon("flask"))),
    menuItem("City", tabName = "none", icon = icon("home"),
    menuSubItem("Graphical", tabName = "dashboard3", icon = icon("bar-chart-o")),
    menuSubItem("Table", tabName = "widgets3", icon = icon("th")),
    menuSubItem("Tests", tabName = "tests2", icon = icon("flask")))
             
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
  fluidRow(div(style = "height:0px", ""),column(12,h2("Graphical barrio overview")),
    column(6,div(style = "height:25px", ""),uiOutput("Variable")),
    column(6,div(style = "height:25px", ""),uiOutput("Barrio")),
    column(width = 4, plotOutput("plot1", height=300, width = "100%")),
    column(width = 4, plotOutput("plot2", height=300, width = "100%")),
    column(width = 4, plotOutput("plot3", height=300, width = "100%"))
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
                 column(12,h2("Barrio table overview"),uiOutput("Variable2"),
                 div(style = "height:25px", ""),
                 DT::dataTableOutput('tbl'))
        )
),
tabItem(tabName = "clustering",
        fluidRow(div(style = "height:0px", ""),
                 column(12,h2("Barrio table overview")),
                 column(12,div(style = "height:25px", ""),uiOutput("Variable7")),
                 column(width = 6, plotOutput("cluster1", height=400, width = "100%")),
                 column(width = 6, plotOutput("cluster2", height=400, width = "100%"))
        )
),
tabItem(tabName = "dashboard2",
        fluidRow(div(style = "height:0px", ""),column(12,h2("Graphical context overview")),
                 column(6,div(style = "height:25px", ""),uiOutput("Variable5")),
                 column(6,div(style = "height:25px", ""),uiOutput("Cities")),
                 column(width = 12, plotOutput("plot4", height=900, width = "100%"))
        )),
tabItem(tabName = "widgets2",
        fluidRow(div(style = "height:0px", ""),
                 column(12,h2("Context table overview"),uiOutput("Variable3"),
                        div(style = "height:25px", ""),
                        DT::dataTableOutput('tbl2'))
                 
        )
),
tabItem(tabName = "tests1",
        fluidRow(div(style = "height:0px", ""),
                 column(12,h2("Test overview"),
                 verbatimTextOutput("test1_resultBox")       
                 ))),
tabItem(tabName = "dashboard3",
        fluidRow(div(style = "height:0px", ""),column(12,h2("Graphical cities overview")),
                 column(6,div(style = "height:25px", ""),uiOutput("Variable6")),
                 column(width = 12, plotOutput("plot5", height=900, width = "100%"))
        )),
tabItem(tabName = "widgets3",
        fluidRow(div(style = "height:0px", ""),
                 column(12,h2("Cities table overview"),uiOutput("Variable4"),
                        div(style = "height:25px", ""),
                        DT::dataTableOutput('tbl3'))
                 
        )
),
tabItem(tabName = "tests2",
        fluidRow(div(style = "height:0px", ""),
                 column(12,h2("Test overview"),
                        verbatimTextOutput("test2_resultBox")       
                 )))
))

dashboardPage(
  skin = "purple",
  header,
  sidebar,
  body
)