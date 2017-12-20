library(shinydashboard)
library(dplyr)
library(tidyverse)
library(moments)
library(DT)

options(shiny.trace=F)


dataset <- read_csv("/Users/robsalasco/Dev/barrio-indicators-evaltool/data.csv")
all_manzanas_no_na <- dataset %>% filter(!is.na(listo))
all_manzanas_no_na$listo <- as.numeric(all_manzanas_no_na$listo)

table_vars_overview <- dataset %>%
  dplyr::group_by(codename, d) %>%
  dplyr::summarize(
    MEAN_BARRIO=mean(listo[manzb==T],na.rm = T),
    MEAN_BUFFER=mean(listo[manzb==F],na.rm = T),
    STD_BARRIO=sd(listo[manzb==T],na.rm = T),
    STD_BUFFER=sd(listo[manzb==F],na.rm = T),    
    UNITS_BARRIO=length(listo[manzb==T]),
    UNITS_BUFFER=length(listo[manzb==F])
  )

table_context_overview  <- dataset %>%
  dplyr::group_by(codename, LOCATION) %>%
  dplyr::summarize(
    MEAN_BARRIO=mean(listo[manzb==T],na.rm = T),
    MEAN_BUFFER=mean(listo[manzb==F],na.rm = T),
    STD_BARRIO=sd(listo[manzb==T],na.rm = T),
    STD_BUFFER=sd(listo[manzb==F],na.rm = T),    
    UNITS_BARRIO=length(listo[manzb==T]),
    UNITS_BUFFER=length(listo[manzb==F])
  )


function(input, output, session) {
  
  output$Variable <- renderUI({
    variableList <- unique(dataset$bigname)
    selectInput("VariableSelection", "Variable", choices = variableList, selected = variableList[1],width = "100%")
  })
  
  output$Variable2 <- renderUI({
    variableList2 <- unique(dataset$bigname)
    selectInput("VariableSelection2", "Variable", choices = variableList2, selected = variableList2[1],width = "100%")
  })
  
  output$Variable3 <- renderUI({
    variableList3 <- unique(dataset$bigname)
    selectInput("VariableSelection3", "Variable", choices = variableList3, selected = variableList3[1],width = "100%")
  })
  
  output$Barrio <- renderUI({
    barrioList <- unique(dataset$d)
    selectInput("BarrioSelection", "Barrio", choices = barrioList, selected = barrioList[1],width = "100%")
  })
  
  output$barriosnBox <- renderValueBox({
    valueBox(
      length(unique(dataset$d)), "Barrios",
      color = "aqua"
    )})
  
  output$ciudadesnBox <- renderValueBox({
    valueBox(
      length(unique(dataset$CIUDAD)), "Ciudades",
      color = "green"
    )}) 
  
  output$varsnBox <- renderValueBox({
    valueBox(
      length(unique(dataset$codename)), "Variables",
      color = "blue"
    )})
  

  
  
  observeEvent({c(input$VariableSelection,input$BarrioSelection)},{  
  req(input$VariableSelection)
  req(input$BarrioSelection)
  
  varcodename <- as.character(all_manzanas_no_na[match(input$VariableSelection,all_manzanas_no_na$bigname),]$codename)
  
  barrioname <- as.character(input$BarrioSelection)
  
  var <- (all_manzanas_no_na %>% filter(codename == varcodename & d == barrioname) %>% select(listo))$listo
  varc <- (all_manzanas_no_na %>% filter(codename == varcodename & d == barrioname) %>% select(manzb))$manzb
  varc[varc==T] <- 2
  varc[varc==F] <- 1
  
  output$plot1 <- renderPlot({
    ggplot((all_manzanas_no_na %>% filter(codename == varcodename & d == barrioname)), aes(listo, fill = manzb)) +
      geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') +
      labs(fill="Barrio") + 
      ggtitle(label="Histogram")
  })
  
  output$plot2 <- renderPlot({
    ggplot((all_manzanas_no_na %>% filter(codename == varcodename & d == barrioname)), aes(sample = listo, colour = manzb)) +
      stat_qq() + labs(colour="Barrio") + ggtitle(label ="Q-Q Plot")
  })

  output$plot3 <- renderPlot({
    fun_mean <- function(x){
      return(data.frame(y=round(mean(x),2),label=paste0("M= ",round(mean(x,na.rm=T),2),"; ","N= ",length(x))))}
    
    ggplot((all_manzanas_no_na %>% filter(codename == varcodename & d == barrioname)), aes(x = manzb, y = listo)) +
      geom_boxplot() + labs(x="Barrio",y="Value") + ggtitle(label ="Boxplot") +
      stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
      stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)
  })    
  
  output$samplesizeBox <- renderValueBox({
    valueBox(
      length(var), "Total spatial units",
      color = "aqua"
    )})
  
  output$skewnessBox <- renderValueBox({
    valueBox(
      round(skewness(var),digits = 2), "Skewness",
      color = "red"
    )})
  
  output$kurtosisBox <- renderValueBox({
    valueBox(
      round(kurtosis(var),digits = 2), "Kurtosis",
      color = "yellow"
    )})

  output$medianBox <- renderValueBox({
    valueBox(
      round(median(var),digits = 2), "Median",
      color = "green"
    )})
  
  output$meanBox <- renderValueBox({
    valueBox(
      round(mean(var),digits = 2), "Mean",
      color = "navy"
    )})
  
  output$varianceBox <- renderValueBox({
    valueBox(
      round(sd(var),digits = 2), "Standard deviation",
      color = "maroon"
    )})
  
  })

  observeEvent({c(input$VariableSelection2)},{
  varcodename2 <- as.character(all_manzanas_no_na[match(input$VariableSelection2,all_manzanas_no_na$bigname),]$codename)
  output$tbl = DT::renderDataTable(
    datatable(filter(table_vars_overview, codename == varcodename2))
  )
  })
  
  observeEvent({c(input$VariableSelection3)},{
    varcodename3 <- as.character(all_manzanas_no_na[match(input$VariableSelection3,all_manzanas_no_na$bigname),]$codename)
    output$tbl2 = DT::renderDataTable(
      datatable(filter(table_context_overview, codename == varcodename3))
    )
  })
  
}