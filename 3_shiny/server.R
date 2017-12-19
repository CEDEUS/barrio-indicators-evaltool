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


function(input, output, session) {
  
  output$Variable <- renderUI({
    variableList <- unique(dataset$bigname)
    selectInput("VariableSelection", "Variable", choices = variableList, selected = variableList[1],width = "100%")
  })
  
  output$Variable2 <- renderUI({
    variableList2 <- unique(dataset$bigname)
    selectInput("VariableSelection2", "Variable", choices = variableList2, selected = variableList2[1],width = "100%")
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
  	hist(var,xlab="", main=paste("Histogram of",input$VariableSelection))
   })
  
  output$plot2 <- renderPlot({
    qqnorm(var, col = varc)
    qqline(var,lty=2)
  })
  
  output$samplesizeBox <- renderValueBox({
    valueBox(
      length(var), "Units",
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
  
}