library(shinydashboard)
library(dplyr)
library(tidyverse)
library(moments)
library(DT)
library(gridExtra)
library(dunn.test)
library(memoise)
library(purrrlyr)
library(broom)
library(Ckmeans.1d.dp)
library(ggrepel)

options(shiny.trace=F)

pformat <- function(p) {
  if (p == 1) {
    return("1.0000")
  }
  else {
    rightspaces <- 4
    rightdigits <- substr(paste0(sprintf("%1.4f", p), 
                                 "000000000"), 3, rightspaces + 2)
    return(paste0("0.", rightdigits))
  }
}

dataset <- read_csv("/Users/robsalasco/Dev/barrio-indicators-evaltool/data.csv")
all_manzanas_no_na <- dataset %>% filter(!is.na(listo))
all_manzanas_no_na$listo <- as.numeric(all_manzanas_no_na$listo)

table_vars_overview <- dataset %>%
  dplyr::group_by(codename, d) %>%
  dplyr::summarize(
    MEAN=round(mean(listo,na.rm = T),2),
    MEAN_BARRIO=round(mean(listo[manzb==T],na.rm = T),2),
    MEAN_BUFFER=round(mean(listo[manzb==F],na.rm = T),2),
    STD=round(sd(listo,na.rm = T),2),
    STD_BARRIO=round(sd(listo[manzb==T],na.rm = T),2),
    STD_BUFFER=round(sd(listo[manzb==F],na.rm = T),2),    
    UNITS_BARRIO=length(listo[manzb==T]),
    UNITS_BUFFER=length(listo[manzb==F])
  )

table_context_overview  <- dataset %>%
  dplyr::group_by(codename, LOCATION, CIUDAD) %>%
  dplyr::summarize(
    MEAN=round(mean(listo,na.rm = T),2),
    MEAN_BARRIO=round(mean(listo[manzb==T],na.rm = T),2),
    MEAN_BUFFER=round(mean(listo[manzb==F],na.rm = T),2),
    STD=round(sd(listo,na.rm = T),2),
    STD_BARRIO=round(sd(listo[manzb==T],na.rm = T),2),
    STD_BUFFER=round(sd(listo[manzb==F],na.rm = T),2),    
    UNITS_BARRIO=length(listo[manzb==T]),
    UNITS_BUFFER=length(listo[manzb==F])
  )

table_cities_overview  <- dataset %>%
  dplyr::group_by(codename, CIUDAD) %>%
  dplyr::summarize(
    MEAN=round(mean(listo,na.rm = T),2),
    MEAN_BARRIO=round(mean(listo[manzb==T],na.rm = T),2),
    MEAN_BUFFER=round(mean(listo[manzb==F],na.rm = T),2),
    STD=round(sd(listo,na.rm = T),2),
    STD_BARRIO=round(sd(listo[manzb==T],na.rm = T),2),
    STD_BUFFER=round(sd(listo[manzb==F],na.rm = T),2),    
    UNITS_BARRIO=length(listo[manzb==T]),
    UNITS_BUFFER=length(listo[manzb==F])
  )

#######################################################

test1 <- dataset %>% filter(!is.na(listo))
vars_test1 <- unique(test1$codename)

stage_2_nonormal_test <- test1 %>% 
  slice_rows(c("codename")) %>% 
  by_slice(~tidy(with(.x,kruskal.test(listo~factor(LOCATION))))) %>%
  unnest() %>% select(codename,p.value,method)

dunn.dt.loc <- function() { bind_rows(lapply(vars_test1, function(x) {
  log <- capture.output({
    dunn <-
      dunn.test(
        subset(test1, codename == x)$listo,
        factor(subset(test1, codename == x)$LOCATION),
        list = T,
        table = F,
        kw = F
      )
  })
  return(data.frame(
    var = rep(unique(subset(
      test1, codename == x
    )$codename), length(dunn$comparisons)),
    contexto = dunn$comparisons,
    p.value = pformat(dunn$P.adjusted)
  ))
}))}

mdunn.dt.loc <- memoise(dunn.dt.loc)

dunn.dt.cit <- function() { bind_rows(lapply(vars_test1, function(x) {
  log <- capture.output({
    dunn <-
      dunn.test(
        subset(test1, codename == x)$listo,
        factor(subset(test1, codename == x)$CIUDAD),
        list = T,
        table = F,
        kw = F
      )
  })
  return(data.frame(
    var = rep(unique(subset(
      test1, codename == x
    )$codename), length(dunn$comparisons)),
    contexto = dunn$comparisons,
    p.value = pformat(dunn$P.adjusted)
  ))
}))}

mdunn.dt.cit <- memoise(dunn.dt.cit)
#################################

function(input, output, session) {
  
  output$test1_resultBox <- renderPrint({
    mdunn.dt.loc()
  })
  
  output$test2_resultBox <- renderPrint({
    mdunn.dt.cit()
  })
  
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
  
  output$Variable4 <- renderUI({
    variableList4 <- unique(dataset$bigname)
    selectInput("VariableSelection4", "Variable", choices = variableList4, selected = variableList4[1],width = "100%")
  })
  
  output$Variable5 <- renderUI({
    variableList5 <- unique(dataset$bigname)
    selectInput("VariableSelection5", "Variable", choices = variableList5, selected = variableList5[1],width = "100%")
  })
  
  output$Variable6 <- renderUI({
    variableList6 <- unique(dataset$bigname)
    selectInput("VariableSelection6", "Variable", choices = variableList6, selected = variableList6[1],width = "100%")
  })
  
  output$Variable7 <- renderUI({
    variableList7 <- unique(dataset$bigname)
    selectInput("VariableSelection7", "Variable", choices = variableList7, selected = variableList7[1],width = "100%")
  })
  
  
  output$Barrio <- renderUI({
    barrioList <- unique(dataset$d)
    selectInput("BarrioSelection", "Barrio", choices = barrioList, selected = barrioList[1],width = "100%")
  })
  
  output$Cities <- renderUI({
    citiesList <- unique(dataset$CIUDAD)
    selectInput("CitySelection", "City", choices = citiesList, selected = citiesList[1],width = "100%")
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
  
  output$plot1 <- renderPlot({
    ggplot((all_manzanas_no_na %>% filter(codename == varcodename & d == barrioname)), aes(listo, fill = manzb)) +
      geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') +
      labs(fill="barrio") + 
      ggtitle(label="Histogram")
  })
  
  output$plot2 <- renderPlot({
    ggplot((all_manzanas_no_na %>% filter(codename == varcodename & d == barrioname)), aes(sample = listo, colour = manzb)) +
      stat_qq() + labs(colour="barrio") + ggtitle(label ="Q-Q Plot")
  })

  output$plot3 <- renderPlot({
    fun_mean <- function(x){
      return(data.frame(y=round(mean(x),2),label=paste0("M= ",round(mean(x,na.rm=T),2),"; ","SD= ",round(sd(x,na.rm=T),2),"; ","N= ",length(x))))}
    
    ggplot((all_manzanas_no_na %>% filter(codename == varcodename & d == barrioname)), aes(x = manzb, y = listo)) +
      geom_boxplot() + labs(x="barrio",y="value") + ggtitle(label ="Boxplot") +
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
    datatable(filter(table_vars_overview, codename == varcodename2), extensions = 'FixedColumns',
              options = list(dom = 'tp',
                             scrollX = TRUE,
                             fixedColumns = FALSE))
  )
  })
  
  observeEvent({c(input$VariableSelection3)},{
    varcodename3 <- as.character(all_manzanas_no_na[match(input$VariableSelection3,all_manzanas_no_na$bigname),]$codename)
    output$tbl2 = DT::renderDataTable(
      datatable(filter(table_context_overview, codename == varcodename3), extensions = 'FixedColumns',
                options = list(dom = 'tp',
                               scrollX = TRUE,
                               fixedColumns = FALSE))
    )
  })
  
  observeEvent({c(input$VariableSelection4)},{
    varcodename4 <- as.character(all_manzanas_no_na[match(input$VariableSelection4,all_manzanas_no_na$bigname),]$codename)
    output$tbl3 = DT::renderDataTable(
      datatable(filter(table_cities_overview, codename == varcodename4), extensions = 'FixedColumns',
                options = list(dom = 'tp',
                               scrollX = TRUE,
                               fixedColumns = FALSE))
    )
  })
  
  observeEvent({c(input$VariableSelection5,input$CitySelection)},{  
    req(input$VariableSelection5)
    req(input$CitySelection)
    
    varcodename <- as.character(all_manzanas_no_na[match(input$VariableSelection5,all_manzanas_no_na$bigname),]$codename)
  
    cityname <- as.character(input$CitySelection)
    
    output$plot4 <- renderPlot({
      fun_mean <- function(x){
        return(data.frame(y=round(mean(x),2),label=paste0("M= ",round(mean(x,na.rm=T),2),"; ","SD= ",round(sd(x,na.rm=T),2),"; ","N= ",length(x))))}
      
      p1 <- ggplot((all_manzanas_no_na %>% filter(codename == varcodename & CIUDAD == cityname)), aes(x = LOCATION, y = listo)) +
        geom_boxplot() + labs(x="",y="") + ggtitle(label ="Boxplot general") +
        stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
        stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)
      
      p2 <- ggplot((all_manzanas_no_na %>% filter(codename == varcodename & CIUDAD == cityname & manzb == T )), aes(x = LOCATION, y = listo)) +
        geom_boxplot() + labs(x="",y="") + ggtitle(label ="Boxplot barrio") +
        stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
        stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)
      
      p3 <- ggplot((all_manzanas_no_na %>% filter(codename == varcodename & CIUDAD == cityname & manzb == F )), aes(x = LOCATION, y = listo)) +
        geom_boxplot() + labs(x="",y="") + ggtitle(label ="Boxplot buffer") +
        stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
        stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)
      
      grid.arrange(p1,p2,p3, ncol=1)
      
    }) 
    
  })

  observeEvent({c(input$VariableSelection6)},{
    req(input$VariableSelection6)

    varcodename <- as.character(all_manzanas_no_na[match(input$VariableSelection6,all_manzanas_no_na$bigname),]$codename)
    
    output$plot5 <- renderPlot({
      fun_mean <- function(x){
        return(data.frame(y=round(mean(x),2),label=paste0("M= ",round(mean(x,na.rm=T),2),"; ","SD= ",round(sd(x,na.rm=T),2),"; ","N= ",length(x))))}
      
      p1 <- ggplot((all_manzanas_no_na %>% filter(codename == varcodename)), aes(x = CIUDAD, y = listo)) +
        geom_boxplot() + labs(x="",y="") + ggtitle(label ="Boxplot general") +
        stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
        stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)
      
      p2 <- ggplot((all_manzanas_no_na %>% filter(codename == varcodename & manzb==T)), aes(x = CIUDAD, y = listo)) +
        geom_boxplot() + labs(x="",y="") + ggtitle(label ="Boxplot barrio") +
        stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
        stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)
      
      p3 <- ggplot((all_manzanas_no_na %>% filter(codename == varcodename & manzb==F)), aes(x = CIUDAD, y = listo)) +
        geom_boxplot() + labs(x="",y="") + ggtitle(label ="Boxplot buffer") +
        stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
        stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)
      
      grid.arrange(p1,p2,p3, ncol=1)
      
    })
    
  }) 
  
  observeEvent({c(input$VariableSelection7)},{
    req(input$VariableSelection7)
    varcodename <- as.character(all_manzanas_no_na[match(input$VariableSelection7,all_manzanas_no_na$bigname),]$codename)
    
    data1 <-  dataset %>% filter(codename==varcodename & manzb==T) %>% select(listo,d) %>% group_by(d) %>% dplyr::summarize(x=mean(listo,na.rm = T))
    data1$x <- round(data1$x,2)
    data1 <- data1 %>% filter(!is.na(x))
    
    result1 <- Ckmeans.1d.dp(data1$x, 3)
    
    data2 <-  dataset %>% filter(codename==varcodename & manzb==F) %>% select(listo,d) %>% group_by(d) %>% dplyr::summarize(x=mean(listo,na.rm = T))
    data2$x <- round(data2$x,2)
    data2 <- data2 %>% filter(!is.na(x))
    
    result2 <- Ckmeans.1d.dp(data2$x, 3)
    
    output$cluster1 <- renderPlot({
      ggplot(data = data1, aes(x = 1:nrow(data1), y = x, colour=as.factor(result1$cluster))) +
        geom_hline(yintercept=round(result1$centers,2), linetype="dashed", color = "black") +
        geom_point() + geom_text_repel(aes(label=d),size = 3)+
        labs(x = "index",colour="cluster",y="value") +
        scale_y_continuous(breaks = sort(c(seq(min(data1$x), max(data1$x), length.out=2), round(result1$centers,2)))) + ggtitle("Barrio")
    })
    
    
    
    output$cluster2 <- renderPlot({
      ggplot(data = data2, aes(x = 1:nrow(data2), y = x, colour=as.factor(result2$cluster))) +
        geom_hline(yintercept=round(result2$centers,2), linetype="dashed", color = "black") +
        geom_point() + geom_text_repel(aes(label=d),size = 3)+
        labs(x = "index",colour="cluster",y="value") +
        scale_y_continuous(breaks = sort(c(seq(min(data2$x), max(data2$x), length.out=2), round(result2$centers,2)))) + ggtitle("Buffer")
    })
  })
}