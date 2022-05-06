library(quantmod)
library(ggplot2)
library(dplyr)
library(VIM) 
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(shinythemes)


## Daten von Yahoo Finance holen ######################################################################################
#getSymbols(symbols, from = start_datum, to = end_datum)
#df <- data.frame(Symbol=character(),
#                 Datum=as.Date(character()),
#                 Open=numeric(),
#                 High=numeric(),
#                 Low=numeric(),
#                 Close=numeric(),
#                 Volume=numeric(),
#                 Adjusted=numeric())
#for (s in symbols) {
#  temp <- data.frame(Symbol=s, Datum=index(get(s)), get(s))
#  colnames(temp) <- c("Symbol","Datum","Open","High","Low","Close","Volume","Adjusted")
#  df <- rbind(df,temp)
#}



#### Load data 

workPath <- "C:\\MySea\\Seafile\\DataMining-WS2122\\01_cardio\\Projekt\\"
cardio_raw= read.csv( paste (workpath, "train.csv", sep = ""))
#cardio_raw= read.csv("D:\\DataMining\\train.csv")
cardio <- cardio_raw[ , -1 ]


cardio$education <- factor(cardio$education)

cardio$sex <- ifelse (cardio$sex == "F", "female", "male")

cardio$is_smoking <- ifelse (cardio$is_smoking == "YES", "smoking", "not smoking")

colnames(cardio)[4] <- "smoking" 

colnames(cardio)[6] <- "BloodPresMed" 

cardio$BloodPresMed <- ifelse (cardio$BloodPresMed == 0, "no", "yes")

cardio$prevalentStroke <- ifelse (cardio$prevalentStroke == 0, "no stroke", "stroke")
colnames(cardio)[7] <- "stroke" 

cardio$TenYearCHD <- ifelse (cardio$TenYearCHD == 0, "healthy", "CHD")

colnames(cardio)[8] <- "hypertensive" 
cardio$hypertensive <- ifelse (cardio$hypertensive == 0, "no hypertensive", "hypertensive")

cardio$diabetes <- ifelse (cardio$diabetes == 0, "no diabetes", "diabetes")

colnames(cardio)[ncol(cardio)] <- "target" 

## hilfsdatensätze
cardio_chd = subset(cardio,target == "CHD" )
cardio_healthy = subset(cardio,target == "healthy" )

## 
plots_auswahl_liste <- c("boxplot", "histogram")
geschlechtwahl_auswahl_liste<- c("mänlich", "weiblich")



filterData <- function (input, p_cardio)  {
  #Geschlecht
  cardioPlot <- p_cardio
  if (input$geschlechtwahl == 1) {
    cardioPlot <- subset(cardioPlot, sex =="male" )
  }else if (input$geschlechtwahl == 2)  {
    cardioPlot <- subset(cardioPlot, sex =="female" )
  }
  
  
  #Alter
  if (input$alterwahl == 1) {
    cardioPlot <- subset(cardioPlot, age >= 0 & age <20 )
  }else if (input$alterwahl == 2) {
    cardioPlot <- subset(cardioPlot, age >= 20 & age <40 )
  }else if (input$alterwahl == 3) {
    cardioPlot <- subset(cardioPlot, age >= 40 & age <60 )
  }else if (input$alterwahl == 4) {
    cardioPlot <- subset(cardioPlot, age >= 60 & age <99 )
  }
  
  
  #Rauchen
  if (input$rauchenwahl == 1) {
    cardioPlot <- subset(cardioPlot, smoking =="smoking" )
  }else if (input$rauchenwahl == 2)  {
    cardioPlot <- subset(cardioPlot, smoking =="not smoking" )
  }
  
  
  return (cardioPlot)
}



## Die Server-Funktion ################################################################################################
server <- function(input, output) {
  # dateRangeInput benutzt Variablen aus server.R, daher hier über renderUI statt in der ui.R definiert
  
  
  output$ui_plotwahl <- renderUI(
    #checkboxGroupInput("plotwahl", 
    #                   label="Plotauswahl:",
    #                   plots_auswahl_liste,
    #                   selected=plots_auswahl_liste[1])
    radioButtons("plotwahl", h3("Plotauswahl"),
                 choices = list("boxplot" = 1, "histogram" = 2 , "Frequency" = 3),selected = 1)
  )
  
  output$ui_geschlechtwahl <- renderUI(
    #checkboxGroupInput("plotwahl", 
    #                   label="Plotauswahl:",
    #                   plots_auswahl_liste,
    #                   selected=plots_auswahl_liste[1])
    radioButtons("geschlechtwahl", h3("Geschlechtauswahl"),
                 choices = list("mänlich" = 1, "weiblich" = 2, "beides" = 3),selected = 3)
  )
  
  
  
  output$ui_alterwahl <- renderUI(
    #checkboxGroupInput("plotwahl", 
    #                   label="Plotauswahl:",
    #                   plots_auswahl_liste,
    #                   selected=plots_auswahl_liste[1])
    radioButtons("alterwahl", h3("Alterauswahl"),
                 choices = list("[0-20]" = 1, "[20-40]" = 2, "[40-60]" = 3 , "[60-99]" = 4, "alle" = 5),selected = 5)
    
    #  sliderInput("alterwahl", h3("Alterauswahl"),
    #              min = 0, max = 100, value = 50),
    
  )
  output$ui_rauchenwahl <- renderUI(
    #checkboxGroupInput("plotwahl", 
    #                   label="Plotauswahl:",
    #                   plots_auswahl_liste,
    #                   selected=plots_auswahl_liste[1])
    radioButtons("rauchenwahl", h3("Rauchenauswahl"),
                 choices = list("Yes" = 1, "No" = 2, "beides" = 3),selected = 3)
  )
  
  
  # Das Liniendiagramm mit Filter abhÃ¤ngig von den Inputs
 
  
  output$chart <- renderPlot({
    cardioPlot <- filterData (input, cardio)  ;
    
    if (input$plotwahl == 1) {
      cardioPlot %>%
        ggplot( aes(x = target, y= age, fill = target)) + 
        geom_boxplot()+
        theme(text = element_text(size=16)) +
        labs ( title = "Boxplot vs. Target: Age") +
        xlab ("Zielvariable") +
        ylab ("Alter")
    }
    
    else if (input$plotwahl == 3) {
      cardioPlot %>%
  
        ggplot( aes(x = age) ) + 
        geom_histogram(binwidth =4)+
        theme(text = element_text(size=14)) +
        labs ( title = "Frequency: Age" )  +
        xlab ("Age") +
        ylab  ("count")
      

    }
    
 
    else if (input$plotwahl == 2) {
      cardioPlot %>%
        ggplot( aes(x = age, fill = target)) + 
        geom_histogram(binwidth =2)+
        facet_wrap(~ target) + 
        theme(text = element_text(size=16)) +
        labs ( title = "Frequency Histogram: Age vs Target" ) +
        xlab ("Age") +
        ylab ("count")
      
    }
    
  })
  
  
  output$missing_data <- renderPlot({
    cardioPlot <- filterData (input, cardio)  ;
    aggr(cardioPlot)
  })
  
  
  
  feature_summary = data.frame(feature = colnames(cardio)[1:ncol(cardio)-1])
  feature_summary$art = c("nominal","ordinal","cardinal","cardinal","nominal","cardinal","cardinal","cardinal","cardinal","nominal","nominal","nominal","nominal","nominal","nominal")
  for (ir in 1:nrow(feature_summary)) {
    tmp_col <- cardio[,feature_summary$feature[ir] ]
    feature_summary$nbr_missing[ir] <- nrow(cardio) -  table(is.na(tmp_col))["FALSE"]
  }
  feature_summary$p_value = c(1.84555411177536e-38,	6.68203706919536e-05 ,9.50443889414983e-07, 	0.0490648844448193,0.000373330337109478	, 	5.23410965432645e-06, 	0.000647033881655412 ,4.91775745429724e-21 ,	8.89527945660407e-12, 	5.18268946012133e-07	,	5.51533444666561e-24 ,	8.89527945660407e-12 ,	0.000414155694522985 ,	0.245536931489089,	3.66688738012339e-06)

    
  output$DataExploration <- renderDT({
    feature_summary%>%
      
      datatable(style = "bootstrap", options = list(pageLength = 20))
})
  
  output$accuracy_plot <- renderPlot({
    results<- readRDS(paste (workPath , "results.rds", sep =""))
    dotplot(results)
  })
  
  
#cardioPlot1 <- filterData(input, cardio)  ;
   # cardioPlot1 %>%
  #    datatable(style = "bootstrap", options = list(pageLength = 20))
 
        
#})
  
  # Datentablle als DT
  #output$table <- renderDT({
    #df %>% 
     # filter(Symbol %in% input$aktienwahl,
       #      Datum >= ifelse(length(input$datumswahl[1])==0,start_datum,input$datumswahl[1]),
       #      Datum <= ifelse(length(input$datumswahl[2])==0,end_datum,input$datumswahl[2])) %>% 
     # datatable(style = "bootstrap", options = list(pageLength = 20))
#  })#
  
  
  output$table <- renderDT({
 cardioPlot <- filterData (input, cardio)  ;
    cardioPlot %>%
      datatable(style = "bootstrap", options = list(pageLength = 20))
  })
  
  
  
  
  
  
  
}

