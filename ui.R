library(shinythemes)
library(DT)
library(shiny)
seite1 <- tabPanel("Data",
                   sidebarLayout(
                     sidebarPanel(
                       uiOutput("ui_plotwahl"),
                       uiOutput("ui_geschlechtwahl"),
                       uiOutput("ui_alterwahl"),
                       uiOutput("ui_rauchenwahl"),
                     ),
                     mainPanel(
                       tabsetPanel(type = "tabs",
                                   tabPanel("Chart", plotOutput("chart")),
                                   #   tabPanel("Chart2", plotOutput("chart2")),
                                   tabPanel("Tabelle", DTOutput("table")),
                       
                              tabPanel("Missing Data", plotOutput("missing_data"))),
                                  
                     )
                   ))


seite2 <- tabPanel("feature",
                  mainPanel(
                    tabsetPanel(type = "tabs",
                     tabPanel("DataExploration", DTOutput("DataExploration")), 
                     tabPanel("accuracy plot", plotOutput("accuracy_plot"))  )
                               )
                  )

                   
                   


#seite2 <- tabPanel("Seite 2",
            #       h1("TODO"))

ui <- navbarPage("Mein Dashboard",
                 theme = bslib::bs_theme(bootswatch = "superhero"),
                 seite1,
                 seite2
                 
)