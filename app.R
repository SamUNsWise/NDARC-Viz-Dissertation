#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Author: Samuel Jones
# Purpose: Provide an interactive visualization platform for Drug Trends
# Date: 22/04/23

#----Installing packages---------------------------------------------------------

library(shiny)
library(shinydashboard)
library(readr)
library(tidyverse)
library(rgdal)
library(sp)
library(ggplot2)
library(dplyr)
library(tidyr)
library(janitor)
library(plotly)
library(shinycustomloader)
library(readxl)
library(haven)
library(tinytex)
library(officer)
library(DescTools)

#----Loading relevant data sets--------------------------------------------------------------------------------

load("death_2020.Rdata")
load("NHMD2021.Rdata")
IDRS <- readRDS("IDRS.rds")
EDRS <- readRDS("EDRS2.rds")
drugtypes.data <- readRDS("drugtypes.data.rds")
diagnosistable.data <- readRDS("diagnosistable.data.rds")
intenttable.data <- readRDS("intenttable.data.rds")
TreatmentGrandViz <- read_dta("TreatmentGrandViz.dta")
nams = names(NHMD)


#----UI content for each of the tabs in the dataset-------------------------------------------------------------

drug_use <- fluidRow(valueBoxOutput("useheader", width = 12), # Header
                     infoBoxOutput("sixmonthidrs", width = 3), # Infographics
                     infoBoxOutput("sixmonthedrs", width = 3),
                     infoBoxOutput("weeklyidrs", width = 3),
                     infoBoxOutput("weeklyedrs", width = 3),
                     box(title= "Summary Information", "This page contains data on drug use in Australia from various sources, including the Ecstasy and Related Drugs Reporting System (EDRS) and",
                         "Illicit Drug Reporting System (IDRS). Please note that these findings may not be representative of all people in Australia, nor of all people who use drugs. There can be",
                         "differences between data sources in how they capture each drug type. See the methods tab for further information and link to the source data.",
                         solidHeader = TRUE, collapsible = TRUE, width = 12),
                     tabBox(title = paste0("Six Month Use"), # Six month use plot
                            tabPanel("Plot", withLoader(plotlyOutput("SixMonthUse", width = "100%"), type="image", loader="DT_NIDIP_tween.gif")),
                            tabPanel("Notes", includeHTML("IDRS_EDRS.html"))),
                     tabBox(title = "Weekly Use", # Weekly use plot
                            tabPanel("Plot", withLoader(plotlyOutput("WeeklyUsePlot", width = "100%"), type="image", loader="DT_NIDIP_tween.gif")),
                            tabPanel("Notes", includeHTML("IDRS_EDRS.html"))
                            )
                     )

drug_harms <- fluidRow(valueBoxOutput("harmsheader", width = 12), 
                       infoBoxOutput("hosp_rate"),
                       infoBoxOutput("death_rate"),
                       infoBoxOutput("treat"),
                       box(title= "Summary Information", solidHeader = TRUE, collapsible = TRUE, width = 12,
                           "This page contains data on drug harms in Australia from various sources, including the National Hospital Morbidity Database (NHMD), Cause of Death Unit Record File (COD-URF) and", 
                           "The National Alcohol and Other Drug Treatment Services National Minimum Data Set (AODTS NMDS). Principal diagnosis was used to identify drug-induced deaths and hospitalisations.",
                           "Where 'All drugs' is depicted, it refers to the sum of all drugs within the datasets and includes drugs that are currently unavailable in isolation."),
                       tabBox(title = "Drug Related Hospitalisations",
                          tabPanel("Plot", withLoader(plotlyOutput("ByDrugPlot", width="100%"), type="image", loader="DT_NIDIP_tween.gif")),
                          tabPanel("Notes", includeHTML("notesByDrug.html")
                                    ),
                          tabPanel("ICD-10-AM", tableOutput('drugtypetable'))),
                       tabBox(title = "Drug Induced Deaths",
                          tabPanel("Plot", withLoader(plotlyOutput("JurPlot", width="100%"), type="image", loader="DT_NIDIP_tween.gif")),
                          tabPanel("Notes", includeHTML("fnoteDTJ.html")
                                   ),
                          tabPanel("ICD-10", includeHTML("notesDT.html"))),
                       tabBox(title = "Treatment Recipients", 
                              tabPanel("Plot", withLoader(plotlyOutput("treatment", width = "100%"), type ="image", loader="DT_NIDIP_tween.gif")),
                              tabPanel("Notes", "The first data were collected for 2000-01 as a pilot year. Data from 2001 onwards contains information about the rate at which alcohol and other drug treatment services are utilised within the Australian population.")))

drug_markets <- fluidRow(
                         infoBox("Hospitalisation rate", "254", "per 100,000 people", color = "red"),
                         infoBox("Sex", "values", "percent", color = "blue"),
                         infoBox("Rate in remote areas", "values", "per 100,000 people", color = "red"),
                         box(plotlyOutput("meth_plot", width = "100%", height = "400px"), width = 6))

#state_trend <- fluidRow(valueBoxOutput("strend", width = 12))

#----UI Sidebar Content--------------------------------------------------------------------------------------------------

ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Drug Trends App"), 
                    dashboardSidebar(
                      sidebarMenu(
                        # Setting id makes input$tabs give the tabName of currently-selected tab
                        id = "tabs",
                        width = 220,
                        style = "position:fixed;width:220px;",
                        menuItem("Drug Use", tabName = "tdrug", icon = icon("pills"), # Menu for the Drug Use Tab
                              menuSubItem("Drug Use Dashboard", tabName = "drguse"),
                              menuItem(
                                selectInput("udrug", "Drug:",
                                            c("Methamphetamine", # This list may be expanded in future, however these four are the most important.
                                              "Heroin",
                                              "Cannabis",
                                              "Cocaine"))),
                              menuItem(
                                selectInput("city", "Jurisdiction:",
                                            c("Australia",
                                              "Adelaide"="SA",
                                              "Brisbane"="QLD",
                                              "Canberra"="ACT",
                                              "Darwin"="NT",
                                              "Hobart"="TAS",
                                              "Melbourne"="VIC",
                                              "Perth"="WA",
                                              "Sydney"="NSW"))),
                              menuItem(
                                selectInput(
                                  "useCI", "Confidence Intervals:",
                                  c("Proportion" = "pr",
                                    "Proportion (95% CI)" = "prci"
                                  ),
                                  selected = "pr"
                                )),
                                menuItem(
                                  sliderInput("yr00", "Year",
                                              min=2000, max=2022, # EDRS data starts in 2003
                                              value = c(2000, 2022), sep="")),
                              
                              menuItem(
                                radioButtons('formatuse', 'Document format', c('HTML'), #, 'PDF', 'Word'),
                                           inline = TRUE)),
                              menuItem(
                                downloadButton("reportuse", "Download Report:"))),
                                  
#----Drug Harms sidebar menu-------------------------------------------------------------------------------
                        menuItem("Drug Harms", tabName = "hdrug", icon = icon("hospital"),
                                 menuSubItem("Drug Harms Dashboard", tabName = "drgharms"),
                                
                                 menuItem(
                                   selectInput("hdrug", "Drug:", # If the datasets had different menus it would be possible to include all drugs
                                             c("All drugs",
                                               "Amphetamine-type stimulants",
                                               "Cannabinoids",
                                               "Cocaine",
                                               "Heroin"))),
                                  menuItem(
                                    selectInput("jur", "Jurisdiction:",
                                                 c("Australia",
                                                   "Australian Capital Territory",
                                                   "New South Wales",
                                                   "Northern Territory",
                                                   "Queensland",
                                                   "South Australia",
                                                   "Tasmania*",
                                                   "Victoria",
                                                   "Western Australia"))),
                                  
                                 menuItem(
                                   selectInput(
                                     "plotByDT", "Plot:",
                                     c("Crude rate" = "cr",
                                       "Crude rate (95% CI)" = "crci",
                                       "Age-standardised rate" = "sr",
                                       "Age-standardised rate (95% CI)" = "srci"
                                     ),
                                     selected = "cr"
                                   )),
                                 
                                  menuItem(
                                    sliderInput("yr97", "Period",
                                                min=1997, max=2021,
                                                value=c(1997, 2021), sep="")),
                                 menuItem(
                                   radioButtons('formatharms', 'Document format', c('HTML'),#, 'PDF', 'Word'),
                                                inline = TRUE)),
                                 menuItem(
                                   downloadButton("reportharms", "Download Report:"))
                                 ),
                                               
                        menuItem("Drug Markets", tabName = "mdrug", icon = icon("money-bills"),
                                 menuSubItem("Drug Markets Dashboard")
                                      ),
                        menuItem("Additional resources", tabName = "notes", icon = icon("question"))
                        )),
              
                    
                    dashboardBody(
                      tags$head(
                        includeScript("google_analytics.js"),
                        tags$link(rel="icon", type="image/png", href="favicon.png"),
                        tags$style(HTML("img.loader-img{
                                         width: 50px;
                                         height: auto;
                                         }"))
                      ),
                      tabItems(
                        tabItem("drguse", drug_use),
                        tabItem("hdrug", "Drug Harms"),
                        tabItem("drgharms", drug_harms),
                        tabItem("usenotes", "Detailed notes on the data sources and potential limiations of the 
                                data will appear in this tab"),
                        tabItem("mdrug", drug_markets),
                        tabItem("notes", fluidRow(
                          column(width = 10, includeHTML("notesCitation.html"))))
                      ))
)

#---------------------Server code---------------------------------------------------------------------------------

server <- function(input, output) {

# Creating reactive functions so that a single menu can control different data sets at the same time
  
  selectedState <- reactive({
    input$jur
  })
  
  selectedYear <- reactive({
    input$yr97
  })
  
  selectedDrug <- reactive({
    input$hdrug
  })
  
#---Drug Use Tab------------------------------------------------------------------------------------------------------------------------------------------------------------------

  output$useheader <- renderValueBox({
    valueBox(paste(input$udrug), color = "purple", paste0(input$city))
  })
  
# Drug Use Infographics
  
  output$sixmonthidrs <- renderInfoBox({
    sub <- IDRS %>% subset((var_year>=input$yr00[1] & var_year<=input$yr00[2]) & istate == input$city & Drug == input$udrug & DrugType == "any") %>% arrange(var_year)
    infoBox("Any use past 6-months",
            paste0(tail(sub$est, n=1),"% in ", input$yr00[2]),
            "of people who regularly inject drugs (IDRS)",
            icon = icon("calendar-check"),
            color = "blue")
  })
  
  output$sixmonthedrs <- renderInfoBox({
    sub <- EDRS %>% subset((var_year>=input$yr00[1] & var_year<=input$yr00[2]) & istate == input$city & Drug == input$udrug & DrugType == "any") %>% arrange(var_year)
    infoBox("Any use past 6-months",
            paste0(tail(sub$est, n=1),"% in ", input$yr00[2]),
            "of people who regularly use ecstasy/other illicit stimulants (EDRS)",
            icon = icon("calendar-check"),
            color = "orange")
  })
  
  output$weeklyidrs <- renderInfoBox({
    sub <- IDRS %>% subset((var_year>=input$yr00[1] & var_year<=input$yr00[2]) & istate == input$city & Drug == input$udrug & DrugType == "weekly") %>% arrange(var_year)
    infoBox("Weekly+ use past 6 months",
            ifelse(!is.na(tail(sub$est, n=1)),paste0(tail(sub$est, n=1),"% in ", input$yr00[2]),"No data available"),
            "of people who regularly inject drugs (IDRS)",
            icon = icon("calendar-week"),
            color = "blue")
  })
  
  output$weeklyedrs <- renderInfoBox({
    sub <- EDRS %>% subset((var_year>=input$yr00[1] & var_year<=input$yr00[2]) & istate == input$city & Drug == input$udrug & DrugType == "weekly") %>% arrange(var_year)
    infoBox("Weekly+ use past 6 months",
            ifelse(!is.na(tail(sub$est, n=1)),paste0(tail(sub$est, n=1),"% in ", input$yr00[2]),"No data available"),
            "of people who regularly use ecstasy/other illicit stimulants (EDRS)",
            icon = icon("calendar-week"),
            color = "orange")
  })
  

  
#---Weekly drug use plot------------------------------------------------------------------------------------------------------
 
  # Multiple versions of these functions are necessary because 'any' and 'weekly' data is present in both datasets
  # These data sets were rebuilt to accommodate the confidence intervals
  
  IDRSwreactive <- reactive({
   IDRS <- IDRS %>% subset((var_year>=input$yr00[1] & var_year<=input$yr00[2]) & istate == input$city & Drug == input$udrug & DrugType == "weekly")
  })
  IDRSareactive <- reactive({
    IDRS <- IDRS %>% subset((var_year>=input$yr00[1] & var_year<=input$yr00[2]) & istate == input$city & Drug == input$udrug & DrugType == "any")
  })
  EDRSwreactive <- reactive({
   EDRS <- EDRS %>% subset((var_year>=input$yr00[1] & var_year<=input$yr00[2]) & istate == input$city & Drug == input$udrug & DrugType == "weekly")
  })
  EDRSareactive <- reactive({
    EDRS <- EDRS %>% subset((var_year>=input$yr00[1] & var_year<=input$yr00[2]) & istate == input$city & Drug == input$udrug & DrugType == "any")
  })
  
 output$WeeklyUsePlot <- renderPlotly({

   COLOURS <- c("IDRS" = "blue", "EDRS" = "orange")
   
   p <- ggplot() +
     geom_line(data = IDRSwreactive(), aes(x = var_year, y = est, colour = "IDRS")) +
     geom_point(data = IDRSwreactive(), aes(x = var_year, y = est, colour = "IDRS", text = paste0(
       "Year: ", var_year,
       "<br>Use: ",est,"%",
       "<br>Drug: ", Drug,
       "<br>State: ", istate
     )), size = 0.8, shape = 21) +
     geom_line(data = EDRSwreactive(), aes(x = var_year, y = est, colour = "EDRS")) +
     geom_point(data = EDRSwreactive(), aes(x = var_year, y = est, colour = "EDRS", text = paste0(
       "Year: ", var_year,
       "<br>Use: ",est,"%",
       "<br>Drug: ", Drug,
       "<br>State: ", istate
     )), size = 0.8, shape = 21) +
     scale_color_manual(values = COLOURS, name = "Data source") +
     labs(x = "Year",  y = "% Samples who regularly use drugs", colour = "Data source") +
     scale_x_continuous(breaks = c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021),
                        labels=c("2000", '2003', '2006', '2009', '2012', '2015', '2018', '2021')) +
     scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 10)) +
     theme(axis.text.x = element_text(angle = 35, hjust = 1), panel.grid.major.x = element_blank(),
           panel.grid.major.y = element_line(color = "gray90", linewidth = 0.2),
           panel.grid.minor.y = element_line(color = "gray90", linewidth = 0.2),
           panel.background = element_blank())
   if (input$useCI == "pr"){
     p <- p
   }
   else if (input$useCI == "prci"){
     p <- p + geom_ribbon(data = IDRSwreactive(),aes(y = est, x=var_year, ymin = lower, ymax = upper), alpha = 0.1, size = 0) +
       geom_ribbon(data = EDRSwreactive(), aes(y=est, x= var_year, ymin = lower, ymax = upper), alpha = 0.1, size = 0)
   }
   
   ggplotly(p, tooltip = "text") %>%
     add_annotations(
       text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/australian-drug-trends-2022-key-findings-national-illicit-drug-reporting-system-idrs">IDRS</a> <a href="https://ndarc.med.unsw.edu.au/resource/australian-drug-trends-2022-key-findings-national-ecstasy-and-related-drugs-reporting-system-edrs">EDRS</a>, NDARC',
       xref = "paper", yref = "paper",
       x = 0, xanchor = "left",
       y = 1.04, yanchor = "top",
       showarrow = F, font = list(size = 10, color = "grey")
     )
 })
  
#---Six Month Drug Use---------------------------------------------------------------------------------------------------------
  
  output$SixMonthUse <- renderPlotly({
    COLOURS <- c("IDRS" = "blue", "EDRS" = "orange")
    
    p <- ggplot() +
      geom_line(data = IDRSareactive(), aes(x = var_year, y = est, colour = "IDRS")) +
      geom_point(data = IDRSareactive(), aes(x = var_year, y = est, colour = "IDRS", text = paste0(
        "Year: ", var_year,
        "<br>Use: ",est,"%",
        "<br>Drug: ", Drug,
        "<br>State: ", istate
      )), size = 0.8, shape = 21) +
      geom_line(data = EDRSareactive(), aes(x = var_year, y = est, colour = "EDRS")) +
      geom_point(data = EDRSareactive(), aes(x = var_year, y = est, colour = "EDRS", text = paste0(
        "Year: ", var_year,
        "<br>Use: ",est,"%",
        "<br>Drug: ", Drug,
        "<br>State: ", istate
      )), size = 0.8, shape = 21) +
      scale_color_manual(values = COLOURS, name = "Data source") +
      labs(x = "Year",  y = "% Samples who regularly use drugs") +
      scale_x_continuous(breaks = c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021),
                         labels=c("2000", '2003', '2006', '2009', '2012', '2015', '2018', '2021')) +
      scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 10)) +
      theme(axis.text.x = element_text(angle = 35, hjust = 1), panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "gray90", linewidth = 0.2),
            panel.grid.minor.y = element_line(color = "gray90", linewidth = 0.2),
            panel.background = element_blank())
    if (input$useCI == "pr"){
      p <- p
    }
    else if (input$useCI == "prci"){
      p <- p + geom_ribbon(data = IDRSareactive(),aes(y = est, x=var_year, ymin = lower, ymax = upper), alpha = 0.1, size = 0) +
        geom_ribbon(data = EDRSareactive(), aes(y=est, x= var_year, ymin = lower, ymax = upper), alpha = 0.1, size = 0)
    }
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/australian-drug-trends-2022-key-findings-national-illicit-drug-reporting-system-idrs">IDRS</a> <a href="https://ndarc.med.unsw.edu.au/resource/australian-drug-trends-2022-key-findings-national-ecstasy-and-related-drugs-reporting-system-edrs">EDRS</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      )
  })
  

#---Drug Harms Tab----------------------------------------------------------------------------------------------------------------------------------------------------------------

  hospitalReactive <- reactive({
    sub <-  NHMD %>% subset(Reason=="Any" & Intent=="Any" & Age == "All ages" & jurisdiction == selectedState() & Drug == input$hdrug &
                              (year >= selectedYear()[[1]] & year <= selectedYear()[[2]])) %>% unite(AgeSex, c(Age, Sex), sep = ", ", remove = FALSE)
  })
  # Deaths data as the total for all drugs
  alldrugreactive <- reactive({
    alldrg <- COD2020_All %>% subset(jurisdiction== selectedState() &
                                       Age %in% "All ages" & Intent %in% "All" & Drug == selectedDrug() &
                                       (Year>= selectedYear()[1] & Year<=selectedYear()[2]) & Release=="Current") 
  })
  # Deaths data with drug breakdowns
  deathsReactive <- reactive({
    pd <- COD2020_DT %>% subset(jurisdiction== selectedState() &
                                   Age %in% "All ages" & Intent %in% "All" & Drug == selectedDrug() &
                                   (Year>= selectedYear()[1] & Year<=selectedYear()[2]) & Release=="Current")
  })
  
  # IF statements to regulate which data set is used for the deaths viz
  jurdata <- reactive({
    if (selectedDrug() %in% c("Heroin", "Cannabinoids", "Cocaine", "Amphetamine-type stimulants")) {
      jurdata <- deathsReactive()
    } else if (selectedDrug() == "All drugs") { # 'All drugs' is located in a different data set
      jurdata <- alldrugreactive()
    }
  })
  
  # Treatment data
  treatmentReactive <- reactive({ 
    sub <- TreatmentGrandViz %>% subset((Year_end>=input$yr97[1] & Year_end<=input$yr97[2]) & Location == input$jur & Drug == input$hdrug)
  })
  
# Creating the output sections for each interactive element in the app
  
# INFOGRAPHIC 1
  output$death_rate <- renderInfoBox({
    yr <- selectedYear()
    deaths_box <- jurdata() %>% arrange(Year) %>% subset(jurisdiction== selectedState() &
                    Age %in% "All ages" & Intent %in% "All" & Sex == "Total" &
                    (Year>=yr[[1]] & Year<=yr[[2]]) & Release=="Current" & Drug == selectedDrug())
    infoBox(
      "Death rate:",
      ifelse(!is.na(tail(deaths_box$cr, n=1)), paste0(round(tail(deaths_box$cr, n=1), digits = 1)," in ", input$yr97[2]),"No data available"),
      "per 100,000 people",
      icon = icon("line-chart"),
      color = "blue"
    )
  })

# INFOGRAPHIC 2
  output$hosp_rate <- renderInfoBox({
    hosp_box <- NHMD %>% arrange(year) %>% subset(Reason=="Any" & Intent=="Any" & Age == "All ages" & jurisdiction == selectedState() & Drug == selectedDrug() &
                                  (year >= selectedYear()[[1]] & year <= selectedYear()[[2]])) %>% unite(AgeSex, c(Age, Sex), sep = ", ", remove = FALSE)
    
    infoBox(
      "Hospitalisation rate:",
      ifelse(nrow(hosp_box) > 0, paste0(round(tail(hosp_box$cr, n=1), digits = 1)," in ", input$yr97[2]),"No data available"),
      "per 100,000 people",
      icon = icon("stethoscope"),
      color = "purple"
    )
  })

# INFOGRAPHIC 3
  output$treat <- renderInfoBox({
   
    infoBox(
      "Treatment rate:",
      ifelse(!is.na(tail(treatmentReactive()$cr, n=1)),paste0(round(tail(treatmentReactive()$cr, n=1), digits = 1)," in ",input$yr97[2]),"No data available"),
      "per 100,000 people",
      icon = icon("user-nurse"),
      color = "purple"
    )
    
  })

  
  output$harmsheader <- renderValueBox({
    valueBox(paste(input$jur), color = "purple", paste(input$hdrug))
  })
  
  # output$strend <- renderValueBox({
  #  valueBox(paste(input$jur), color = "red", "Weekly drug listings on Cryptomarkets between 22/10/2021 and 26/05/2022")
  # })

#---Hospitalization Plot in the Drug Harms Tab--------------------------------------------------------------------------------


   output$ByDrugPlot <- renderPlotly({

     p <- ggplot(hospitalReactive()) + aes(x = year, colour = Sex, linetype = Sex, group = 1) +
       geom_line() + geom_point(size=0.55) +
       labs(x = "Financial Year", colour = "", linetype = "") +
       scale_color_manual(values = c("red", "blue", "black")) +
       scale_linetype_manual(values = c("dashed", "dotdash", "solid")) +
       theme(panel.grid.major.x = element_blank(),
             panel.grid.major.y = element_line(color = "gray90", linewidth = 0.2),
             panel.grid.minor.y = element_line(color = "gray90", linewidth = 0.2),
             panel.background = element_blank()) +
       scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020),
                          labels=c("2000", '2005', '2010', '2015', '2020'))
     
    if (input$plotByDT == "cr" | input$plotByDT == "crci") {
       p <- p + aes(y = cr,
                    text = paste0(
                      "Year: ", year,
                      "<br>Location: ", jurisdiction,
                      "<br>Drug: ", Drug,
                      "<br>Hospitalisations: ", n,
                      "<br>Crude Rate: ", cr_p
                    )
       ) + scale_y_continuous(limits = c(0, max(hospitalReactive()$cr_uci, 2.5))) +
         labs(x = "Financial Year", y = "Crude Rate per 100,000")
       if (input$plotByDT == "crci") {
         
         p <- p + geom_ribbon(aes(ymin = cr_lci, ymax = cr_uci), alpha = 0.1, size = 0)# + scale_fill_manual(values=c("colour"="red"))
       }
     }
     
     else if (input$plotByDT == "sr" | input$plotByDT == "srci"  ) {
       p <- p + aes(y = sr,
                    text = paste0(
                      "Year: ", year,
                      "<br>Location: ", jurisdiction,
                      "<br>Drug: ", Drug,
                      "<br>Hospitalisations:", n,
                      "<br>Standardised Rate: ", sr_p
                    )
       ) + scale_y_continuous(limits = c(0, max(hospitalReactive()$sr_uci, 2.5))) +
         labs(x = "Financial Year", y = "Age-standardised Rate per 100,000")
       if (input$plotByDT == "srci") {
         p <- p + geom_ribbon(aes(ymin = sr_lci, ymax = sr_uci), alpha = 0.1, size = 0)
       }
       
     }
     
     validate(need(nrow(hospitalReactive()) > 0, "No data selected or data unavailable"))
     
     # Remove vertical gridlines
     p <- p + theme(panel.grid.minor.x = element_blank(),
                    panel.grid.major.x = element_blank())
     
     ggplotly(p, tooltip = "text") %>%
       add_annotations(
         text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource-analytics/trends-drug-related-hospitalisations-australia-1999-2021">DrugTrends</a>, NDARC',
         xref = "paper", yref = "paper",
         x = 0, xanchor = "left",
         y = 1.04, yanchor = "top",
         showarrow = F, font = list(size = 10, color = "grey")
       ) %>% layout(
         images = list(
           source = "DrugTrends-Logo-stacked.png",
           x = 0.01, xanchor = "left", y = .99, yanchor = "top",
           sizex = 0.07, sizey = 0.15,
           xref = "paper", yref = "paper",
           xanchor = "left", yanchor = "bottom"
         ))
   })
  
  # These are the notes and ICD-AM files for the Harms tab
  output$drugtypetable <- renderTable({drugtypes.data},sanitize.text.function=function(x){x})
  
  output$diagnosistable <- renderTable(diagnosistable.data)
  
  output$intenttable <- renderTable(intenttable.data)
  
#---------------Drug deaths plot by drug, jurisdiction, intent and sex ----------------------------------


jurdata2 <- reactive({jurdata})

  output$JurPlot <- renderPlotly({
    # Plot settings that are the same for all possible inputs
    
    gp <- ggplot(jurdata(), aes(x = Year, colour = Sex, linetype = Sex, group = 1, text = paste0(
      "Year: ", Year,
      "<br>Location: ", jurisdiction,
      "<br>Drug: ", Drug,
      "<br>Deaths: ", n,
      "<br>Crude Rate: ", cr_p
    ))) + geom_point(size=0.55) +
    labs(x = "Year", colour = "", linetype = "") + 
      scale_color_manual(values = c("red", "blue", "black")) +
      scale_linetype_manual(values = c("dashed", "dotdash", "solid")) +
      theme_light()
    
    # Crude rate per 100,000
    if (input$plotByDT == "cr" | input$plotByDT == "crci") {
      gp <- gp + geom_line() + 
        aes(y = cr) + scale_y_continuous(limits = c(0, max(jurdata()$cr_uci, 2.5))) +
        labs(x = "Year", y = "Crude Rate per 100,000")
      if (input$plotByDT == "crci") {
    # Adding confidence intervals if specified
        gp <- gp + geom_ribbon(aes(ymin = cr_lci, ymax = cr_uci), alpha = 0.1, size = 0)
      }
    }
    # Age-standardized rate per 100,000
    else if (input$plotByDT == "sr" | input$plotByDT == "srci"  ) {
      gp <- gp + geom_line() + 
        aes(y = sr) + scale_y_continuous(limits = c(0, max(jurdata()$sr_uci, 2.5))) +
        labs(x = "Year", y = "Age-standardised Rate per 100,000")
      if (input$plotByDT == "srci") {
        gp <- gp + geom_ribbon(aes(ymin = sr_lci, ymax = sr_uci), alpha = 0.1, size = 0)
      }
    }
    # Missing or low data

    gp <- gp + theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank())
    
    validate(need(!is.na(tail(jurdata()$cr, n=1)), "No data selected or data unavailable"))
    
    # Adding a link to the most recent paper and the source
    ggplotly(gp, tooltip="text") %>%
      
      add_annotations(
        text='Source: <a href="https://ndarc.med.unsw.edu.au/resource-analytics/trends-drug-induced-deaths-australia-1997-2020">DrugTrends</a>, NDARC',
        xref="paper", yref="paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      )
  })
  
  #---Treatment Visualization---------------------------------------------------------------------------
  
  output$treatment <- renderPlotly({

    gp <- ggplot(treatmentReactive()) + aes(x = Year_end, y = cr, colour = Sex, linetype = Sex, group = 1, 
                          text = paste0(
                            "Year ", Year_end,
                            "<br>Crude rate: ", round(cr, digits = 3),
                            "<br>Location: ", Location,
                            "<br>Drug: ", Drug
                          )) + 
      geom_point(size=0.55) +
      labs(x = "Year", colour = "", linetype="") + 
      scale_color_manual(values = c("red", "blue", "black")) +
      scale_linetype_manual(values = c("dashed", "dotdash", "solid")) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "gray90", linewidth = 0.2),
            panel.grid.minor.y = element_line(color = "gray90", linewidth = 0.2),
            panel.background = element_blank())
    # Crude rate per 100,000
    if (input$plotByDT == "cr"){
      gp <- gp + geom_line() + 
        scale_y_continuous(limits = c(0, max(treatmentReactive()$cr_uci, 2.5))) +
        labs(x = "Year", y = "Crude Rate per 100,000")
    }
    else if (input$plotByDT == "crci"){
      gp <- gp + geom_line() + geom_ribbon(aes(ymin = cr_lci, ymax = cr_uci), alpha = 0.1, size = 0) + 
        scale_y_continuous(limits = c(0, max(treatmentReactive()$cr_uci, 2.5))) +
        labs(x = "Year", y = "Crude Rate per 100,000")
    }
    ggplotly(gp, tooltip="text")

  })
  

#---Download button----------------------------------------------------------------------------------------

  output$reportuse <- downloadHandler(
    filename = function() {
      paste('drug_use_report', sep = '.', switch(
        input$formatuse, HTML = 'html' #, PDF = 'pdf', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('drug_use_report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'drug_use_report.Rmd', overwrite = TRUE)
      params <- list(yr00 = input$yr00,
                     city = input$city,
                     udrug = input$udrug,
                     IDRSw = IDRSwreactive(),
                     IDRSa = IDRSareactive(),
                     EDRSa = EDRSareactive(),
                     EDRSw = EDRSwreactive())
      library(rmarkdown)
      out <- render('drug_use_report.Rmd', switch(
        input$formatuse,
        HTML = html_document()), #, PDF = pdf_document(), Word = word_document()),
        params = params,
        envir = new.env(parent = globalenv()))
      file.rename(out, file)
    }
  )

#---Drug Harms Downloadable Report------------------------------------------------------------------------------------------------------------  
  
  output$reportharms <- downloadHandler(
    filename = function() {
      paste('drug_harms_report', sep = '.', switch(
        input$formatharms, HTML = 'html'#, Word = 'docx', PDF = 'pdf' 
      ))
    },
    
    content = function(file) {
      src <- normalizePath('drug_harms_report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'drug_harms_report.Rmd', overwrite = TRUE)
      params <- list(yr97 = input$yr97,
                     jur = input$jur,
                     hdrug = input$hdrug,
                     jurdata = jurdata(),
                     treatment = treatmentReactive(),
                     hospital = hospitalReactive())
      library(rmarkdown)
      out <- render('drug_harms_report.Rmd', switch(
        input$formatharms,
        HTML = html_document()), #, PDF = pdf_document(), Word = word_document()),
        params = params,
        envir = new.env(parent = globalenv()))
      file.rename(out, file)
    }
  )
#------------ Cryptomarket visualization ------------------------------------------------------------------------
  
#   output$meth_plot <- renderPlotly({
#     mplot <- ggplot(Weekly2[(Weekly2$Week > "2020-10-08"),] 
#                     %>% subset(DrugAZ == input$drg), aes(x= Week, y = listing, color = MarketAZ)) + geom_line()
#   })
}

# Run the application 
shinyApp(ui = ui, server = server)
