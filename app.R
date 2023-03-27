#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Author: Samuel Jones
# Purpose: 

#----Installing packages---------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyscreenshot)
library(readr)
library(tidyverse)
library(rgdal)
library(sp)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(dplyr)
library(tidyr)
library(janitor)
library(readr)
library(plotly)
library(shinycustomloader)
library(readxl)


#----Loading relevant data sets--------------------------------------------------------------------------------

load("death_2020.Rdata")
load("NHMD2021.Rdata")
Weekly_use_data <- read_excel("Weekly_use_data.xlsx")
X6monthuse <- read_excel("6monthuse.xlsx")
drugtypes.data <- readRDS("drugtypes.data.rds")
diagnosistable.data <- readRDS("diagnosistable.data.rds")
intenttable.data <- readRDS("intenttable.data.rds")
Weekly2 <- readRDS("Weekly2.rds")
nams = names(NHMD)


#----UI content for each of the tabs in the dataset-------------------------------------------------------------

drug_use <- fluidRow(valueBoxOutput("useheader", width = 12),
                     infoBoxOutput("sixmonthidrs", width = 3),
                     infoBoxOutput("sixmonthedrs", width = 3),
                     infoBoxOutput("weeklyidrs", width = 3),
                     infoBoxOutput("weeklyedrs", width = 3),
                     box(title= "Summary Information", solidHeader = TRUE, collapsible = TRUE, width = 12,
                         HTML("<p>This page presents self-reported rates of drug use among IDRS and EDRS participants from each Australian capital city. The IDRS dataset recruits’ participants who have injected an illicit substance in the past six months and contains Heroin data. EDRS is a sentinel sample of people who regularly use ecstasy or other stimulants. Therefore, the two surveys are comprised of specific demographics who do not represent all people who use drugs.<p>
                         
                         <p>Within the IDRS in 2022, drug of choice remained stable compared to 2021 (p=0.568), with nearly half of participants nominating methamphetamine (46%; 45% in 2021) as their drug of choice followed by two-fifths nominating heroin (39%; 40% in 2021). The drug injected most often in the past month also remained stable in 2022 relative to 2021 (p=0.498), with methamphetamine reported as the drug injected most often by 54% of the sample (53% in 2021). Weekly or more frequent consumption of heroin among the total sample remained stable in 2022 compared to 2021 (40% versus 37%; p=0.205), as did weekly or more frequent consumption of crystal methamphetamine (58% versus 57%; p=0.631). There was a significant increase in weekly or more frequent consumption of cannabis in 2022 compared to 2021 (60% versus 54%; p=0.013).<p>
                         
                         <p>Within the EDRS, recent use of any ecstasy significantly decreased in 2022 (88%; 95% in 2021; p<0.001), reaching the lowest percentage since monitoring began. Recent use of cocaine remained stable in 2022 (79%; 80% in 2021), however weekly or more frequent use increased (11%; 7% in 2021; p=0.009). Whilst recent methamphetamine use has been declining over time, a significant increase was observed in 2022 (31%) relative to 2021 (26%; p=0.030). This was mostly driven by an increase in recent powder use (16%; 12% in 2021; p=0.024), although crystal remained the most commonly used form of methamphetamine (18%).<p>")
                     ),
                     tabBox(title = paste0("Six Month Use"),
                            tabPanel("Plot", withLoader(plotlyOutput("SixMonthUse", width = "100%"), type="image", loader="DT_NIDIP_tween.gif")),
                            tabPanel("Methods")),
                     tabBox(title = "Weekly Use",
                            tabPanel("Plot", withLoader(plotlyOutput("WeeklyUsePlot", width = "100%"), type="image", loader="DT_NIDIP_tween.gif")),
                            tabPanel("Methods")
                            ),
                     box(title = "Click here to create report:", width = 2, weight = 200, downloadButton("downp1", "Download Plot:"))
                     )

drug_harms <- fluidRow(valueBoxOutput("harmsheader", width = 12), 
                       infoBoxOutput("death_rate"),
                       infoBoxOutput("hosp_rate"),
                       infoBoxOutput("poisonings"),
                       box(title= "Summary Information", solidHeader = TRUE, collapsible = TRUE, width = 12,
                           "This space will include key summary statistics for the drug that has been selected. 
                           It will also provide a written description of the individual plots depicted on the page."),
                       tabBox(title = "Drug Related Hospitalisations",
                          tabPanel("Plot", withLoader(plotlyOutput("ByDrugPlot", width="100%"), type="image", loader="DT_NIDIP_tween.gif")),
                          tabPanel("ICD-AM", tableOutput('drugtypetable'), 
                                   tableOutput('diagnosistable'),
                                   tableOutput('intenttable'))),
                       tabBox(title = "Drug Induced Deaths",
                          tabPanel("Plot", withLoader(plotlyOutput("JurPlot", width="100%"), type="image", loader="DT_NIDIP_tween.gif")),
                          tabPanel("ICD-AM", includeHTML("notesDT.html"))),
                       valueBox("Additional content coming soon", "Two additional plots may be placed here", width = 12, color = "red"))

drug_markets <- fluidRow(
                         infoBox("Hospitalisation rate", "254", "per 100,000 people", color = "red"),
                         infoBox("Sex", "values", "percent", color = "blue"),
                         infoBox("Rate in remote areas", "values", "per 100,000 people", color = "red"),
                         box(plotlyOutput("meth_plot", width = "100%", height = "400px"), width = 6))

#state_trend <- fluidRow(valueBoxOutput("strend", width = 12))

#----UI Sidebar Content--------------------------------------------------------------------------------------------------

ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Drug Trends App", 
                                    dropdownMenu(
                                      type = "notifications", 
                                      headerText = strong("HELP"), 
                                      icon = icon("question"), 
                                      badgeStatus = NULL)), 
                    dashboardSidebar(
                      sidebarMenu(
                        # Setting id makes input$tabs give the tabName of currently-selected tab
                        id = "tabs",
                        menuItem("Drug Use", tabName = "tdrug", icon = icon("pills"), # Menu for the Drug Use Tab
                              menuSubItem("Drug Use Dashboard", tabName = "drguse"),
                              menuItem(
                                selectInput("udrug", "Drug:",
                                            c("Methamphetamine", # This list may be expanded in future, however these four are the most important.
                                              "Heroin",
                                              "Ecstasy",
                                              "Cannabis",
                                              "Cocaine"))),
                              menuItem(
                                selectInput("city", "Jurisdiction:",
                                            c("Australia",
                                              "Adelaide",
                                              "Brisbane",
                                              "Canberra",
                                              "Darwin",
                                              "Hobart",
                                              "Melbourne",
                                              "Perth",
                                              "Sydney"))),
                                menuItem(
                                  sliderInput("yr00", "Year",
                                              min=2000, max=2022, # EDRS data starts in 2003
                                              value = c(2000, 2022), sep="")),
                              menuSubItem("Drug Use Report", tabName = "report")
                                 ),
                                  
#----Drug Harms sidebar menu-------------------------------------------------------------------------------
                        menuItem("Drug Harms", tabName = "hdrug", icon = icon("hospital"),
                                 menuSubItem("Drug Harms Dashboard", tabName = "drgharms"),
                                 # menuItem(
                                 #   selectInput("drg2", "Select Drug",
                                 #               c(levels(factor(NHMD$Drug))), selected = c("All drugs")
                                 #               )),
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
                                   # menuItem(
                                   #   sliderInput("yearsByDT", "Financial year",
                                   #               min = 2000, max = 2021, step=1, 
                                   #               value = c(2000, 2021), sep = "")),
                                 menuItem(
                                   selectInput(
                                     "plotByDT", "Plot:",
                                     c(
                                       "Age-standardised rate" = "sr",
                                       "Age-standardised rate (95% CI)" = "srci",
                                       "Crude rate" = "cr",
                                       "Crude rate (95% CI)" = "crci"
                                     ),
                                     selected = "sr"
                                   )),
                                 
                                  menuItem(
                                    sliderInput("yr97", "Period",
                                                min=1997, max=2021,
                                                value=c(1997, 2021), sep="")
                                 ),
                                 
                                   menuSubItem("Explanatory Notes", tabName = "usenotes")
                                 ),
                                               
                        menuItem("Drug Markets", tabName = "mdrug", icon = icon("money-bills"),
                          selectInput("drg", "Drug",
                                      c(levels(Weekly2$DrugAZ))
                                      ))
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
                        tabItem("mdrug", drug_markets)
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
  
  alldrugreactive <- reactive({
    COD2020_All$Drug <- "All drugs"
    alldrg <- COD2020_All %>%  subset(jurisdiction== selectedState() &
                                    Age %in% "All ages" & Intent %in% "All" & Drug == selectedDrug() &
                                    (Year>= selectedYear()[1] & Year<=selectedYear()[2]) & Release=="Current") %>% unite(AgeSex, c(Age, Sex), sep = ", ", remove = FALSE)
  })
  
  hospitalReactive <- reactive({

    sub <-  NHMD %>% subset(Reason=="Any" & Intent=="Any" & Age == "All ages" & jurisdiction == selectedState() & Drug == input$hdrug &
                                     (year >= selectedYear()[[1]] & year <= selectedYear()[[2]])) %>% unite(AgeSex, c(Age, Sex), sep = ", ", remove = FALSE)
  })
  
  
  deathsReactive <- reactive({
    COD2020_DT$jurisdiction[COD2020_All$jurisdiction == "Tasmania"] <- "Tasmania*"
    COD2020_DT$Drug <- ifelse(COD2020_DT$Drug == "AMPHETAMINES", "Amphetamine-type stimulants",
                              ifelse(COD2020_DT$Drug == "COCAINE", "Cocaine",
                                     ifelse(COD2020_DT$Drug == "CANNABINOIDS", "Cannabinoids",
                                            ifelse(COD2020_DT$Drug == "heroin", "Heroin", COD2020_DT$Drug))))
    

    pd <- COD2020_DT %>%  subset(jurisdiction== selectedState() &
                    Age %in% "All ages" & Intent %in% "All" & Drug == selectedDrug() &
                    (Year>= selectedYear()[1] & Year<=selectedYear()[2]) & Release=="Current") %>% unite(AgeSex, c(Age, Sex), sep = ", ", remove = FALSE)

  })
  
  # These if statements are necessary because the input 'All Drugs' for drug deaths is in a separate dataset to the other options
  
  jurdata <- reactive({
    if (selectedDrug() == "Heroin") {
      data <- deathsReactive()
    } else if (selectedDrug() == "Cannabinoids") {
      jurdata <- deathsReactive()
    } else if (selectedDrug() == "Cocaine") {
      data <- deathsReactive()
    } else if (selectedDrug() == "Amphetamine-type stimulants") {
      jurdata <- deathsReactive()
    } else if (selectedDrug() == "All drugs") {
      data <- alldrugreactive()
    }
  })
  
#---Drug Use Tab------------------------------------------------------------------------------------------------------------------------------------------------------------------

  output$useheader <- renderValueBox({
    valueBox(paste(input$udrug), color = "purple", paste0(input$city))
  })
  
# Infographics
  
  output$sixmonthidrs <- renderInfoBox({
    sub <- X6monthuse %>% subset((Year>=input$yr00[1] & Year<=input$yr00[2]) & Jurisdiction == input$city & Source == "IDRS")
    infoBox("Latest 6-month Use",
            paste0(tail(sub[[input$udrug]], n=1),"%"),
            "of IDRS respondents",
            icon = icon("calendar-check"),
            color = "blue")
  })
  
  output$sixmonthedrs <- renderInfoBox({
    sub <- X6monthuse %>% subset((Year>=input$yr00[1] & Year<=input$yr00[2]) & Jurisdiction == input$city & Source == "EDRS")
    infoBox("Latest 6-month Use",
            paste0(tail(sub[[input$udrug]], n=1),"%"),
            "of EDRS respondents",
            icon = icon("calendar-check"),
            color = "orange")
  })
  
  output$weeklyidrs <- renderInfoBox({
    sub <- Weekly_use_data %>% subset((Year>=input$yr00[1] & Year<=input$yr00[2]) & Source == "IDRS")
    infoBox("Latest Weekly Use",
            paste0(tail(sub[[input$udrug]], n=1),"%"),
            "of IDRS respondents",
            icon = icon("calendar-week"),
            color = "blue")
  })
  
  output$weeklyedrs <- renderInfoBox({
    sub <- Weekly_use_data %>% subset((Year>=input$yr00[1] & Year<=input$yr00[2]) & Source == "EDRS")
    infoBox("Latest Weekly Use",
            paste0(tail(sub[[input$udrug]], n=1),"%"),
            "of EDRS respondents",
            icon = icon("calendar-week"),
            color = "orange")
  })
  

  
#---Weekly drug use plot
 output$WeeklyUsePlot <- renderPlotly({
   sub <- Weekly_use_data %>% subset((Year>=input$yr00[1] & Year<=input$yr00[2]))
   
   p <- ggplot() +
     geom_line(data = sub, aes(x = Year, y = !!sym(input$udrug), colour = Source)) +
     geom_point(data = sub, aes(x = Year, y = !!sym(input$udrug), colour = Source), size = 0.8, shape = 21) +
     scale_color_manual(values = c("IDRS" = "blue", "EDRS" = "orange")) +
     labs(x = "Year",  y = "% IDRS/EDRS Participants", color = "Data source") +
     scale_x_continuous(breaks = Weekly_use_data$Year) +
     scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 10)) +
     theme(axis.text.x = element_text(angle = 35, hjust = 1), panel.grid.major.x = element_blank(),
           panel.grid.major.y = element_line(color = "gray90", linewidth = 0.2),
           panel.grid.minor.y = element_line(color = "gray90", linewidth = 0.2),
           panel.background = element_blank())
 })
  
  output$SixMonthUse <- renderPlotly({
    sub <- X6monthuse %>% subset((Year>=input$yr00[1] & Year<=input$yr00[2]) & Jurisdiction == input$city)
    
    p <- ggplot() +
      geom_line(data = sub, aes(x = Year, y = !!sym(input$udrug), colour = Source)) +
      geom_point(data = sub, aes(x = Year, y = !!sym(input$udrug), colour = Source, text = paste0("% Use: ",!!sym(input$udrug), "%")), size = 0.8, shape = 21) +
      scale_color_manual(values = c("IDRS" = "blue", "EDRS" = "orange")) +
      labs(x = "Year",  y = "% IDRS/EDRS Participants", color = "Data source") +
      scale_x_continuous(breaks = Weekly_use_data$Year) +
      scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 10)) +
      theme(axis.text.x = element_text(angle = 35, hjust = 1), panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "gray90", linewidth = 0.2),
            panel.grid.minor.y = element_line(color = "gray90", linewidth = 0.2),
            panel.background = element_blank())
    
  })
  

#---Drug Harms Tab----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Creating the output sections for each interactive element in the app
  
# INFOGRAPHIC 1
  output$death_rate <- renderInfoBox({
    yr <- selectedYear()
    deaths_box <- jurdata() %>% arrange(Year) %>% subset(jurisdiction== selectedState() &
                    Age %in% "All ages" & Intent %in% "All" & Sex == "People" &
                    (Year>=yr[[1]] & Year<=yr[[2]]) & Release=="Current" & Drug == selectedDrug())
    infoBox(
      "Most recent Death rate:",
      round(tail(deaths_box$sr, n=1), digits = 1),
      "per 100,000 people",
      icon = icon("line-chart"),
      color = "purple"
    )
  })

# INFOGRAPHIC 2
  output$hosp_rate <- renderInfoBox({
    hosp_box <- NHMD %>% subset(Reason=="Any" & Intent=="Any" & Age == "All ages" & jurisdiction == selectedState() & Drug == selectedDrug() &
                                  (year >= selectedYear()[[1]] & year <= selectedYear()[[2]])) %>% unite(AgeSex, c(Age, Sex), sep = ", ", remove = FALSE)
    
    infoBox(
      "Most recent Hospitalisation rate:",
      round(tail(hosp_box$sr, n=1), digits = 1),
      "per 100,000 people",
      icon = icon("stethoscope"),
      color = "blue"
    )
  })
  
# INFOGRAPHIC 3
  output$poisonings <- renderInfoBox({
    pois_box <- NHMD %>% subset(Reason=="Poisoning" & Intent=="Intentional" & Age == "All ages" & jurisdiction == selectedState() & Drug == selectedDrug() &
                         (year >= selectedYear()[[1]] & year <= selectedYear()[[2]]))
    
    infoBox(
      "Most recent Intentional Poisonings:",
      round(tail(pois_box$sr, n=1), digits = 1),
      "per 100,000 people",
      icon = icon("biohazard"),
      color = "purple"
    )
    
  })

  
  output$harmsheader <- renderValueBox({
    valueBox(paste(input$jur), color = "purple", "Drug Harms")
  })
  
  # output$strend <- renderValueBox({
  #  valueBox(paste(input$jur), color = "red", "Weekly drug listings on Cryptomarkets between 22/10/2021 and 26/05/2022")
  # })

#---Hospitalization Plot in the Drug Harms Tab--------------------------------------------------------------------------------
  
   output$ByDrugPlot <- renderPlotly({

     p <- ggplot(hospitalReactive()) + aes(x = year, colour = AgeSex, linetype = AgeSex, group = 1) +
       geom_line() + geom_point(size=0.55) +
       labs(x = "Financial Year", colour = "", linetype="") +
       theme(axis.text.x = element_text(angle = 30, hjust = 1), panel.grid.major.x = element_blank(),
             panel.grid.major.y = element_line(color = "gray90", linewidth = 0.2),
             panel.grid.minor.y = element_line(color = "gray90", linewidth = 0.2),
             panel.background = element_blank()) +
       scale_linetype_manual(values = AgeSextype) +
       #theme(legend.title = element_blank()) +
       scale_x_continuous(breaks = c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021),
                          labels=c("2000", '2003', '2006', '2009', '2012', '2015', '2018', '2021'))
     
    if (input$plotByDT == "cr" | input$plotByDT == "crci") {
       p <- p + aes(y = cr,
                    text = paste0(
                      "Financial Year: ", data_year,
                      "<br>Location: ", jurisdiction,
                      "<br>Drug: ", Drug,
                      "<br>Age group: ", Age,
                      "<br>Sex: ", Sex,
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
                      "Financial Year: ", data_year,
                      "<br>Location: ", jurisdiction,
                      "<br>Drug: ", Drug,
                      "<br>Age group: ", Age,
                      "<br>Sex: ", Sex,
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
         text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource-analytics/trends-drug-related-hospitalisations-australia-1999-2019">DrugTrends</a>, NDARC',
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
  
  
  output$drugtypetable <- renderTable({drugtypes.data},sanitize.text.function=function(x){x})
  
  output$diagnosistable <- renderTable(diagnosistable.data)
  
  output$intenttable <- renderTable(intenttable.data)
  
  #---------------Drug deaths plot by drug, jurisdiction, intent and sex ----------------------------------

  # These if statements are necessary because the input 'All Drugs' is in a separate dataset to the other options
  
jurdata2 <- reactive({jurdata})

  output$JurPlot <- renderPlotly({
    # Plot settings that are the same for all possible inputs
    
    gp <- ggplot(jurdata(), aes(x = Year, colour = AgeSex, linetype = AgeSex, group = 1)) + geom_point(size=0.55) +
    labs(x = "Year", colour = "", linetype="") + theme_light() + scale_linetype_manual(values = AgeSextype)
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
    validate(need(nrow(jurdata()) > 0, "No data selected or data unavailable"))
    
    gp <- gp + theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank())
    
    # Adding the NIDIP logo, a link to the most recent paper and the source
    ggplotly(gp, tooltip="text") %>%
      
      add_annotations(
        text='Source: <a href="https://ndarc.med.unsw.edu.au/resource-analytics/trends-drug-induced-deaths-australia-1997-2020">DrugTrends</a>, NDARC',
        xref="paper", yref="paper",
        x = 0, xanchor = "left",
        y = 1.04, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      )
  })
  
#observeEvent(input$downp1, { screenshot(id= "WeeklyUsePlot")})

  output$downp1 <- downloadHandler(
    filename = "plots_and_report.zip",
    content = function(file){
      # create a Zip file to store the PNG files and R Markdown report
      zip(file, extras = "-j")
      
      # loop through each plot and save as a PNG in the Zip file
      for (i in 1:1) {
        # generate the plot using code specific to your app
        plot <- output$WeeklyUsePlot
        
        # save the plot as a PNG in the Zip file
        png(paste0("plot", i, ".png"))
        print(plot)
        dev.off()
        
        # add the PNG file to the Zip file
        writeZip(paste0("plot", i, ".png"), file)
      }
      
      # render the R Markdown document and add it to the Zip file
      rmarkdown::render("Downloadable_report.Rmd", output_format = "html_document", output_file = "Downloadable_report.html")
      writeZip("Downloadable_report.html", file)
      
      # close the Zip file
      closeZip(file)
    }
  )
  #------------ Cryptomarket visualization ------------------------------------------------------------------------
  
  output$meth_plot <- renderPlotly({
    mplot <- ggplot(Weekly2[(Weekly2$Week > "2020-10-08"),] 
                    %>% subset(DrugAZ == input$drg), aes(x= Week, y = listing, color = MarketAZ)) + geom_line()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
