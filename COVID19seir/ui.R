library(shiny)
library(shinyWidgets)
library(plotly)

fluidPage(
  titlePanel("COVID-19 Spread vs Healthcare Capacity - with age-specific parameters"),
  hr(),
  p(div(HTML("This dashboard is an edited version of the analysis available 
             <a href=https://alhill.shinyapps.io/COVID19seir/> here</a> using code available 
             <a href=https://github.com/alsnhll/SEIR_COVID19_Dev> here</a>, as cloned on 10/04/2020. All credit to the original authors, whose work is  licensed under a <a href=https://creativecommons.org/licenses/by-sa/4.0/> Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0) License </a>. 
             Please refer to the links for updates to their SEIR model."))),
  
  p(div(HTML("This version has so far been changed to include download buttons for the content of all graphs, and to allow different
             parameter values for different age groups. These will change the model outputs depending on the proportion of the population in each age band."))),
  
  
  sidebarLayout(
    
    sidebarPanel(width=6,
                 
    # TODO: add error checking for sum to 100
    # TODO: boxes sticking out
    
    fluidRow(
     tags$style(HTML("
                     input[type=number] {
                     -moz-appearance:textfield;
                     }
                     input[type=number] {
                     -font-size:10px;
                     }
                     input[type=number]::{
                     -moz-appearance:textfield;
                     }
                     input[type=number]::-webkit-outer-spin-button,
                     input[type=number]::-webkit-inner-spin-button {
                     -webkit-appearance: none;
                     margin: 0;
                     }
                     ")),
     
     column(width=6,
            setSliderColor(c(rep("#b2df8a", 3)), sliderId=c(8,9,10)),
            p("Clinical parameters", style = "font-size:20px; font-weight:bold; font-style:oblique"),
            #h3(div(HTML("<em>Clinical parameters</em>"))),
            sliderInput("IncubPeriod", "Duration of incubation period", 0, 20, 5, step=0.5, post = " days"),
            
            p("% of infections that are severe", style = "font-size:14px; font-weight:bold"),
            #h4(div(HTML("<em>% of infections that are severe</em>"))),
            #sliderInput("FracSevere", "% of infections that are severe", 0, 100, 15, step=1, pre="%"),
            fluidRow(splitLayout(cellWidths = c(60),
                                 div(numericInput("FracSevere0to9","0 to 9",value = 0.2, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#FracSevere0to9 {font-size:11px;}")),
                                 div(numericInput("FracSevere10to19","10 to 19",value = 0.61, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#FracSevere10to19 {font-size:11px;}")), 
                                 div(numericInput("FracSevere20to29","20 to 29",value = 2.44, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#FracSevere20to29 {font-size:11px;}")),
                                 div(numericInput("FracSevere30to39","30 to 39",value = 6.51, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#FracSevere30to39 {font-size:11px;}")),
                                 div(numericInput("FracSevere40to49","40 to 49",value = 9.97, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#FracSevere40to49 {font-size:11px;}"))
            )),
            fluidRow(splitLayout(cellWidths = c(60),
                                 div(numericInput("FracSevere50to59","50 to 59",value = 20.75, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#FracSevere50to59 {font-size:11px;}")),
                                 div(numericInput("FracSevere60to69","60 to 69",value = 33.77, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#FracSevere60to69 {font-size:11px;}")),
                                 div(numericInput("FracSevere70to79","70 to 79",value = 49.43, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#FracSevere70to79 {font-size:11px;}")),
                                 div(numericInput("FracSevere80p","80+",value = 55.53, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#FracSevere80p {font-size:11px;}"))
            )),
            br(),
            
            p("% of infections that are critical", style = "font-size:14px; font-weight:bold"),
            #sliderInput("FracCritical", "% of infections that are critical",0, 20, 5, step=1, pre="%"),
            fluidRow(splitLayout(cellWidths = c(60),
                                 div(numericInput("FracCritical0to9","0 to 9",value = 0.87, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#FracCritical0to9 {font-size:11px;}")),
                                 div(numericInput("FracCritical10to19","10 to 19",value = 0.87, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#FracCritical10to19 {font-size:11px;}")), 
                                 div(numericInput("FracCritical20to29","20 to 29",value = 0.87, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#FracCritical20to29 {font-size:11px;}")),
                                 div(numericInput("FracCritical30to39","30 to 39",value = 0.87, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#FracCritical30to39 {font-size:11px;}")),
                                 div(numericInput("FracCritical40to49","40 to 49",value = 1.1, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#FracCritical40to49 {font-size:11px;}"))
            )),
            fluidRow(splitLayout(cellWidths = c(60),
                                 div(numericInput("FracCritical50to59","50 to 59",value = 2.13, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#FracCritical50to59 {font-size:11px;}")),
                                 div(numericInput("FracCritical60to69","60 to 69",value = 4.77, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#FracCritical60to69 {font-size:11px;}")),
                                 div(numericInput("FracCritical70to79","70 to 79",value = 7.53, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#FracCritical70to79 {font-size:11px;}")),
                                 div(numericInput("FracCritical80p","80+",value = 12.35, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#FracCritical80p {font-size:11px;}"))
            )),
            br(),
            
            p("Death rate for critical infections", style = "font-size:14px; font-weight:bold"),
            #sliderInput("ProbDeath", "Death rate for critical infections", 0, 100, 40, step=1, pre="%"),
            fluidRow(splitLayout(cellWidths = c(60),
                                 div(numericInput("ProbDeath0to9","0 to 9",value = 0.19, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#ProbDeath0to9 {font-size:11px;}")),
                                 div(numericInput("ProbDeath10to19","10 to 19",value = 17.49, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#ProbDeath10to19 {font-size:11px;}")), 
                                 div(numericInput("ProbDeath20to29","20 to 29",value = 31.1, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#ProbDeath20to29 {font-size:11px;}")),
                                 div(numericInput("ProbDeath30to39","30 to 39",value = 31.1, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#ProbDeath30to39 {font-size:11px;}")),
                                 div(numericInput("ProbDeath40to49","40 to 49",value = 31.1, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#ProbDeath40to49 {font-size:11px;}"))
            )),
            fluidRow(splitLayout(cellWidths = c(60),
                                 div(numericInput("ProbDeath50to59","50 to 59",value = 51.78, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#ProbDeath50to59 {font-size:11px;}")),
                                 div(numericInput("ProbDeath60to69","60 to 69",value = 63.85, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#ProbDeath60to69 {font-size:11px;}")),
                                 div(numericInput("ProbDeath70to79","70 to 79",value = 89.99, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#ProbDeath70to79 {font-size:11px;}")),
                                 div(numericInput("ProbDeath80p","80+",value = 95, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#ProbDeath80p {font-size:11px;}"))
            )),
            br(),
            
            #uiOutput("FracCriticalSlider"), # was commented
            #sliderInput("CFR", "Case fatality rate", 0, 100, 2, step=5, pre="%"), # was commented
            htmlOutput("CFR"), 
            br(),
            
            p("Duration of mild infections", style = "font-size:14px; font-weight:bold"),
            #sliderInput("DurMildInf", "Duration of mild infections", 0, 20, 6, step=1, post = " days"),
            fluidRow(splitLayout(cellWidths = c(60),
                                 div(numericInput("DurMildInf0to9","0 to 9",value = 6, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#DurMildInf0to9 {font-size:11px;}")),
                                 div(numericInput("DurMildInf10to19","10 to 19",value = 6, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#DurMildInf10to19 {font-size:11px;}")), 
                                 div(numericInput("DurMildInf20to29","20 to 29",value = 6, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#DurMildInf20to29 {font-size:11px;}")),
                                 div(numericInput("DurMildInf30to39","30 to 39",value = 6, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#DurMildInf30to39 {font-size:11px;}")),
                                 div(numericInput("DurMildInf40to49","40 to 49",value = 6, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#DurMildInf40to49 {font-size:11px;}"))
            )),
            fluidRow(splitLayout(cellWidths = c(60),
                                 div(numericInput("DurMildInf50to59","50 to 59",value = 6, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#DurMildInf50to59 {font-size:11px;}")),
                                 div(numericInput("DurMildInf60to69","60 to 69",value = 6, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#DurMildInf60to69 {font-size:11px;}")),
                                 div(numericInput("DurMildInf70to79","70 to 79",value = 6, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#DurMildInf70to79 {font-size:11px;}")),
                                 div(numericInput("DurMildInf80p","80+",value = 6, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#DurMildInf80p {font-size:11px;}"))
            )),
            br(),
            
            p("Duration of severe infection/hospitalisation", style = "font-size:14px; font-weight:bold"),
            #sliderInput("DurHosp", "Duration of severe infection/hospitalization", 0, 10, 4, step=1, post = " days"),
            fluidRow(splitLayout(cellWidths = c(60),
                                 div(numericInput("DurHosp0to9","0 to 9",value = 4, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#DurHosp0to9 {font-size:11px;}")), 
                                 div(numericInput("DurHosp10to19","10 to 19",value = 4, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#DurHosp10to19 {font-size:11px;}")), 
                                 div(numericInput("DurHosp20to29","20 to 29",value = 4, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#DurHosp20to29 {font-size:11px;}")),
                                 div(numericInput("DurHosp30to39","30 to 39",value = 4, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#DurHosp30to39 {font-size:11px;}")),
                                 div(numericInput("DurHosp40to49","40 to 49",value = 4, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#DurHosp40to49 {font-size:11px;}"))
            )),
            fluidRow(splitLayout(cellWidths = c(60),
                                 div(numericInput("DurHosp50to59","50 to 59",value = 4, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#DurHosp50to59 {font-size:11px;}")),
                                 div(numericInput("DurHosp60to69","60 to 69",value = 4, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#DurHosp60to69 {font-size:11px;}")),
                                 div(numericInput("DurHosp70to79","70 to 79",value = 4, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#DurHosp70to79 {font-size:11px;}")),
                                 div(numericInput("DurHosp80p","80+",value = 4, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#DurHosp80p {font-size:11px;}"))
            )),
            br(),
            
            p("Duration of critical infection/ICU stay", style = "font-size:14px; font-weight:bold"),
            #sliderInput("TimeICUDeath", "Duration critical infection/ICU stay", 0, 30, 10, step=1, post = " days"),
            fluidRow(splitLayout(cellWidths = c(60),
                                 div(numericInput("TimeICUDeath0to9","0 to 9",value = 10, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#TimeICUDeath0to9 {font-size:11px;}")),
                                 div(numericInput("TimeICUDeath10to19","10 to 19",value = 10, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#TimeICUDeath10to19 {font-size:11px;}")), 
                                 div(numericInput("TimeICUDeath20to29","20 to 29",value = 10, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#TimeICUDeath20to29 {font-size:11px;}")),
                                 div(numericInput("TimeICUDeath30to39","30 to 39",value = 10, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#TimeICUDeath30to39 {font-size:11px;}")),
                                 div(numericInput("TimeICUDeath40to49","40 to 49",value = 10, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#TimeICUDeath40to49 {font-size:11px;}"))
            )),
            fluidRow(splitLayout(cellWidths = c(60),
                                 div(numericInput("TimeICUDeath50to59","50 to 59",value = 10, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#TimeICUDeath50to59 {font-size:11px;}")),
                                 div(numericInput("TimeICUDeath60to69","60 to 69",value = 10, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#TimeICUDeath60to69 {font-size:11px;}")),
                                 div(numericInput("TimeICUDeath70to79","70 to 79",value = 10, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#TimeICUDeath70to79 {font-size:11px;}")),
                                 div(numericInput("TimeICUDeath80p","80+",value = 10, min = 0, max=100, step = 1), style = "font-size:12px;", tags$style("#TimeICUDeath80p {font-size:11px;}"))
            )),
            br()
            
     ),
     column(width=6,
            p("Age band proportions", style = "font-size:20px; font-weight:bold; font-style:oblique"),
            p("(must sum to 100)", style = "font-size:15px"),
            fluidRow(
              splitLayout(cellWidths = c(60),
                          div(numericInput("prop0to9","0 to 9",value = 19.5, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#prop0to9 {font-size:11px;}")),
                          div(numericInput("prop10to19","10 to 19",value = 17.2, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#prop10to19 {font-size:11px;}")), 
                          div(numericInput("prop20to29","20 to 29",value = 17.8, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#prop20to29 {font-size:11px;}")),
                          div(numericInput("prop30to39","30 to 39",value = 17.2, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#prop30to39 {font-size:11px;}")),
                          div(numericInput("prop40to49","40 to 49",value = 11.3, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#prop40to49 {font-size:11px;}"))
              )
            ),
            fluidRow(
              splitLayout(cellWidths = c(60),
                          div(numericInput("prop50to59","50 to 59",value = 8, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#prop50to59 {font-size:11px;}")),
                          div(numericInput("prop60to69","60 to 69",value = 5.4, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#prop60to69 {font-size:11px;}")),
                          div(numericInput("prop70to79","70 to 79",value = 2.6, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#prop70to79 {font-size:11px;}")),
                          div(numericInput("prop80p","80+",value = 1, min = 0, max=100, step = 0.5), style = "font-size:12px;", tags$style("#prop80p {font-size:11px;}"))
              )
            ),
            
            hr(),
            p("Set transmission rates", style = "font-size:20px; font-weight:bold; font-style:oblique"),
            sliderInput("b1", div(HTML("Transmission rate (mild infections)")), 0, 3, 0.33, step=0.02),
            sliderInput("b2", div(HTML("Transmission rate (severe infections, relative to mild)")),0, 2, 0, step=0.1),
            sliderInput("b3", div(HTML("Transmission rate (critical infections, relative to mild)")), 0, 2, 0, step=0.1),
            hr(),
            radioButtons("AllowSeason", "Allow seasonality in transmission?",
                         choices = list("Yes" = "Yes","No" = "No"),inline=TRUE, selected="No"),
            conditionalPanel(
              condition="input.AllowSeason == 'Yes'",
              sliderInput("seas.amp", "Amplitude of seasonality", 0, 100, 0, step=10, pre="%"),
              sliderInput("seas.phase", "Day of peak transmission (relative to t=0)", -365, 365, 0, step=1, post = " days")
            ),
            radioButtons("AllowAsym", "Allow asymptomatic infections?",
                         choices = list("Yes" = "Yes","No" = "No"),inline=TRUE,selected="No"),
            conditionalPanel(
              condition="input.AllowAsym == 'Yes'",
              sliderInput("FracAsym", "% of infections that are asymptomatic", 0, 100, 30, step=1, pre="%"),
              sliderInput("DurAsym", "Duration of asymptomatic infections", 1, 20, 6, step=1, post = " days"),
              sliderInput("b0", div(HTML("Asymptomatic transmission rate")), 0, 3, 0.5, step=0.02, post="/day")
            ),
            radioButtons("AllowPresym", "Allow pre-symptomatic transmission?",
                         choices = list("Yes" = "Yes","No" = "No"),inline=TRUE, selected="No"),
            conditionalPanel(
              condition="input.AllowPresym == 'Yes'",
              sliderInput("PresymPeriod", "Time before symptom onset at which transmission is possible", 0, 3, 2, step=0.5, post = " days"), #Make reactive
              sliderInput("be", div(HTML("Presymptomatic transmission rate")),0, 3, 0.5, step=0.02, post="/day")
            ),
            hr(),
            p("Set simulation values", style = "font-size:20px; font-weight:bold; font-style:oblique"),
            
            
            
            numericInput("N", div(HTML("Population size:")), value=1000, max=10^10, min=1000, step=1000),
            br(),
            numericInput("InitInf","Initial # infected:",value = 1, min = 1, step = 1),
            sliderInput("Tmax", div(HTML("Maximum time")),0, 1000, 300, step=10, post=" days"),
            actionButton("reset", "Reset all") 
            
            
            #sliderInput("LogN", div(HTML("Total population size (log10)")), 1, 9, 3, step=0.1),
            #htmlOutput("N"),
            
     )
     )
    
     ),
    
    mainPanel(width=6,
      
      #p(div(HTML("Test")))
      navbarPage("Output:",
                 
                 tabPanel("Spread",
                          fluidPage(
                            fluidRow(
                              
                              h3("Predicted COVID-19 cases by clinical outcome"),
                              p(HTML("Simulate the natural course of a COVID-19 epidemic in a single population without any interventions.")),
                              
                              downloadButton('downloadDataSpread', 'Download spread data with current parameters'),

                              plotlyOutput("plot0"),
                              br(),
                              br(),
                              column(width=6,
                                     radioButtons("yscale", "Y axis scale:",
                                                  choices = list("Linear" = "linear","Log10" = "log"),inline=TRUE)
                                     ),
                              column(width=6,
                                     radioButtons("PlotCombine", "Plot total infected?",
                                                  choices = list("Yes" = "Yes","No" = "No"),inline=TRUE, selected="No")
                              ),
                              br(),
                              p(HTML("<b>User instructions:</b> The graph shows the expected numbers of individuals over time who are infected, recovered, susceptible, or dead over time. Infected individuals first pass through an exposed/incubation phase where they are asymptomatic and not infectious, and then move into a symptomatic and infections stage classified by the clinical status of infection (mild, severe, or critical). A more detailed description of the model is provided in the Model Description tab. The population size, initial condition, and parameter values used to simulate the spread of infection can be specified through the sliders located in the left-hand panel. Default slider values are equal to estimates taken from the literature (see Sources tab). To reset default values, click on the <em>Reset all</em> button located on the bottom of the panel. The plot is interactive: Hover over it to get values, double-click a curve in the legend to isolate it, or single-click to remove it. Dragging over a range allows zooming."))
                            )
                          )
                 ),
                 
                 tabPanel("Intervention",
                          fluidPage(
                            fluidRow(
                              h3("Reduction in predicted COVID-19 after intervention"),
                              p(HTML("Simulate the change in the time course of COVID-10 cases after applying an intervention")),
                              downloadButton('downloadDataIntervention', 'Download intervention data with current parameters'),

                              plotlyOutput("plotInt"),
                              br(),
                              br(),
                              radioButtons("yscaleInt", "Y axis scale:",
                                           choices = list("Linear" = "linear","Log10" = "log"),inline=TRUE),
                              wellPanel(
                                h4(div(HTML("<em>Set intervention parameters...</em>"))),
                                selectInput("VarShowInt",
                                            label = "Select variable to show:",
                                            choices = c("Suceptible (S)"="S", "Exposed (E)"="E", "Mild Infections (I1)"="I1", "Severe Infections (I2)"="I2", "Critical Infections (I3)"="I3", "Recovered (R)"="R", "Dead (D)"="D", "All infected (E + all I)"="Inf","All symptomatic (I1+I2+I3)"="Cases","All hospitalized (I2+I3)"="Hosp"),
                                            selected = c("Cases")
                                ),
                                column(width=6,
                                         numericInput("Tint","Intervention start time (days):",value = 0, min = 0, step = 10)
                                         ),
                                  column(width=6,
                                         numericInput("Tend","Intervention end time (days):",value = 300, min = 0, step = 10)
                                         ),
                                p(HTML("<b>Intervention type: reducing transmission, </b> for example via social distancing or quarantining in the community (for those with mild infection) or better isolation and personal-protective wear in hospitals (for those with more severe infection). Transmission from each of the clinical stages of infection can only be reduced if the user has chosen parameters such that these stages contribute to transmission.")),
                                sliderInput("s1", "Reduction in transmission from mild infections ", 0, 100, 30, pre="%",step=1, animate=TRUE),
                                sliderInput("s2", "Reduction in transmission from severe infections", 0, 100, 0, pre="%",step=1, animate=TRUE),
                                sliderInput("s3", "Reduction in transmission rate from critical infections", 0, 100, 0, pre="%",step=1, animate=TRUE),
                                conditionalPanel(
                                  condition="input.AllowAsym == 'Yes' || input.AllowPresym == 'Yes' ",
                                  sliderInput("s0", "Reduction in transmission from pre/asymptomatic infections ", 0, 100, 0, pre="%",step=1, animate=TRUE),
                                ),
                                radioButtons("RoundOne", "Round values to nearest integar post-intervention?",
                                             choices = list("True" = "True","False" = "False"),inline=TRUE),
                              ),
                              p(HTML("<b>User instructions:</b> The graph shows the expected numbers of individuals over time who are infected, recovered, susceptible, or dead over time, with and without an intervention. Infected individuals first pass through an exposed/incubation phase where they are asymptomatic and not infectious, and then move into a symptomatic and infections stage classified by the clinical status of infection (mild, severe, or critical). A more detailed description of the model is provided in the Model Description tab. The population size, initial condition, and parameter values used to simulate the spread of infection can be specified through the sliders located in the left-hand panel. Default slider values are equal to estimates taken from the literature (see Sources tab). The strength and timing of the intervention is controlled by the sliders below the plot. To reset default values, click on the <em>Reset all</em> button located on the bottom of the panel. The plot is interactive: Hover over it to get values, double-click a curve in the legend to isolate it, or single-click to remove it. Dragging over a range allows zooming."))
                            )
                          )
                 ),
                 
                 tabPanel("Capacity",
                          fluidPage(
                            fluidRow(
                              h3("COVID-19 Cases vs Healthcare Capacity"),
                              p(HTML("Simulate predicted COVID-19 cases vs the capacity of the healthcare system to care for them. The care required depends on disease severity - individuals with `severe' infection require hospitalization and individuals with 'critical' infection often require ICU-level care and mechanical ventilation.")),
                              plotlyOutput("plotCap"),
                              br(),
                              br(),
                              radioButtons("yscaleCap", "Y axis scale:",
                                           choices = list("Linear" = "linear","Log10" = "log"),inline=TRUE),
                              wellPanel(
                                h4(div(HTML("<em>Set healthcare capacity...</em>"))),
                                p(HTML(" The default values are for the U.S. and details of their sources are given in the Sources tab")),
                                #Sliders for hospital capacity are reactive, since they take in default values from a file, so they are defined in the server file.  
                                fluidRow(
                                  p(HTML(" <b> All hospital beds: </b>")),
                                  column(width = 6,
                                         uiOutput("HospBedper")
                                  ),
                                  column(width = 6,
                                         uiOutput("HospBedOcc")
                                  ),
                                  p(HTML(" <b> ICU beds: </b>")),
                                  column(width = 6,
                                         uiOutput("ICUBedper")
                                  ),
                                  column(width = 6,
                                         uiOutput("ICUBedOcc")
                                  ),
                                  column(width = 12,
                                         uiOutput("IncFluOcc")
                                  ),
                                  p(HTML(" <b> Mechanical ventilators: </b>")),
                                  column(width = 4,
                                         uiOutput("ConvVentCap")
                                  ),
                                  column(width = 4,
                                         uiOutput("ContVentCap")
                                  ),
                                  column(width = 4,
                                         uiOutput("CrisisVentCap")
                                  )
                                ),
                                ),
                              wellPanel(
                                h4(div(HTML("<em>Set intervention parameters...</em>"))),
                                selectInput("VarShowCap",
                                            label = "Select variable to show:",
                                            choices = c("Critical Infections (I3) vs ICU beds"="I3bed", "Critical Infections (I3) vs ventilator capacity"="I3mv", "Severe + Critical Infections (I2+I3) vs Hospital Beds"="Hosp", "All symptomatic cases (I1+I2+I3) vs Hospital Beds"="CasesCap"),
                                            selected = c("Hosp")
                                ),
                                column(width=6,
                                       numericInput("TintC","Intervention start time (days):",value = 0, min = 0, step = 10)
                                ),
                                column(width=6,
                                       numericInput("TendC","Intervention end time (days):",value = 300, min = 0, step = 10)
                                ),
                                p(HTML("<b>Intervention type: reducing transmission, </b> for example via social distancing or quarantining in the community (for those with mild infection) or better isolation and personal-protective wear in hospitals (for those with more severe infection). Transmission from each of the clinical stages of infection can only be reduced if the user has chosen parameters such that these stages contribute to transmission.")),
                                sliderInput("s1C", "Reduction in transmission rate (mild infections) ", 0, 100, 30, pre="%",step=1, animate=TRUE),
                                sliderInput("s2C", "Reduction in transmission rate (severe infections) ", 0, 100, 0, pre="%",step=1, animate=TRUE),
                                sliderInput("s3C", "Reduction in transmission rate (critical infections) ", 0, 100, 0, pre="%",step=1, animate=TRUE),
                                conditionalPanel(
                                  condition="input.AllowAsym == 'Yes' || input.AllowPresym == 'Yes' ",
                                  sliderInput("s0C", "Reduction in transmission from pre/asymptomatic infections ", 0, 100, 0, pre="%",step=1, animate=TRUE),
                                ),
                                radioButtons("RoundOneCap", "Round values to nearest integar post-intervention?",
                                             choices = list("True" = "True","False" = "False"), inline=TRUE),
                              ),
                              p(HTML("<b>User instructions:</b> The graph shows the expected numbers of individuals over time who are infected, recovered, susceptible, or dead over time, with and without an intervention. Infected individuals first pass through an exposed/incubation phase where they are asymptomatic and not infectious, and then move into a symptomatic and infections stage classified by the clinical status of infection (mild, severe, or critical). A more detailed description of the model is provided in the Model Description tab. The population size, initial condition, and parameter values used to simulate the spread of infection can be specified through the sliders located in the left-hand panel. Default slider values are equal to estimates taken from the literature (see Sources tab). The strength and timing of the intervention is controlled by the sliders below the plot. To reset default values, click on the <em>Reset all</em> button located on the bottom of the panel. The plot is interactive: Hover over it to get values, double-click a curve in the legend to isolate it, or single-click to remove it. Dragging over a range allows zooming."))
                            )
                          )
                 ),

                 tabPanel("Model", br(),
                          fluidRow(column(12,
                                          withMathJax(),
                                          h2("Model Description"),
                                          plotOutput("plot4", height=200),
                                          includeMarkdown("SEIR.Rmd"),
                                          #h3("Equations"),
                                          br(),
                                          h2("Output"),
                                          h3("Rate parameters of dynamic model"),
                                          p(HTML("These parameters can be changed using the sliders in the other tabs. The values in this table represent the current values chosen via the sliders. Note that the transmission rates chosen by the sliders are always scaled by \\(N\\), so that \\(\\beta*N\\) is constant as \\(N\\) changes.")),
                                          tableOutput("ParameterTable"),br(),
                                          h3("Ratios of cases during early growth phase"),
                                          p(HTML("These values are calculated based on the current model parameters")),
                                          tableOutput("RatioTable"),br(),
                          ))),
                 
                 tabPanel("Sources",
                          fluidPage(
                            br(),
                            uiOutput("parameterDesc")
                          ))
                 
      )
    )
    
  )
  
)

