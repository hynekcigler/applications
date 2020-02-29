library(shiny)

shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Kauzalita: Directed Acyclic Graphs (DAG)"),
  sidebarPanel(
    h4("Síla vztahů"),
    sliderInput("AB", "A <-> B", min = -1, max = 1, value = 0, step = .01),   
    sliderInput("AC", "A <-> C", min = -1, max = 1, value = 0, step = .01),  
    sliderInput("BC", "B <-> C", min = -1, max = 1, value = 0, step = .01), 
    h4("Směr vztahů"),
    radioButtons("ABdir", label = "A <-> B", 
                 choices = list('A -> B' = "AB",
                                'A <- B' = "BA")),
    radioButtons("ACdir", label = "A <-> C", 
                 choices = list('A -> C' = "AC",
                                'A <- C' = "CA")),
    radioButtons("BCdir", label = "b <-> C", 
                 choices = list('B -> C' = "BC",
                                'B <- C' = "CB")),
    checkboxInput("advanced", "Pokročilé možnosti"),
    conditionalPanel(condition = "input.advanced == 1", 
                     hr(),
                     h4("Reliability testů"),
                     p(strong("Upozornění: "), "Nastavení reliability zatím nefunguje."), 
                     numericInput("relA", "Reliabilita uzlu A", value = 1, min = 0, max = 1, width = 130, step = .05), 
                     numericInput("relB", "Reliabilita uzlu B", value = 1, min = 0, max = 1, width = 130, step = .05), 
                     numericInput("relC", "Reliabilita uzlu C", value = 1, min = 0, max = 1, width = 130, step = .05), 
                     checkboxInput("sampling", "Simulovat výběrovou chybu"),
                     conditionalPanel(condition = "input.sampling == 1",
                                      p(strong("Upozornění: "), "Nastavení výběrové chyby zatím nefunguje."), 
                                      numericInput("samplesize", "Velikost vzorku", 
                                                   value = 100, min = 10, width = 130))), 
    
    hr(),

    HTML("<p>Tento jednoduchý nástroj byl vyvinut k ilustraci usuzování na kauzalitu
    s pomocí orientovaných acyklických grafů <br />(DAG  &ndash; Directed Acyclic Graphs).<br />
    Další zdroje: Pearl, J. (2009). <i>Causality : Models, Reasoning, Inference</i>. Cambridge University Press.</p>
         <p>&copy; 2020 Hynek Cígler<br>
         Department of psychology, Faculty of Social Studies<br>
         Masaryk University, Brno, Czech Republic<br>
         <a href='https://github.com/hynekcigler/applications/tree/master/DAG' target='_blank'>Source code on GitHub.</a></p>"), 
    width = 3
  ),
  
  mainPanel(conditionalPanel("output.DAG == 1", "XXX"), 
            h2(textOutput("warn"), style="color: red;"),
            h3("Pravé modely"),
            fluidRow(
              column(4, h4("Kauzální data-generating model"),
                     plotOutput("true_plot", inline = TRUE),br(),
                     hr(),
                     tableOutput("true_tab")),
              column(4, h4("Pravá korelační matice"),
                     plotOutput("truecor_plot", inline = TRUE),br(),
                     hr(),
                     tableOutput("truecor_tab"))
            ),
            tags$hr(style="border-color: gray; border-width: 3px;"),
            
            h3("Pozorované modely"),
            
            fluidRow(
              column(4, h4("Pozorovaná korelační matice"), 
                     plotOutput("observed_plot", inline = TRUE),br(),
                     hr(),
                     tableOutput("observedcor_tab")),
              column(4, h4("Pozorovaná parciálně-korelační matice"),
                     plotOutput("partial", inline = T),
                     hr(),
                     tableOutput("partial_tab"),
                     textOutput("partial_warn1"),
                     textOutput("partial_warn2"))
            ),

    
    tags$hr(style="border-color: gray; border-width: 3px;"),

    
            
            br(), 
    width = 9
            
  ))
  
)
