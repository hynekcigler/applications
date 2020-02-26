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

    HTML("<hr><p>Tento jednoduchý nástroj byl vyvinut k ilustraci usuzování na kauzalitu
    s pomocí orientovaných acyklických grafů <br />(DAG  &ndash; Directed Acyclic Graphs).<br />
    Další zdroje: Pearl, J. (2009). <i>Causality : Models, Reasoning, Inference</i>. Cambridge University Press.</p>
         <p>&copy; 2020 Hynek Cígler<br>
         Department of psychology, Faculty of Social Studies<br>
         Masaryk University, Brno, Czech Republic<br>
         <a href='#' target='_blank'>Source code on GitHub</a></p>"), 
    width = 3
  ),
  
  mainPanel(
    h3("Data-generating (skutečný) model"),
    fluidRow(column(4, plotOutput("true_plot", inline = TRUE)),
             column(4, tableOutput("true_tab"))),
    tags$hr(style="border-color: gray; border-width: 3px;"),

    h3("Pozorované modely"),
  
    fluidRow(
      column(4, 
             h4("Korelační matice"),
             plotOutput("truecor_plot", inline = TRUE),br(),
             tableOutput("truecor_tab")),
      column(4, 
             h4("Parciálně-korelační matice"),
             plotOutput("partial", inline = TRUE),br(),
             tableOutput("parcial_tab"),
             textOutput("parcial_warn1"),
             textOutput("parcial_warn2"))
    ),
    hr(),
    

            
            br(), 
    width = 9
            
  ))
  
)
