library(shiny)

shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Let's measure your height the way psychologists do!"),
  sidebarPanel(
    radioButtons("sex", label = "You are a:",
                 choices = list(man="man", woman="woman")),
    textInput("height", label = "Your true height is (in cm):", value=180, width='180px'),
    selectInput("test", label = "How good is your IQ test?", 
                choices = list('Bad one (r=.60)' = .60,
                               'Acceptable (r=.70)' = .70,
                               'Enough good (r=.80)' = .80,
                               'Good one (r=.90)' = .90,
                               'The best one (r=.95)' = .95,
                               'user-specified' = 0
                )),
    conditionalPanel(condition = "input.test == 0", 
                     numericInput("test.user", label = "Your own reliability: ", 
                                  value = .5, min = 0, max = 0, width = "100px", step = .05)),
    actionButton(inputId="button", label="Let's measure!"),
    hr(),
    radioButtons("type", label = "Plot my height:",
                 choices = list('in IQ'="iq",
                                'in cm' = "cm"
                                )),
    HTML("<hr><p><i>This simple tool was developed as a joke to illustrate why it is not a good idea to trust your exact IQ value 
         reported by psychological tests.<br> 
         The author ot this tool is a psychologist involved in developing psychological tests.</i></p>
         <p>&copy; 2021 Hynek Cígler<br>
         Department of psychology, Faculty of Social Studies<br>
         Masaryk University, Brno, Czech Republic<br>
         <a href='https://github.com/hynekcigler/applications/tree/master/height' target='_blank'>Source code on GitHub</a></p>")
    ),
  mainPanel(h3("Right now, your height is ", textOutput("height", inline=T), "cm, expressed as an IQ, it is ", 
               textOutput("IQ", inline=T), "( percentile", textOutput("perc", inline=T), ")."),
            plotOutput("gauss"),
            h3("Do you want to know what your height will be at some other time?"),
            h3("Click on the 'Let's measure!' button on the left side!"),
            hr(),
            plotOutput("scatter"),
            br(), br()
            
  ))
  
  )
