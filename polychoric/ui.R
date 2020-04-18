library(shiny)

year <- substr(Sys.Date(), 1, 4)

shinyUI(fluidPage(
  titlePanel("Polychoric Correlation: Bias simulation"),
  tags$head(includeHTML(("ga.html"))),
  title = "Polychoric Correlation: Bias simulationů",
  hr(),


fluidRow(  
  column(
    numericInput("N", "Sample size", value = 200, min = 10),
    hr(),
  
  
  # * copyright -------------------------------------------------------------
  
  HTML("<p>This application serves for simulation bias in polychoric</p><p>&copy; ", 
       year, 
       " Hynek Cígler, Ph.D.<br>
         Katedra psychologie, Fakulta sociálních studií<br>
         Masarykova universita<br>
         <a href='https://github.com/hynekcigler/applications/tree/master/ROC' target='_blank'>Source code on GitHub.</a></p>"), 
  width = 3
),



# Mainpanel ---------------------------------------------------------------


column(h3("Výsledek testu vs. skutečnost"),
       
       tags$table(
         tags$thead(
           tags$tr(tags$th("", style="text-align: center; vertical-align: bottom; ", rowspan=2),
                   tags$th("", style="text-align: center; vertical-align: bottom; ", rowspan=2),
                   tags$th("výsledky testu", colspan=2, style="text-align: center;"),
                   tags$th("celkem", style="text-align: center; vertical-align: bottom; ", rowspan=2)),
           tags$tr(tags$th("pozitivní", style="text-align: center;"),
                   tags$th("negativní", style="text-align: center;"))
         ),
         tags$tbody(
           tags$tr(
             tags$th("skutečnost", rowspan=2,
                     style="text-align: left; vertical-align: middle; border-bottom: 2px solid #ddd;"),
             tags$th("nemocní", style="vertical-align: middle;"),
             tags$td(textOutput("TP", inline = T), style="text-align: center;"),
             tags$td(textOutput("FN", inline = T), style="text-align: center;"),
             tags$th(textOutput("sick", inline = T), style="text-align: center;"),
           ),
           tags$tr(
             tags$th("zdraví", style="text-align: left; border-bottom: 2px solid #ddd; vertical-align: middle;"),
             tags$td(textOutput("FP", inline = T), style="text-align: center; border-bottom: 2px solid #ddd;"),
             tags$td(textOutput("TN", inline = T), style="text-align: center; border-bottom: 2px solid #ddd;"),
             tags$th(textOutput("healthy", inline = T), style="text-align: center; border-bottom: 2px solid #ddd;")
           )
         ),
         tags$tfoot(tags$th("", style="text-align: right;"),
                    tags$th("celkem", style="text-align: left; vertical-align: middle;"),
                    tags$th(textOutput("positive", inline = T), style="text-align: center;"),
                    tags$th(textOutput("negative", inline = T), style="text-align: center;"),
                    tags$th(textOutput("total", inline = T), style="text-align: center;")),
         class='table shiny-table table- spacing-c', style='width:auto; font-size: 120%;'
       ),
       hr(),
       
       
       # * plots -----------------------------------------------------------------
       
       h3("Přesnost testu"),
       p("Spolehlivost testu:", textOutput("spolehlivost", inline = T), "%", br(), 
         "Nespolehlivost testu:", textOutput("nespolehlivost", inline = T), "%", br(), 
         "Pozitivní prediktivní hodnota testu:", textOutput("PP", inline = T), "%", br(), 
         "Negativní prediktivní hodnota testu:", textOutput("NP", inline = T), "%", br(), 
         style = "font-size: 120%;"
       ),
       hr(),
       h3("Podíl chybných výsledků"),
       p("Jaká je pravděpodobnost, že vyšetřený člověk s pozitivním (resp. negativním) výsledkem testu 
              je ve skutečnosti nemocný (resp. zdravý)? Jinými slovy podíl lidí s určitým výsledkem testu, 
              u kterých je tento výsledek chybný. Výsledkem je vlastně jen prediktivní hodnota testu odečtená od jedné."),
       plotOutput("plot1", width = "800"),
       hr(),
       h3("Pravděpodobnost chyby"),
       p("Jaká je pravděpodobnost, že u zdravého (resp. nemocného) člověka bude výsledek testu chybný? 
              Výsledkem je vlastně jen senzitivita, resp. specificita testu odečtená od jedné."),
       plotOutput("plot2", width = "800"),
       
      
       
       br(),
       
       width = 9
       
)), fluidRow(
  column(
    # * info ------------------------------------------------------------------
    hr(),
    h3("Postup výpočtu, vysvětlivky"),
    p("Tato kalkulačka popisuje chybovost testů používaných v epidemiologii i v dalších oborech 
  (např. psychologie). Pokud N označuje počet osob v celé populaci, existují čtyři možné výsledky testování: "),
    tags$ul(
      tags$li(strong("True-Positive (TP):"), 
              "Počet infikovaných osob, které test správně označí jako nakažené."),
      tags$li(strong("True-Negative (TN):"), 
              "Počet zdravých osob, kteří jsou správně označeni jako zdraví."),
      tags$li(strong("False-Positive (FP):"), 
              "Počet osob, kteří mají pozitivní výsledek testu a jsou označeni jako infikovaní, 
          přestože jsou zdraví."),
      tags$li(strong("False-Negative (FN):"), 
              "Počet osob, kteří mají negativní výsledek testu a jsou tedy označeni jako zdraví, 
          přestože jsou ve skutečnosti nakažení.")
    ),
    p("Tyto výsledky lze shrnout do následující tabulky:"),
    
    tags$table(
      tags$thead(
        tags$tr(tags$th("", style="text-align: center; vertical-align: bottom; ", rowspan=2),
                tags$th("", style="text-align: center; vertical-align: bottom; ", rowspan=2),
                tags$th("výsledky testu", colspan=2, style="text-align: center;"),
                tags$th("celkem", style="text-align: center; vertical-align: bottom; ", rowspan=2)),
        tags$tr(tags$th("pozitivní", style="text-align: center;"),
                tags$th("negativní", style="text-align: center;"))
      ),
      tags$tbody(
        tags$tr(
          tags$th("skutečnost", rowspan=2,
                  style="text-align: left; vertical-align: middle; border-bottom: 2px solid #ddd;"),
          tags$th("nemocní", style="vertical-align: middle;"),
          tags$td("TP", style="text-align: center;"),
          tags$td("FN", style="text-align: center;"),
          tags$th("TP+FN", style="text-align: center;"),
        ),
        tags$tr(
          tags$th("zdraví", style="text-align: left; border-bottom: 2px solid #ddd; vertical-align: middle;"),
          tags$td("FP", style="text-align: center; border-bottom: 2px solid #ddd;"),
          tags$td("TN", style="text-align: center; border-bottom: 2px solid #ddd;"),
          tags$th("FP+TN", style="text-align: center; border-bottom: 2px solid #ddd;")
        )
      ),
      tags$tfoot(tags$th("", style="text-align: right;"),
                 tags$th("celkem", style="text-align: left; vertical-align: middle;"),
                 tags$th("TP+FP", style="text-align: center;"),
                 tags$th("FN+TN", style="text-align: center;"),
                 tags$th("N", style="text-align: center;")),
      class='table shiny-table table- spacing-c', style='width:auto; font-size: 120%;'
    ),
    
    p("Základními parametry, které popisují přesnost testu nezávisle na diagnostikované populaci, jsou:"),
    tags$ul(tags$li(strong("Senzitivita (Se): "), "Pravděpodobnost, že nakažený člověk bude správně testem 
                identifikován jako nakažený. Lze spočítat jako TP/(TP+FN)"), 
            tags$li(strong("Specificita (Sp): "), "Pravděpodobnost, že zdravý člověk bude správně diagnostikován 
                       jako zdravý. Lze spočítat jako TN/(FP+TN)")),
    
    p("S pomocí těchto informací lze spočítat přesnost diagnostiky v dané populaci, zejména pak:"),
    tags$ul(
      tags$li(strong("Spolehlivost: "), "podíl správně identifikovaných osob, tedy (TP+TN)/N"),
      tags$li(strong("Chybovost:"), "podíl chybně identifikovaných osob, tedy (FP+FN)/N."),
      tags$li(strong("Pozitivní prediktivní hodnota"), "podíl nakažených osob mezi lidmi, 
          které jako nakažené identifikuje test. Lze spočítat jako TP/(TP+FP)."),
      tags$li(strong("Negativní prediktivní hodnota"), "podíl zdravých osob mezi lidmi, 
          které identifikuje jako zdravé. Lze spočítat jako TN/(TN+FN)."),
    ),
    p("Je důležité si uvědomit, že prevalence nemoci záleží na konkrétní populaci. Jiná bude 
  u náhodně vybraného občana České republiky, jiná (a výrazně vyšší) bude u člověka, který vykazuje 
  běžné symptomy onemocnění virem COVID-19."),
    p("Jako spolehlivost testu RT-PCR byly použity", 
      tags$a("tyto hodnoty", href="https://www.massdevice.com/covid-19-test-development-surges-with-pandemic/"),
      "spolehlivost tzv. rychlotestů byla odhadnuta na základě", 
      a("vyjádření prof. Prymuly", 
        href="https://www.novinky.cz/domaci/clanek/prymula-o-chybovosti-rychlotestu-jsme-vedeli-menime-narizeni-40317761"),
      "který uváděl jejich spolehlivost kolem 20–30 %."
    ),
    p("Pro více informací doporučujeme například", 
      a("tento článek", href="https://en.wikipedia.org/wiki/Receiver_operating_characteristic"), 
      "na anglické Wikipedii."),
    br(), br(),
    width=12)))


  
)
