library(shiny)

year <- substr(Sys.Date(), 1, 4)

shinyUI(fluidPage(
  titlePanel("COVID-19: Chybovost a spolehlivost testů"),
  tags$head(includeHTML(("ga.html"))),
  title = "COVID-19: Chybovost a spolehlivost testů",
  withMathJax(),
  hr(),


fluidRow(  

# Sidepanel ---------------------------------------------------------------

  column(
  h4("Prevalence viru"),
  radioButtons("type", label = "Jaký typ prevalence chcete zadat?", 
               choices = list('Skutečnou prevalenci' = "populace", 
                              'Pozorovanou prevalenci' = "vzorek")),
  sliderInput("prevalence", label = "Kolik procent lidí ze vzorku je nakažených?", min = 0, max = 100, value = 5, step = .1, 
              animate=animationOptions(interval = 10, loop = T), 
              post=" %"),
  helpText("Pokud jste vybrali skutečnou prevalenci, zadejte podíl osob, které jsou infikované ve skutečnosti. 
           Pokud pracujete s pozorovanou prevalenci, zadejte podíl osob, u kterých vyšel pozitivní test."),
  checkboxInput("republika", label = "Chcete použít namísto vzorku počet obyvatel České republiky?", 
                value = F), 
  conditionalPanel(
    condition ="input.republika == 0",
    sliderInput("populace", label = "Velikost vzorku", ticks = F,
                min = 100, max = 100000, value = 27000, step = 100)
  ),
  conditionalPanel(
    condition ="input.republika == 1",
    p(tags$small(em("Česká republika má asi 10.649.800 obyvatel."))),
  ),

  
  hr(),
  h4("Parametry použitého testu"),
  radioButtons("typ", label = "Který test používáte?", 
               choices = list(PCR="PCR", 
                              rychlotest="rychlo0",
                              WANTAI="rychlo1", 
                              ELISA="rychlo2",
                              vlastní="user")),
  helpText(tags$strong("PCR:"), " senzitivita 0,937; specificita 0,999", br(), 
    tags$small("(Testy používané v nemocnici.)"), br(), 
    tags$strong("rychlotest:"), "senzitivita 0,7; specificita 0,8", br(), 
    tags$small("(Parametry celostátní studie odhadnuté z původních informací."), br(),
    tags$strong("WANTAI:"), ": senzitivita 0,956; specificita 0,952", br(), 
    tags$small("(Testy použité v celostátní studii ÚZIS.)"), br(),
    tags$strong("ELISA:"), ": senzitivita 0,960; specificita 0,903", br(), 
    tags$small("(Testy použité ve studii Jihočeského kraje.)")), 
  conditionalPanel(
    condition = "input.typ == 'user'",
    h4("Odhad parametrů testu:"),
    p(tags$small("Se kterými pracujeme v odhadech.")),
    sliderInput("senz", label = "senzitivita:", min = .5, max = 1, value = .7, step = .01, 
                animate=animationOptions(interval = 200, loop = T)), 
    sliderInput("spec", label = "specificita:", min = .5, max = 1, value = .8, step = .01, 
                animate=animationOptions(interval = 200, loop = T))),
  
  conditionalPanel(
    condition ="input.republika == 0",
    checkboxInput("sampling", "Je vzorek vybraný náhodně?", value = T),
    hr(),
    conditionalPanel(
      condition = "input.sampling == 0", 
      helpText(tags$small("Vzorek není vybraný náhodně; jde například o dobrovolníky.", br(), 
                   "Lze očekávat, že lidé působící v rizikovém prostředí, 
                 kteří mají vyšší pravděpodobnost nákazy, se rozhodnou pro účast ve studii ve srovnání 
                 s běžnou populací ČR.")),
      sliderInput("bias", label = "Kolikrát je větší pravděpodobnost, 
                že se pro dobrovolnou účast ve studii rozhodne infikovaný člověk než neinfikovaný?", 
                  value = 1, min = 1, max = 10, step = .1),
      hr()
    )
  ),
  
  
  checkboxInput("error", "Jsou parametry testu správné?", value = T),
  conditionalPanel(
    condition = "input.error == 0", 
    h4("Skutečné parametry testu:"),
    helpText(tags$small("Zde zadejte skutečné parametry testu, pokud se odlišují od známých (resp. odhadovaných) parametrů.")),
    sliderInput("truesenz", label = "senzitivita", min = .5, max = 1, value = .7, step = .01,
                animate = animationOptions(interval = 200, loop = T)),
    sliderInput("truespec", label = "specificita", min = .5, max = 1, value = .8, step = .01,
                animate = animationOptions(interval = 200, loop = T))
  ),

  
  
  hr(),
  
  
  # * copyright -------------------------------------------------------------
  
  HTML("<p>Tento jednoduchý nástroj byl vyvinut k ilustraci vlastností diagnostických testů, 
    používaných (nejen) během epidemie viru COVID-19.</p><p>&copy; ", 
       year, 
       " Hynek Cígler, Ph.D.<br>
         Katedra psychologie, Fakulta sociálních studií<br>
         Masarykova universita<br>
         Verze aplikace v0.3.<br>
         <a href='https://github.com/hynekcigler/applications/tree/master/ROC' target='_blank'>Source code on GitHub.</a></p>"), 
  width = 3
),



# Mainpanel ---------------------------------------------------------------


column(verbatimTextOutput("warn"),
       h3("Výsledek testu vs. skutečnost"),
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
             tags$th("skutečnost", br(), tags$small("ve vzorku"), rowspan=2,
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
       
       
       # * přesnost a odhad populace -----------------------------------------------------------------

       h3("Přesnost testu"),
       p("Spolehlivost testu:", textOutput("spolehlivost", inline = T), "%", br(), 
         "Nespolehlivost testu:", textOutput("nespolehlivost", inline = T), "%", br(), 
         "Pozitivní prediktivní hodnota testu:", textOutput("PP", inline = T), "%", br(), 
         "Negativní prediktivní hodnota testu:", textOutput("NP", inline = T), "%",
         style = "font-size: 120%;"),
       h3("Odhad prevalence"),
       p("Pozorovaná prevalence je:", textOutput("positive2", inline = T), 
         "% s 95% intervalem spolehlivosti CI", tags$sub("95%"), " = [", 
         textOutput("ci", inline = T), "].", br(),
         "Chyba oproti skutečné prevalenci ", textOutput("prevalence1", inline = T), 
         "% je tedy", textOutput("bias1", inline = T),  "%, ", tags$em("p"), " = ", 
         textOutput("p", inline = T), ".",
         style = "font-size: 120%;"),
       p("Korigovaná prevalence je:", textOutput("positive_correct", inline = T), 
         "% s 95% intervalem spolehlivosti CI", tags$sub("95%"), " = [", 
         textOutput("ci_correct", inline = T), "].", br(),
         "Chyba oproti skutečné prevalenci ", textOutput("prevalence2", inline = T), 
         "% je tedy", textOutput("positive_correct_bias1", inline = T),  "%, ", tags$em("p"), "=", 
         textOutput("p_correct", inline = T), ".", br(), 
         tags$small("Korigovaná prevalence by měla být očištěna o chybu odhadu způsobenou nepřesností testu."),
         style = "font-size: 120%;"),
       
       
       # * plots -----------------------------------------------------------------
       
       hr(),
       
       fluidRow(column(
         h3("Podíl chybných výsledků"),
         p("Jaká je pravděpodobnost, že vyšetřený člověk s pozitivním (resp. negativním) výsledkem testu 
              je ve skutečnosti nemocný (resp. zdravý)? Jinými slovy podíl lidí s určitým výsledkem testu, 
              u kterých je tento výsledek chybný. Výsledkem je vlastně jen prediktivní hodnota testu odečtená od jedné."),
         plotOutput("plot1", width = "100%"), 
         width = 6),
         column(
           h3("Pravděpodobnost chyby"),
           p("Jaká je pravděpodobnost, že u zdravého (resp. nemocného) člověka bude výsledek testu chybný? 
              Výsledkem je vlastně jen senzitivita, resp. specificita testu odečtená od jedné."),
           plotOutput("plot2", width = "100%"), 
           width = 6)), 
       br(),
       width = 9
       
)), fluidRow(
  withMathJax(),
  
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
    h4("Odhad skutečné prevalence a nepřesnost diagnostiky"),
    p("Nepřesnost diagnostického nástroje má vliv na odhad promořenosti populace. 
      Zjednodušeně lze říct, že čím větší je chybovost testu, tím blíže je odhadovaná prevalence 
      blízká 50 %. Tuto nepřesnost však lze možné korigovat mnoha různými postupy. Jeden z nejjednodušších, 
      který jsme použili v této aplikaci,  odhaduje skutečnou prevalenci \\(P^*\\) v daném vzorku podle vzorce
      
      $$P^* = \\frac{P+Sp-1}{Se+Sp-1}$$
      
      kde \\(P\\) je pozorovaný podíl pozorovaných pozitivních případů), 
      \\(Se\\) je specificita a  \\(Sp\\) specificita testu.", 
      "Pro účely této korekce je nicméně nezbytně nutné znát skutečnou senzitivitu a specificitu. 
      Případná nepřesnost v těchto parametrech pochopitelně zkreslí odhad populační prevalence. 
      Aplikace proto umožňuje nastavit jinou specificitu a senzitivitu (stačí zrušit zakliknutí 
      Jsou parametry testu správné)."),
    
    p("Z ukázkových důvodů si může uživatel aplikace vybrat, zda chce zadat pozorovanou prevalenci v daném vzorku 
      (tedy přímo podíl případů identifikovaných jako pozitivní), nebo skutečnou prevalenci (počet skutečně infikovaných). 
      Je nicméně potřeba si uvědomit, že skutečnou prevalenci nikdy nemůžeme znát; zde je tato možnost poskytnuta 
      pouze pro ilustrační účely."),
    
    p("Dále je důležité si uvědomit, že prevalence nemoci záleží na konkrétní populaci. Jiná bude 
  u náhodně vybraného občana České republiky, jiná (a výrazně vyšší) bude v populaci, jejíž členové vykazují 
  běžné symptomy onemocnění virem COVID-19."),
    h4("Použité parametry testů"),
    p("Jako spolehlivost testu RT-PCR byly použity", 
      tags$a("tyto hodnoty", href="https://www.massdevice.com/covid-19-test-development-surges-with-pandemic/"),
      ". Spolehlivost rychlotestů WANTAI, které byly použity v celostátní studii, byla odhadnuta na základě", 
      tags$a("výsledků studie", 
        href="https://covid-imunita.uzis.cz/res/file/prezentace/20200506-dusek.pdf"),
      ", zatímco pro přesnost testů ELISA (Anti-SARS-CoV-2 ELISA IgA) byly použity",  
      tags$a("informace výrobce", href="https://www.coronavirus-diagnostics.com/antibody-detection-tests-for-covid-19.html"), 
      ". V původní verzi této kalkulačky byly parametry rychlotestů z celostátní studie 
      (v aplikaci označované jako rychlotest) odhadnuty na základě", 
      tags$a("vyjádření prof. Prymuly", href="https://www.novinky.cz/domaci/clanek/prymula-o-chybovosti-rychlotestu-jsme-vedeli-menime-narizeni-40317761"), 
      "."),
    p("Pro více informací doporučujeme například", 
      a("tento článek", href="https://en.wikipedia.org/wiki/Receiver_operating_characteristic"), 
      "na anglické Wikipedii."),
    br(), br(),
    width=12)))


  
)
