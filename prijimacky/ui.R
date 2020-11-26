library(shiny)

shinyUI(
  fluidPage(
    titlePanel("Paradox podmíněné pravděpodobnosti při přijímacích zkouškách"),
    sidebarLayout(

  sidebarPanel(
    h4("Vztahy proměnných v celém vzorku včetně nepřijatých"),
    sliderInput(inputId = "r_osp_zsv", 
                label = "Korelace OSP a ZSV", 
                min = 0, max = 1, step = .01, value = .71),
    sliderInput(inputId = "r_osp_prospech", 
                "Korelace OSP a prospěchu", 
                min = 0, max = 1, step = .01, value = .5),
    sliderInput("r_zsv_prospech", 
                label = "Korelace ZSV a prospěchu", 
                min = 0, max = 1, step = .01, value = .5),
    sliderInput("vaha_zsv", 
                "Váha test ZSV", 
                min = 0, max = 1, step = .01, value = .6),
    numericInput("N", "Počet uchazečů", value = 12029),
    numericInput("n", "Počet přijatých", value = 441),
    numericInput("nsamples", "Počet simulovaných vzorků", value = 10),
    helpText("Pozor, více než 100 vzorků při počtech uchazečů kolem 10.000 může výrazně zpomalit běh aplikace.")
  ), 
  mainPanel(
    h3("Hypotetická populační korelační matice všech uchazečů"),
    tableOutput("cormat"),
    h3("Hypotetická pozorovaná korelační matice všech uchazečů (simulovaná)"),
    tableOutput("cormat_obs"),
    verbatimTextOutput("lm1"),
    h3("Pozorovaná korelační matice přijatých uchazečů"),
    tableOutput("cormat_prijati"),
    verbatimTextOutput("lm2")
  )
  
)))