library(shiny)


shinyServer(function(input, output, session) {
  
# prepare -----------------------------------------------------------------
  
  # output$senzitivita <- renderText("125")
  output$specificita <- renderText("666")
  
  
  SENZ <- reactive({if(input$typ == "PCR") {
    .937
  } else if (input$typ == "rychlo") {
    .7
  } else {
    input$senz
  }})
  
  prevalence <- reactive({
    input$prevalence/100
  })
  
  
  
  SPEC <- reactive({if(input$typ == "PCR") {
    .999
  } else if (input$typ == "rychlo") {
    .8
  } else {
    input$spec
  }})
  
  output$senzitivita <- renderText(SENZ())
  output$specificita <- renderText(SPEC())
  
  

  sick <- reactive({
    input$populace * prevalence()
  })
  healthy <- reactive({
    input$populace * (1-prevalence())
  })
  TP <- reactive({
    SENZ()*sick()
  })
  TN <- reactive({
    SPEC()*healthy()
  })
  FN <- reactive({
    sick()-TP()
  })
  FP <- reactive({
    healthy()-TN()
  })


# Output ------------------------------------------------------------------


# * ROC table -------------------------------------------------------------

  
  output$TP <- renderText({
    paste0(formatC(TP(), big.mark = ".", format = "d", decimal.mark = ","), " (", 
           formatC(TP()/input$populace*100, digits=1, decimal.mark = ",", format="f"), ("%)"))
  })
  output$FN <- renderText({
    paste0(formatC(FN(), big.mark = ".", format = "d", decimal.mark = ","), " (", 
           formatC(FN()/input$populace*100, digits=1, decimal.mark = ",", format="f"), ("%)"))  
  })
  output$TN <- renderText({
    paste0(formatC(TN(), big.mark = ".", format = "d", decimal.mark = ","), " (", 
           formatC(TN()/input$populace*100, digits=1, decimal.mark = ",", format="f"), ("%)"))  
  })
  output$FP <- renderText({
    paste0(formatC(FP(), big.mark = ".", format = "d", decimal.mark = ","), " (", 
           formatC(FP()/input$populace*100, digits=1, decimal.mark = ",", format="f"), ("%)"))  
  })
  


# * Table sum -------------------------------------------------------------

  output$positive <- renderText({
    paste0(formatC(FP()+TP(), big.mark = ".", format = "d", decimal.mark = ","), " (", 
           formatC((FP()+TP())/input$populace*100, digits=1, decimal.mark = ",", format="f"), ("%)"))  
  })
  
  output$negative <- renderText({
    paste0(formatC(FN()+TN(), big.mark = ".", format = "d", decimal.mark = ","), " (", 
           formatC((FN()+TN())/input$populace*100, digits=1, decimal.mark = ",", format="f"), ("%)"))  
  })
  
  output$sick <- renderText({
    paste0(formatC(FN()+TP(), big.mark = ".", format = "d", decimal.mark = ","), " (", 
           formatC((FN()+TP())/input$populace*100, digits=1, decimal.mark = ",", format="f"), ("%)"))  
  })
  
  output$healthy <- renderText({
    paste0(formatC(FP()+TN(), big.mark = ".", format = "d", decimal.mark = ","), " (", 
           formatC((FP()+TN())/input$populace*100, digits=1, decimal.mark = ",", format="f"), ("%)"))  
  })
  
  output$total <- renderText({
    paste0(formatC(TP()+FP()+TN()+FN(), big.mark = ".", format = "d", decimal.mark = ","), " (", 
           formatC((TP()+FP()+TN()+FN())/input$populace*100, digits=0, decimal.mark = ",", format="f"), ("%)"))  
  })
  

# * přesnost --------------------------------------------------------------

  output$spolehlivost <- renderText({
    formatC((TP()+TN())/input$populace*100, digits=1, decimal.mark = ",", format="f")
  })  
  output$nespolehlivost <- renderText({
    formatC((FP()+FN())/input$populace*100, digits=1, decimal.mark = ",", format="f")
  }) 
  output$PP <- renderText({
    formatC(TP()/(TP()+FP())*100, digits=1, decimal.mark = ",", format="f")
  }) 
  output$NP <- renderText({
    formatC(TN()/(TN()+FN())*100, digits=1, decimal.mark = ",", format="f")
  }) 
  
  output$plot1 <- renderPlot({
    cols <- c("firebrick2", "lightgreen")
    x <- cbind("pozitivní" = c(FP(), TP())/(TP() + FP()),
               "negativní" = c(FN(), TN())/(TN() + FN()))*100
    mybar <- barplot(x, horiz=T, 
                     ylab="výsledek testu", xlab = "chybný výsledek (%)", 
                     cex.names = 1.5, cex.axis = 1.3, cex.lab = 1.6, cex.main = 2,
                     col = cols, 
                     main = "Podíl chybných výsledků (podle výsledku testu)")
    text(1, mybar, paste0(round(c(x[1,1], x[1,2]), 2), " %"), cex=1.5, pos=4)
    legend("top", c("chybně", "správně"), horiz = T, col = cols, pch=15, pt.cex=2, 
           title="Identifikováno:")
  })
  
  output$plot2 <- renderPlot({
    cols <- c("firebrick2", "lightgreen")
    x <- cbind("pozitivní" = c(FN(), TP())/(FN() + TP()),
               "negativní" = c(FP(), TN())/(FP() + TN()))*100
    mybar <- barplot(x, horiz=T, 
                     ylab="ve skutečnosti", xlab = "chybný výsledek (%)", 
                     cex.names = 1.5, cex.axis = 1.3, cex.lab = 1.6, cex.main = 2,
                     col = cols, 
                     main = "Pravděpodobnost chyby (podle zdravotního stavu)")
    text(1, mybar, paste0(round(c(x[1,1], x[1,2]), 2), " %"), cex=1.5, pos=4)
    legend("top", c("chybně", "správně"), horiz = T, col = cols, pch=15, pt.cex=2, 
           title="Identifikováno:")
  })
  
})





