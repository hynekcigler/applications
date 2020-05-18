library(shiny)


shinyServer(function(input, output, session) {
  
# prepare -----------------------------------------------------------------
  
  # output$senzitivita <- renderText("125")
  # output$specificita <- renderText("666")
  
  
  SENZ <- reactive({if(input$typ == "PCR") {
    .937
  } else if (input$typ == "rychlo0") {
    .7
  } else if (input$typ == "rychlo1") {
    .956
  } else if(input$typ == "rychlo2") {
    .96
  } else {
    input$senz
  }})
  
  SPEC <- reactive({if(input$typ == "PCR") {
    .999
  } else if (input$typ == "rychlo0") {
    .8
  } else if (input$typ == "rychlo1") {
    .952
  } else if(input$typ == "rychlo2") {
    .903
  } else {
    input$spec
  }})
  
  prevalence <- reactive({
    if (input$type == "populace") {
      input$prevalence/100
    } else {
      if (input$error == T) {
        (input$prevalence/100 + SPEC() - 1)/(SENZ() + SPEC() - 1)
      } else {
        (input$prevalence/100 + trueSPEC() - 1)/(trueSENZ() + trueSPEC() - 1)
      }
      
    }
  })
  
  populace <- reactive({
    if (input$republika == T) {
      10649800
    } else {
      input$populace
    }
  })
  
  output$senzitivita <- renderText(SENZ())
  output$specificita <- renderText(SPEC())
  
  trueSPEC <- reactive({
    input$truespec
  })
  trueSENZ <- reactive({
    input$truesenz
  })
  
  bias <- reactive({
    if (input$republika == 1 | input$sampling == 1) {
      prevalence()
    } else {
      prevalence()*input$bias / (1 - prevalence() + prevalence()*input$bias)
    }
  })
  

  sick <- reactive({
    populace() * bias()
  })
  healthy <- reactive({
    populace() * (1-bias())
  })
  TP <- reactive({
    if(input$error == T) {
      SENZ()*sick()
    } else {
      trueSENZ()*sick()
    }

  })
  TN <- reactive({
    if(input$error == T) {
      SPEC()*healthy()
    } else {
      trueSPEC()*healthy()
    }
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
           formatC(TP()/populace()*100, digits=1, decimal.mark = ",", format="f"), ("%)"))
  })
  output$FN <- renderText({
    paste0(formatC(FN(), big.mark = ".", format = "d", decimal.mark = ","), " (", 
           formatC(FN()/populace()*100, digits=1, decimal.mark = ",", format="f"), ("%)"))  
  })
  output$TN <- renderText({
    paste0(formatC(TN(), big.mark = ".", format = "d", decimal.mark = ","), " (", 
           formatC(TN()/populace()*100, digits=1, decimal.mark = ",", format="f"), ("%)"))  
  })
  output$FP <- renderText({
    paste0(formatC(FP(), big.mark = ".", format = "d", decimal.mark = ","), " (", 
           formatC(FP()/populace()*100, digits=1, decimal.mark = ",", format="f"), ("%)"))  
  })
  


# * Table sum -------------------------------------------------------------

  output$positive <- renderText({
    paste0(formatC(FP()+TP(), big.mark = ".", format = "d", decimal.mark = ","), " (", 
           formatC((FP()+TP())/populace()*100, digits=1, decimal.mark = ",", format="f"), ("%)"))  
  })
  
  output$negative <- renderText({
    paste0(formatC(FN()+TN(), big.mark = ".", format = "d", decimal.mark = ","), " (", 
           formatC((FN()+TN())/populace()*100, digits=1, decimal.mark = ",", format="f"), ("%)"))  
  })
  
  output$sick <- renderText({
    paste0(formatC(FN()+TP(), big.mark = ".", format = "d", decimal.mark = ","), " (", 
           formatC((FN()+TP())/populace()*100, digits=1, decimal.mark = ",", format="f"), ("%)"))  
  })
  
  output$healthy <- renderText({
    paste0(formatC(FP()+TN(), big.mark = ".", format = "d", decimal.mark = ","), " (", 
           formatC((FP()+TN())/populace()*100, digits=1, decimal.mark = ",", format="f"), ("%)"))  
  })
  
  output$total <- renderText({
    paste0(formatC(TP()+FP()+TN()+FN(), big.mark = ".", format = "d", decimal.mark = ","), " (", 
           formatC((TP()+FP()+TN()+FN())/populace()*100, digits=0, decimal.mark = ",", format="f"), ("%)"))  
  })
  

# * Prevalence v populaci -------------------------------------------------
  
  correction <- reactive({
    (((FP()+TP())/populace()) + SPEC() - 1)/(SENZ() + SPEC() - 1)
  })

  output$positive2 <- renderText({
    formatC((FP()+TP())/populace()*100, digits=1, decimal.mark = ",", format="f")
  })
  
  output$bias1 <- renderText({
    result <- ((FP()+TP())/populace() - prevalence())*100
    result <- round(result, 8)
    formatC(result, digits=2, decimal.mark = ",", format="f")
  })
  
  output$positive_correct <- renderText({
    result <- correction()*100
    formatC(result, digits=1, decimal.mark = ",", format="f")
  })
  
  output$positive_correct_bias1 <- renderText({
    correction <- round(correction(), 8)
    formatC((correction - prevalence())*100, digits=2, decimal.mark = ",", format="f")
  })
  
  output$prevalence1 <- renderText({
    input$prevalence
  })
  output$prevalence2 <- renderText({
    input$prevalence
  })

  binom1 <- reactive({
    prop.test(x = round(FP()+TP()), 
               n = populace(), 
               p = input$prevalence/100)
  })
  binom2 <- reactive({
    prop.test(x = round(populace() * correction(), 0), 
               n = populace(), 
               p = input$prevalence/100)
  })
  
  output$ci <- renderText({
    if (min(c(TP(), TN(), FP(), FN()))/populace() > 0 & max(c(TP(), TN(), FP(), FN()))/populace() < 1) {
      paste(round(binom1()$conf.int[1]*100, 1), round(binom1()$conf.int[2]*100, 1), sep="; ")
    } else {"NaN, NaN"}

  })
  
  output$ci_correct <- renderText({
    if(correction() < 0 | min(c(TP(), TN(), FP(), FN()))/populace() < 0 | max(c(TP(), TN(), FP(), FN()))/populace() > 1) {
      "NaN, NaN"
    } else {
      paste(round(binom2()$conf.int[1]*100, 1), round(binom2()$conf.int[2]*100, 1), sep="; ")
    }
    
  })
  
  output$p <- renderText({
    if (min(c(TP(), TN(), FP(), FN()))/populace() > 0 & max(c(TP(), TN(), FP(), FN()))/populace() < 1) {
      round(binom1()$p.value, 3)
    } else  {
      "NaN"
    }
  })
  
  
  output$p_correct <- renderText({
    if(correction() < 0| min(c(TP(), TN(), FP(), FN()))/populace() < 0 | max(c(TP(), TN(), FP(), FN()))/populace() > 1) {
      "NaN"
    } else {
      round(binom2()$p.value, 3)
    }
    
  })
  

# * přesnost --------------------------------------------------------------

  output$spolehlivost <- renderText({
    formatC((TP()+TN())/populace()*100, digits=1, decimal.mark = ",", format="f")
  })  
  output$nespolehlivost <- renderText({
    formatC((FP()+FN())/populace()*100, digits=1, decimal.mark = ",", format="f")
  }) 
  output$PP <- renderText({
    formatC(TP()/(TP()+FP())*100, digits=1, decimal.mark = ",", format="f")
  }) 
  output$NP <- renderText({
    formatC(TN()/(TN()+FN())*100, digits=1, decimal.mark = ",", format="f")
  }) 
  
  

# * warning ---------------------------------------------------------------

  output$warn <- renderPrint({
    if (min(c(TP(), TN(), FP(), FN()))/populace() < 0 | max(c(TP(), TN(), FP(), FN()))/populace() > 1) {
      print("Varování: V bivariační tabulce jsou záporné hodnoty. Do aplikace jsou zadána nesmyslná data.")
    }
    
    if (correction() < 0) {
      print("Varování: Odhad populační prevalence je menší než 0, což není možné. Výsledky nedávají smysl.")
    }
    if (input$republika == 1) {
      print("Varování: P-hodnoty nejsou korigované na konečnou velikost vzorku a jsou zkreslené.")
    }
  })  
  


# grafy -------------------------------------------------------------------

  output$plot1 <- renderPlot({
    cols <- c("firebrick2", "lightgreen")
    x <- cbind("pozitivní" = c(FP(), TP())/(TP() + FP()),
               "negativní" = c(FN(), TN())/(TN() + FN()))*100
    mybar <- barplot(x, horiz=T, 
                     ylab="výsledek testu", xlab = "chybný výsledek (%)", 
                     cex.names = 1.5, cex.axis = 1.3, cex.lab = 1.6, cex.main = 2,
                     col = cols, 
                     main = "Podle výsledku testu")
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
                     main = "Podle zdravotního stavu")
    text(1, mybar, paste0(round(c(x[1,1], x[1,2]), 2), " %"), cex=1.5, pos=4)
    legend("top", c("chybně", "správně"), horiz = T, col = cols, pch=15, pt.cex=2, 
           title="Identifikováno:")
  })
  
})





