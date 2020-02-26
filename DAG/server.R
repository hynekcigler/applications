library(shiny)
library(psych)
library(qgraph)
library(corpcor)

shinyServer(function(input, output, session) {
  
  ## true correlation graph
  
  DAG <- reactive({
    if (((input$ABdir == "AB" & input$BCdir == "BC" & input$ACdir == "CA") | (input$ABdir == "BA" & input$BCdir == "CB" & input$ACdir == "AC")) & 
        (input$AB != 0 & input$BC != 0 & input$AC != 0)) {
      FALSE
    } else {
      TRUE
    }
  })
  
  truegraph <- reactive({
    if (isTRUE(DAG())) {
      truegraph <- matrix(c(0, NA, NA, 
                            NA, 0, NA,
                            NA, NA, 0), ncol = 3, 
                          dimnames = list(LETTERS[1:3], LETTERS[1:3]))
      
      if(input$ABdir == "AB") {
        truegraph[2, 1] <- 0
        truegraph[1, 2] <- input$AB
      } else {
        truegraph[2, 1] <- input$AB
        truegraph[1, 2] <- 0
      }
      if(input$ACdir == "AC") {
        truegraph[3, 1] <- 0
        truegraph[1, 3] <- input$AC
      } else {
        truegraph[3, 1] <- input$AC
        truegraph[1, 3] <- 0
      }
      if(input$BCdir == "BC") {
        truegraph[3, 2] <- 0
        truegraph[2, 3] <- input$BC
      } else {
        truegraph[3, 2] <- input$BC
        truegraph[2, 3] <- 0
      }
      truegraph
    } else {
      NULL
    }

  })
  
  truecorgraph <- reactive({
    unweight <- as.logical(truegraph())
    truecorgraph <- matrix(c(1, NA, NA, 
                             NA, 1, NA,
                             NA, NA, 1), ncol = 3, 
                           dimnames = list(LETTERS[1:3], LETTERS[1:3]))
    if (input$ABdir == "AB" & input$BCdir == "BC" & input$ACdir == "AC") {
      truecorgraph[1,2] <- truecorgraph[2,1] <- truegraph()[1,2]
      truecorgraph[2,3] <- truecorgraph[3,2] <- truegraph()[2,3] + truegraph()[1,2]*truegraph()[1,3]
      truecorgraph[1,3] <- truecorgraph[3,1] <- truegraph()[1,3] + truegraph()[1,2]*truegraph()[2,3]
    } else if (input$ABdir == "AB" & input$BCdir == "CB" & input$ACdir == "AC") {
      truecorgraph[1,2] <- truecorgraph[2,1] <- truegraph()[1,2] + truegraph()[1,3]*truegraph()[3,1]
      truecorgraph[2,3] <- truecorgraph[3,2] <- truegraph()[3,2] + truegraph()[1,2]*truegraph()[1,3]
      truecorgraph[1,3] <- truecorgraph[3,1] <- truegraph()[1,3]
    } else if (input$ABdir == "BA" & input$BCdir == "BC" & input$ACdir == "AC") {
      truecorgraph[1,2] <- truecorgraph[2,1] <- truegraph()[2,1]
      truecorgraph[2,3] <- truecorgraph[3,2] <- truegraph()[2,3] + truegraph()[2,1]*truegraph()[1,3]
      truecorgraph[1,3] <- truecorgraph[3,1] <- truegraph()[1,3] + truegraph()[2,1]*truegraph()[2,3]
    } else if (input$ABdir == "BA" & input$BCdir == "BC" & input$ACdir == "CA") {
      truecorgraph[1,2] <- truecorgraph[2,1] <- truegraph()[1,2] 
      truecorgraph[2,3] <- truecorgraph[3,2] <- truegraph()[3,2] + truegraph()[3,1]*truegraph()[1,2]
      truecorgraph[1,3] <- truecorgraph[3,1] <- truegraph()[3,1] + truegraph()[2,1]*truegraph()[2,3]
    } else if (input$ABdir == "AB" & input$BCdir == "CB" & input$ACdir == "CA") {
      truecorgraph[1,2] <- truecorgraph[2,1] <- truegraph()[1,2] + truegraph()[3,1]*truegraph()[3,2]
      truecorgraph[2,3] <- truecorgraph[3,2] <- truegraph()[3,2] + truegraph()[3,1]*truegraph()[1,2]
      truecorgraph[1,3] <- truecorgraph[3,1] <- truegraph()[3,1]
    } else if (input$ABdir == "BA" & input$BCdir == "CB" & input$ACdir == "CA") {
      truecorgraph[1,2] <- truecorgraph[2,1] <- truegraph()[2,1] + truegraph()[3,1]*truegraph()[3,2]
      truecorgraph[2,3] <- truecorgraph[3,2] <- truegraph()[3,2]
      truecorgraph[1,3] <- truecorgraph[3,1] <- truegraph()[3,1] + truegraph()[3,2]*truegraph()[2,1]
    } else if (input$ABdir == "AB" & input$BCdir == "BC" & input$ACdir == "CA") {
      truecorgraph[1,2] <- truecorgraph[2,1] <- truegraph()[1,2] + truegraph()[2,3]*truegraph()[3,1]
      truecorgraph[2,3] <- truecorgraph[3,2] <- truegraph()[2,3] + truegraph()[3,1]*truegraph()[1,2]
      truecorgraph[1,3] <- truecorgraph[3,1] <- truegraph()[3,1] + truegraph()[1,2]*truegraph()[2,3]
    } else if (input$ABdir == "BA" & input$BCdir == "CB" & input$ACdir == "AC") {
      truecorgraph[1,2] <- truecorgraph[2,1] <- truegraph()[2,1] + truegraph()[3,2]*truegraph()[1,3]
      truecorgraph[2,3] <- truecorgraph[3,2] <- truegraph()[3,2] + truegraph()[1,3]*truegraph()[2,1]
      truecorgraph[1,3] <- truecorgraph[3,1] <- truegraph()[1,3] + truegraph()[2,1]*truegraph()[3,2]
    } else {
      truecorgraph <- NULL
    }
    truecorgraph
    
  })
  
  
  partial_graph <- reactive({
    partial_graph <- cor2pcor(truecorgraph())
    warn1 <- ""
    warn2 <- ""
    if (is.na(partial_graph[1,2])) {
      partial_graph <- cor2pcor(cor.shrink(truecorgraph()))
      warn1 <- "Upozornění: Determinant korelační matice byl menší nebo roven jedné. 
      Korelační matice byla před invertováním upravena ztrátovou funkcí (shrinkage)."
    }
    
    dimnames(partial_graph) <- dimnames(truecorgraph())
    list(partial_graph, warn1, warn2)
  })
  
  
  
  output$true_tab <- renderTable({
    truegraph()
  }, rownames =T)
  
  output$truecor_tab <- renderTable({
    truecorgraph()
  }, rownames =T)
  
  output$parcial_tab <- renderTable({
    partial_graph()[[1]]
  }, rownames =T)
  output$parcial_warn1 <- renderText({
    partial_graph()[[2]]
  })
  output$parcial_warn2 <- renderText({
    partial_graph()[[3]]
  })
  

  
  output$true_plot <- renderPlot({
    qgraph(truegraph(), layout = "circle", asize=15, color = colors()[571])
  }, width = 320, height = 320)
  output$truecor_plot <- renderPlot({
    qgraph(truecorgraph(), layout = "circle", color = colors()[148])
  }, width = 320, height = 320)
  output$partial <- renderPlot({
    qgraph(partial_graph()[[1]], layout = "circle", color = colors()[148])
  }, width = 320, height = 320)
})





