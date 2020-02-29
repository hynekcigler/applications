library(shiny)
library(psych)
library(qgraph)
library(corpcor)

shinyServer(function(input, output, session) {
  

# Je graf DAG? ------------------------------------------------------------

  DAG <- reactive({
    if (((input$ABdir == "AB" & input$BCdir == "BC" & input$ACdir == "CA") | (input$ABdir == "BA" & input$BCdir == "CB" & input$ACdir == "AC")) & 
        (input$AB != 0 & input$BC != 0 & input$AC != 0)) {
      FALSE
    } else {
      TRUE
    }
  })
  

# definice pravého kauzálního grafu ---------------------------------------


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
  

# definice pravého korelačního grafu --------------------------------------

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
  

# Vektor reliabilit -------------------------------------------------------

  relmat <- reactive({
    # relmat <- matrix(NA, ncol=3, nrow = 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
    # diag(relmat) <- c(input$relA, input$relB, input$relC)
    c(input$relA, input$relB, input$relC)
  })
  

# Korelační pozorovaný graf ---------------------------------------------------

  observedcorgraph <- reactive({
    # observedcorgraph <- matrix(1, nrow = 3, ncol = 3, dimnames = list(LETTERS[1:3], LETTERS[1:3]))
    # observedcorgraph
    truecorgraph <- truecorgraph()
    observedcorgraph <- truecorgraph
    observedcorgraph[1,2] <- observedcorgraph[2,1] <- truecorgraph()[1,2]*relmat()[1]*relmat()[2]
    observedcorgraph[1,3] <- observedcorgraph[3,1] <- truecorgraph()[1,3]*relmat()[1]*relmat()[3]
    observedcorgraph[2,3] <- observedcorgraph[3,2] <- truecorgraph()[2,3]*relmat()[2]*relmat()[3]
    observedcorgraph
  })
  

# Parciální graf ----------------------------------------------------------

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
    list(graph=partial_graph, warn1=warn1, warn2=warn2)
    # partial_graph
  })
  
  

  

# OUTPUT: Tables ---------------------------------------------------------

  output$true_tab <- renderTable({
    if (isTRUE(DAG())) {
      truegraph()
    }
  }, rownames =T)
  
  output$truecor_tab <- renderTable({
    if (isTRUE(DAG())) {
      truecorgraph()
    }
  }, rownames =T)
  
  output$partial_tab <- renderTable({
    if (isTRUE(DAG())) {
      partial_graph()$graph
    }
  }, rownames =T)
  
  output$observedcor_tab <- renderTable({
    if (isTRUE(DAG())) {
      observedcorgraph()
    }
  }, rownames =T)
  

# OUTOUT: Warnings -------------------------------------------------------

  output$partial_warn1 <- renderText({
    if (isTRUE(DAG())) {
      partial_graph()$warn1
    }
  })
  output$partial_warn2 <- renderText({
    if (isTRUE(DAG())) {
      partial_graph()$warn2
    }
  })
  
  output$warn <- renderText({
    if (!isTRUE(DAG())) {
      "Graf je cyklický a nejde tedy o DAG. Změňte nastavení grafu!"
    }
  })
  


# OUTPUT: plots -----------------------------------------------------------

  output$true_plot <- renderPlot({
    if (isTRUE(DAG())) {
      qgraph(truegraph(), layout = "circle", asize=15, color = colors()[571], maximum=1)
    }
  }, width = 320, height = 320)
  output$truecor_plot <- renderPlot({
    if (isTRUE(DAG())) {
      qgraph(truecorgraph(), layout = "circle", color = colors()[148], maximum=1)
    }
  }, width = 320, height = 320)
  output$partial <- renderPlot({
    if (isTRUE(DAG())) {
      qgraph(partial_graph()$graph, layout = "circle", color = colors()[148], maximum=1)
    }
  }, width = 320, height = 320)
  output$observed_plot <- renderPlot({
    if (isTRUE(DAG())) {
      qgraph(observedcorgraph(), layout = "circle", color = colors()[148], maximum=1)
      
    }
  }, width = 320, height = 320)
})





