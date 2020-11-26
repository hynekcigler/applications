library(shiny)
library(ps)
library(lm.beta)

shinyServer(function(input, output, session) {
  cormat <- reactive({
    cormat <- matrix(c(1, input$r_osp_zsv, input$r_osp_prospech, 
                       input$r_osp_zsv, 1, input$r_zsv_prospech, 
                       input$r_osp_prospech, input$r_zsv_prospech, 1), ncol=3)
    dimnames(cormat) <- list(c("osp", "zsv", "prospech"), c("osp", "zsv", "prospech"))
    cormat
  })
  
  dat <- reactive({
    dat <- as.data.frame(sim.correlation(cormat(), n = input$N*input$nsamples, data = TRUE))
    dat$sum <- input$vaha_zsv*dat$zsv + (1-input$vaha_zsv)*dat$osp
    cutsc <- quantile(dat$sum, 1-input$n/input$N)
    dat$prijat <- (dat$sum >= cutsc)
    dat
  })
  
  prijat <- reactive({
    dat()[dat()$prijat == 1, ]
  })
  
  

# Results -----------------------------------------------------------------

  output$cormat <- renderTable(round(cormat(), 2), rownames = T)
  output$cormat_obs <- renderTable({
    cormat <- cor(dat())
    round(cormat, 2)
    }, rownames = T)
  output$cormat_prijati <- renderTable({
    cormat <- cor(prijat()[-5])
    round(cormat, 2)
  }, rownames = T)
  output$lm1 <- renderPrint({
    lm1 <- lm(prospech ~ osp+zsv, dat())
    print(summary(lm.beta(lm1)))
  })
  
  output$lm2 <- renderPrint({
    lm1 <- lm(prospech ~ osp+zsv, prijat())
    print(summary(lm.beta(lm1)))
  })
})