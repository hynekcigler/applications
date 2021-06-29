library(shiny)
library(psych)

shinyServer(function(input, output, session) {
  #input$button
  
  staff <- reactive({
    input$button
    height <- as.numeric(input$height)
    sex <- input$sex
    rel <- as.numeric(input$test)
    if (rel == 0) {
      rel <- input$test.user
    }
    
    male_M <- 180
    male_SD <- 7
    female_M <- 167
    female_SD <- 6.1
    
    male_SE <- male_SD * sqrt(1 - rel)
    female_SE <- female_SD * sqrt(1 - rel)
    
    rnormA <- repeatable(rnorm)
    
    if (sex == "man") {
      result <- rnormA(1, height, male_SE)
      IQ <- 100 + (result - male_M) / male_SD * 15
    } else {
      result <- rnormA(1, height, female_SE)
      IQ <- 100 + (result - female_M)/ (female_SD) * 15
    }
    IQ <- c(round(IQ, 0), result)
  })
  
  output$IQ <- renderText({
    input$button
    staff()[1]
  })
  output$height <- renderText({
    input$button
    round(staff()[2], 0)
  })
  output$perc <- renderText({
    input$button
    round(pnorm(staff()[1], 100, 15)*100, 0)
  })
  output$gauss <- renderPlot({
    if (input$type == "iq") {
      x <- seq(40, 160, by=.01)
      y <- dnorm(x, 100, 15)
      if (staff()[1] < 50) {
        by.par <- -.01
      } else {
        by.par <- .01
      }
      x2 <- seq(50, staff()[1], by=by.par)
      plot(x, y, type="l", lwd=3, col="darkblue", main="Gauss curve", xlab="intelligence scale (IQ)", ylab="", yaxt="ny")
      polygon(c(x[x <= staff()[1]], staff()[1]), c(y[x <= staff()[1]], 0), col="lightblue", border=NA)
      lines(x, y, lwd=3, col="darkblue")
      abline(v=staff()[1], lwd=3, col="red")
      text(staff()[1]+1, y=.005, labels = paste("Your IQ =", staff()[1]), pos=4, srt=90)
    } else {
      x <- seq(140, 210, by=.01)
      if (input$sex == "man") {
        M <- 180
        SD <- 7
      } else {
        M <- 167
        SD <- 6.1
      }
      y <- dnorm(x, M, SD)
      plot(x, y, type="l", lwd=3, col="darkblue", main="Gauss curve", xlab="height scale (cm)", ylab="", yaxt="ny")
      polygon(c(x[x <= staff()[2]], staff()[2]), c(y[x <= staff()[2]], 0), col="lightblue", border=NA)
      lines(x, y, lwd=3, col="darkblue")
      abline(v=staff()[2], lwd=3, col="red")
      text(staff()[2]+1, y=.005, labels = paste("Your height =", round(staff()[2], 0), "cm"), pos=4, srt=90)
    }
  })
  output$scatter <- renderPlot({
    rel <- as.numeric(input$test)
    if (rel == 0) {
      rel <- input$test.user
    }
    rcor <- sqrt(rel)
    
    if (input$type == "iq") {
      ylim <- c(50, 150)
      male_M <- 100
      male_SD <- 15
      female_M <- 100
      female_SD <- 15
    } else {
      ylim <- c(150, 200)
      male_M <- 180
      male_SD <- 7
      female_M <- 167
      female_SD <- 6.1
    }
    
    if(input$sex == "man") {
      x <- rnorm(300, male_M, male_SD)
      y <- rnorm(300, mean = rcor*x + (1-rcor)*male_M, sqrt(1-rel) * male_SD)
      subtitle <- "Simulated sample of 300 men."
    } else {
      x <- rnorm(300, female_M, female_SD)
      y <- rnorm(300, rcor*x + (1-rcor)*female_M, sqrt(1-rel) * female_SD)
      subtitle <- "Simulated sample of 300 women."
    }
    plot(x, y, xlab = "True height", ylab = "Measured height", 
         main = "Comparison of true and measured height", 
         sub = paste0(subtitle, " Simulated correlation r = ", round(cor(x, y), 3)), 
         ylim = ylim, xlim=ylim)
    abline(0,1, col="gray", lty=3)
  })
})