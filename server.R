library(shiny)

mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

shinyServer(function(input, output) {
  
  getX <- reactive({
    var  <- ""
    
    if(input$cyl   == TRUE)
      var <- paste(c(var, "factor(cyl)"), collapse = " + ")
    if(input$disp  == TRUE)
      var <- paste(c(var, "disp"), collapse = " + ")
    if(input$hp    == TRUE)
      var <- paste(c(var, "hp"), collapse = " + ")
    if(input$drat  == TRUE)
      var <- paste(c(var, "drat"), collapse = " + ")
    if(input$wt    == TRUE)
      var <- paste(c(var, "wt"), collapse = " + ")
    if(input$qsec  == TRUE)
      var <- paste(c(var, "qsec"), collapse = " + ")
    if(input$vs  == TRUE)
      var <- paste(c(var, "factor(vs)"), collapse = " + ")
    if(input$am  == TRUE)
      var <- paste(c(var, "factor(am)"), collapse = " + ")
    if(input$gear  == TRUE)
      var <- paste(c(var, "factor(gear)"), collapse = " + ")
    if(input$carb  == TRUE)
      var <- paste(c(var, "factor(carb)"), collapse = " + ")
    
    var <- substring(var,3)
    var
  })
  
  getY <- reactive({
    var  <- "mpg"
    var
  })
  
  regressionFormulaText <- reactive({
    varX <- getX()
    varY <- getY()
    var <- paste(varY," ~", varX)
    var
  })
  
  summaryText <- reactive({
    l <- getLenghtInputs()
    if(l > 0){
      var <- regressionFormulaText()
      summary(lm(var, mpgData))
    } else {
      "Check at least one variable to see the output."
    }
  })
  
  getLenghtInputs <- reactive({
    x <- getX()
    x <- gsub("+", ",", x, fixed = TRUE) 
    x <- strsplit(x,",")
    l <- length(x[[1]])
    l
  })
  
  output$regressionFormula <- renderPrint({
    regressionFormulaText()
  })
  
  output$summary <- renderPrint({
    summaryText()
  })
  
  output$residPlot <- renderPlot({
    l <- getLenghtInputs()
    if (l > 0){
      var <- regressionFormulaText()
      plot(density(resid(lm(formula = var, data = mpgData))), main=paste(c("Residuals: ", var)))
    }
  })
  
  output$ggplot <- renderPlot({
    l <- getLenghtInputs()
    if(l == 1){
      par(mfcol=c(1,1))
    }
    if(l == 2){
      par(mfcol=c(1,2))
    }
    if(l == 3){
      par(mfcol=c(1,3))
    }
    if(l == 4){
      par(mfcol=c(2,2))
    }
    if(l == 5){
      par(mfcol=c(2,3))
    }
    if(l == 6){
      par(mfcol=c(2,3))
    }
    if(l == 7){
      par(mfcol=c(3,3))
    }
    if(l == 8){
      par(mfcol=c(2,4))
    }
    if(l == 9){
      par(mfcol=c(3,3))
    }
    if(l == 10){
      par(mfcol=c(4,3))
    }

    if(input$cyl   == TRUE){
      plot(y=mpgData$mpg, x=factor(mpgData$cyl), ylab="Miles/(US) gallon", xlab="Number of cylinders")
    }
    
    if(input$disp   == TRUE){
      reg <- lm(mpgData$mpg~mpgData$disp)
      plot(y=mpgData$mpg, x=mpgData$disp, ylab="Miles/(US) gallon", xlab="Displacement (cu.in.)")
      abline(reg)
    }
    
    if(input$hp   == TRUE){
      reg <- lm(mpgData$mpg~mpgData$hp)
      plot(y=mpgData$mpg, x=mpgData$hp, ylab="Miles/(US) gallon", xlab="Gross horsepower")
      abline(reg)
    }
    
    if(input$drat   == TRUE){
      reg <- lm(mpgData$mpg~mpgData$drat)
      plot(y=mpgData$mpg, x=mpgData$drat, ylab="Miles/(US) gallon", xlab="Rear axle ratio")
      abline(reg)
    }
    
    if(input$wt   == TRUE){
      reg <- lm(mpgData$mpg~mpgData$wt)
      plot(y=mpgData$mpg, x=mpgData$wt, ylab="Miles/(US) gallon", xlab="Weight (lb/1000)")
      abline(reg)
    }
    
    if(input$qsec   == TRUE){
      reg <- lm(mpgData$mpg~mpgData$qsec)
      plot(y=mpgData$mpg, x=mpgData$qsec, ylab="Miles/(US) gallon", xlab="1/4 mile time")
      abline(reg)
    }
    
    if(input$vs   == TRUE){
      plot(y=mpgData$mpg, x=factor(mpgData$vs), ylab="Miles/(US) gallon", xlab="V/S")
    }
    
    if(input$am   == TRUE){
      plot(y=mpgData$mpg, x=factor(mpgData$am), ylab="Miles/(US) gallon", xlab="Transmission")
    } 
    
    if(input$gear   == TRUE){
      plot(y=mpgData$mpg, x=factor(mpgData$gear), ylab="Miles/(US) gallon", xlab="Number of forward gears")
    }  
    
    if(input$carb   == TRUE){
      plot(y=mpgData$mpg, x=factor(mpgData$carb), ylab="Miles/(US) gallon", xlab="Number of carburetors")
    } 
    
  })
  
})
