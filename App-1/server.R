library(shiny)
library(plyr)
library(psych)
library(memisc)
library(ggplot2)
library(shinysky)
library(tools)
library(xlsx)
shinyServer(function(input, output,session) {
  
  datasetInput <- reactive({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    if(file_ext(inFile)[1]=="csv")
    {alp <<-na.omit(read.csv(inFile$datapath, header = input$header))
    return(alp)}
    if(file_ext(inFile)[1]== "xls" || file_ext(inFile)[1]== "xlsx"){
      alp <<-na.omit(read.xlsx(inFile$datapath, sheetIndex =1, header = input$header))
      return(alp)
    }
  })
  output$contents <- renderDataTable({
    if(is.null(input$contents))
    {return (datasetInput())}
    else
    {return(alp)}}, options = list(pageLength = 10)
  )
  output$summary <- renderPrint({
    summary(alp)
    
  })
  
  output$editTable <- renderHotable({
    alp},readOnly = F)
  
  observe({
    alp <<- as.data.frame(sapply(hot.to.df(input$editTable),as.numeric))
    
  })
  #     if (is.null(input$editTable))
  #     {editTable <- alp
  #     return(editTable)
  #     }
  #     FinalTable <- eventReactive(input$actButton3,{
  #       isolate({updatedData <- input$editTable})
  #     })
  #     
  #     alp <<- data.frame(FinalTable())
  
  
  #     observe({
  #       editTable<- alp
  #     })
  #     
  #     updatedtable <- eventReactive(input$actButton3,{
  #       editTable
  #     })
  
  #}
  #     newevent <- eventReactive(input$actButton3,{
  #       alp
  #     })
  #     alp <<- newevent()
  #})
  
  output$headerr <- renderPrint({
    
    ddply(alp,.(alp$header),na.rm=T)
    describe(alp)
  })
  output$plot1 <- renderPlot({
    
    observe({
      updateSelectInput(session,"x","X variable",choices = names(alp))
      updateSelectInput(session,"y","Y variable",choices = names(alp))
    })
    
    selectedVars <- reactive({c(input$x, input$y)})
    #plot1 <- eventReactive(input$actButton)
    dtFrame <- eventReactive(input$actButton,{
      xdata <- alp[as.character(selectedVars()[1])]
      ydata <- alp[as.character(selectedVars()[2])]
      newData <- data.frame(xdata,ydata)
      
    })
    #attach(alp)
    plot(dtFrame(),type = input$plotType, pch=16, cex=1, col="black")
    #type = input$plotType, pch=16, cex=1, col="black"
    mod <- lm(dtFrame()[,2] ~ dtFrame()[,1])
    abline(mod,col="grey", lwd=2)
    output$sumLm1 <- renderPrint({mtable(mod)})
    
    #     output$click_info <- renderPrint({
    #       #     cat("input$plot1_click:\n")
    #       #     str(input$plot1_click)
    #       nearPoints(dtFrame(), input$plot1_click, addDist = T)
    #     })
    #     
    #     output$brush_info <- renderPrint(({
    #       #     cat("input$plot1_brush:\n")
    #       #     str(input$plot1_brush)
    #       brushedPoints(dtFrame(), input$plot1_brush)
    #     }))
    
  })
  
  
  
  output$sumLM <- renderPrint({
    
    observe({
      # updateSelectizeInput(session,"x2","X2 variable",choices = names(alp))
      updateSelectInput(session,"y2","Y2 variable",choices = names(alp))
    })
    selectedVarsReg <- reactive({input$y2})
    
    
    updatedY <- eventReactive(input$actButton2,{
      #yselect <- selectedVarsReg()
      ydata <-alp[as.character(selectedVarsReg())]
      if (names(ydata) == names(alp[2])) {
        xdata <- alp[3]
      }
      else{
        xdata <- alp[2]
      }
      #updatedHeader <- data.frame(headerx[headerx != as.character(selectedVarsReg())])
      newDatay <- data.frame(xdata,ydata)
    })
    #ydata <- alp[as.character(updatedY())]
    #print (ydata)
    #updatedHeader <- headerx[ headerx != as.character(updatedY())]
    
    modReg <- lm(updatedY()[,2] ~ updatedY()[,1], data = alp)
    headerx <- names(alp)
    headerx <- headerx[ headerx != as.character(names(updatedY()[2]))]
    leng = length(headerx)
    for (i in 2:leng) {
      if(i!= match(names(updatedY()[2]), names(alp)) &  abs(cor(alp[i],updatedY()[2])) > 0.5 )
      {
        modReg <- update(modReg,as.formula(paste0(".~ .+ alp[,",i,"]")))
        
      }
    }
    print(mtable(modReg))
    
    #summary(modReg)
    #attach(alp)
    
    #dtFrame22 <- eventReactive(input$actButton2,{
    #ydataReg <- alp[as.character(selectedVarsReg()[2])]
    #xdataReg <- alp[as.character(c(selectedVarsReg()[1]))]
    #newDatax <- data.frame(xdataReg,ydataReg)
    #})
    
    #print(dtFrame22()[1])
    #print (dtFrame22()[,1])
    #     modReg <- lm(dtFrame22()[,2] ~ dtFrame22()[,1][1])
    #     leng = length(dtFrame22()[,1])
    #     for (i in 2:leng) {
    #       modReg <- update(modReg, ~ .+ dtFrame22()[,1][i] )
    #     }
    #     summary(modReg)
    
    #plot(dtFrame22(), col ="red")
  })
  output$textNLQ <- renderPrint({
    observe({pangram <- tolower(as.character(input$NLQ))})
    pangram <- tolower(as.character(input$NLQ))
    words <- eventReactive(input$runNLQ,{words <- strsplit(pangram, " ")[[1]]})
    #words <- strsplit(pangram," ")[[1]]
    #attach(alp)
    wordx <- as.list(words())
    if ('plot' %in% wordx){
      if ('plot' %in% wordx || 'graph' %in% wordx || 'trendline' %in% wordx){
        for(word in wordx){
          if(word == 'vs' || word == 'and'){
            index <- match(word,wordx)
            xvar <- wordx[index+1]
            yvar <- wordx[index-1]
            xdata <- alp[,as.character(xvar)]
            ydata <- alp[,as.character(yvar)]
            xydat <- data.frame(xdata,ydata)
            #plot(xydat,type = "p", pch=16, cex=1, col="black", xlab = xvar,ylab=yvar)
            #type = input$plotType, pch=16, cex=1, col="black"
            modnlq <<- lm(as.formula(ydata ~ xdata))
            #abline(modnlq,col="grey", lwd=2)
            return(mtable(modnlq))
          }
        }}
    }
    
    if ('mean' %in% wordx){
      for(word in wordx){
        if(word %in% names(alp))
        {
          return(mean(alp[,word]))
        }
      }
    }
    if ('median' %in% wordx){
      for(word in wordx){
        if(word %in% names(alp))
        {
          return(median(alp[,word]))
        }
      }
    }
    if ('min' %in% wordx || 'minimum' %in% wordx){
      for(word in wordx){
        if(word %in% names(alp))
        {
          return(min(alp[,word]))
        }
      }
    }
    if ('max' %in% wordx || 'maximum' %in% wordx){
      for(word in wordx){
        if(word %in% names(alp))
        {
          return(max(alp[,word]))
        }
      }
    }
    if ('range' %in% wordx){
      for(word in wordx){
        if(word %in% names(alp))
        {
          return(range(alp[,word]))
        }
      }
    }
    if ('count' %in% wordx || 'length' %in% wordx){
      for(word in wordx){
        if(word %in% names(alp))
        {
          return(length(alp[,word]))
        }
      }
    }
    if ('skew' %in% wordx){
      for(word in wordx){
        if(word %in% names(alp))
        {
          return(skew(alp[,word]))
        }
      }
    }
    if ('standard' %in% wordx & 'deviation' %in% wordx){
      for(word in wordx){
        if(word %in% names(alp))
        {
          return(sd(alp[,word], na.rm=T))
        }
      }
    }
    
    if('model' %in% wordx || 'equation' %in% wordx){
      for(word in wordx){
        if(word %in% names(alp)){
          ydata <-alp[as.character(word)]
          #names(ydata)<- as.character(word)
          if (names(ydata) == names(alp[2])) {
            xdata <- alp[3]
          }
          else{
            xdata <- alp[2]
          }
          
          newDatay <- data.frame(xdata,ydata)
          modReg <- lm(newDatay[,2] ~ newDatay[,1], data = alp)
          headerx <- names(alp)
          headerx <- headerx[ headerx != as.character(names(newDatay[2]))]
          leng = length(headerx)
          for (i in 2:leng) {
            if(i!= match(names(newDatay[2]), names(alp)) &  abs(cor(alp[i],newDatay[2])) > 0.5 )
            {
              modReg <- update(modReg,as.formula(paste0(".~ .+ alp[,",i,"]")))
              
            }
          }
          return(mtable(modReg))
        }
      }
    }
    
  })
  
  output$plotNLQ <- renderPlot({
    observe({pangram <- tolower(as.character(input$NLQ))})
    pangram <- tolower(as.character(input$NLQ))
    words <- eventReactive(input$runNLQ,{words <- strsplit(pangram, " ")[[1]]})
    #words <- strsplit(pangram," ")[[1]]
    #attach(alp)
    wordx <- as.list(words())
    if ('plot' %in% wordx || 'graph' %in% wordx || 'trendline' %in% wordx){
      for(word in wordx){
        if(word == 'vs' || word == 'and'){
          index <- match(word,wordx)
          xvar <- wordx[index+1]
          yvar <- wordx[index-1]
          xdata <- alp[,as.character(xvar)]
          ydata <- alp[,as.character(yvar)]
          xydat <- data.frame(xdata,ydata)
          plot(xydat,type = "p", pch=16, cex=1, col="black", xlab = xvar,ylab=yvar)
          #type = input$plotType, pch=16, cex=1, col="black"
          modnlq <<- lm(as.formula(ydata ~ xdata))
          abline(modnlq,col="grey", lwd=2)
          
    }
      }}
    })
  
})