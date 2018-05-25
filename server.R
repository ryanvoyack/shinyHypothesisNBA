library(ggplot2)
library(magrittr)
library(shinyBS)
library(shiny)
library(shinyjs)

playerdata<-read.csv("NBA1617E.csv",header=TRUE)

#  "server file must return a server function", create server:
function(input,output,session){
  dataFilter<- reactive({
    
    games<-input$gamesplayed
    
    gameindex<-which(((playerdata$G >= games[1])*(playerdata$G<=games[2]))==1)
    
    fta<-input$FTA1
    index<-which(((playerdata$FTA >= fta[1])*(playerdata$FTA<=fta[2]))==1)
    playerdata<-playerdata[index,]
    bballdata<-playerdata[gameindex,]
    
  })
  
  player.select <-reactive({
    #Filter the player data so that it does not choose a player who has no free throw attempts => no free throw %
    index1 = which(((playerdata$FTA >= 1)*(playerdata$FTA<=1000))==1)
    playerdata2 = playerdata[index1,]
    
    #Randomly select a player if it is random
    decision = input$howToChooseNBA
    if(decision == "rand"){
      s1 = playerdata2[sample(nrow(playerdata2), 1), ]
      name = s1$Player
    }
    else{
      name = input$player
    }
    
    #Random Button
    input$random
    
    #If it is not random use the player that the user selected
    index<-which(playerdata2$Player == name)
    namedata<-playerdata2[index,]
  })
  
  
  #This is a reactive element for how many shots will be simulated
  nNBA <- reactive({
    return(input$samp.sizeNBA)
  })
  
  #This is a reactive element for what the user chooses for the null value
  hNBA <- reactive({
    return(input$null.valNBA)
  })
  
  truepropNBA <-reactive({
    return(input$trueNBA)
  })
  
  
  valsNBA <- reactiveValues(sim1 = 0)
  observeEvent(input$samp.sizeNBA, {
    namedata<-player.select()
    ftp = namedata$FT/namedata$FTA
    n1 <- nNBA()
    valsNBA$sim1 = rbinom(n = n1,size = 1, prob = ftp)
  })
  
  #Output text for what the free throw percentage is for the player
  output$text1NBA <- renderText({
    namedata <-player.select()
    ftp = namedata$FT/namedata$FTA
    
    paste("The free throw proportion for ",namedata$Player, "is",round(ftp,2))
  })
  
  
  
  #Output text for the null hypothesis
  output$text3NBA <- renderText({
    namedata <- player.select()
    h1 <- hNBA()
    paste("Test the hypothesis that the free throw percentage for ",namedata$Player, "is equal to",h1)
  })
  
  ####Output plot, the histogram for "filtering", part 1####
  output$histogramNBA<-renderPlot({
    validate(
      need(input$gamesplayed>0,
           message = "Please input a valid number of games played")
    )
    
    bballdata<-dataFilter()
    
    n = nrow(bballdata)
    y = numeric(length = n)
    
    #Calculates the free throw percentages for all the players and puts it into a variable
    #Produces NAN's for players that haven't taken any free throws
    #Doesn't matter for the Histogram though because the Histogram won't display the NaN's 
    #I take out the NaN's in a different part where it is needed
    for(i in 1:n){
      y[i] = bballdata$FT[i]/bballdata$FTA[i]
    }
    
    #The actual histogram
    par(bg = "lightsteelblue")
    hist(y,xlab = "Free Throw Proportion",main = "Histogram of Free Throw Proportion",col ="firebrick")
  })

  
  ####output plot of the plot in part 2, "Three proportions"####
  output$proportion2NBA <-renderPlot({
    validate(
      need(input$samp.sizeNBA>0,
           message = "Please input a valid number of shots")
    )
    validate(
      need(!is.null(input$samp.sizeNBA),
           message = "Please input the number of shots")
    )
    h3<-hNBA()
    namedata<-player.select()
    ftp = namedata$FT/namedata$FTA
    n1 <-nNBA()
    true1 = truepropNBA()
    phat = 0
    
    sim1 = valsNBA$sim1
    
    for(i in 1:n1){
      if(sim1[i]==1){
        phat = phat+1
      }
      else{
        phat = phat
      }
    }
    phat = phat/n1
    
    
    #sampling distribuion
    phat = 0
    phats <- c()
    j=1
    for(j in 1:500){
      i=1
      sim1=rbinom(n = n1,size = 1, prob = ftp)
      phat=0
      for(i in 1:n1){
        if(sim1[i]==1){
          phat = phat+1
        }
        else{
          phat = phat
        }
        i=i+1
      }
      phat = phat/n1
      phats[j]<-phat
      j=j+1
    }
    #par(bg = "lightsteelblue")

    #Calculates the free throw percentages for all the players and puts it into a variable
    #Produces NAN's for players that haven't taken any free throws
    #Doesn't matter for the Histogram though because the Histogram won't display the NaN's 
    #I take out the NaN's in a different part where it is needed
    
    options(digits = 6)
    dat <- round(playerdata$FT / playerdata$FTA, 6)
    dat <- as.numeric(na.omit(dat))
    temp <- data.frame(length=dat)
    #temp <- data.frame(length=dat[which(.419474<dat & dat<1)])
    #temp2 <- data.frame(length=dat)
    #temp2.1 <- data.frame(length=dat[which(dat==1)])
    #temp2.2 <- data.frame(length=dat[which(.419474>=dat)])
    temp2 <- data.frame(length=phats)
    temp$data <- "population" #'inner 95% of population'
    #temp2$data <- 'outer 5% of population'
    temp2$data <- 'sampling distribution'

    data1 <- rbind(temp, temp2)
    g <- data1 %>% ggplot(aes(length, fill=data)) + geom_density(alpha=.35) + geom_vline(aes(xintercept = h3), color="red", lwd=1) + geom_vline(aes(xintercept = phat), color="green", lwd=1) + xlim(.25,1) 
    g + if(true1 == TRUE){geom_vline(aes(xintercept=ftp), color = "blue", lwd = 1)}else{NULL} 
    
  })

  
  #### hypothesis test output in part 2####  
  output$testtableNBA <- renderTable({
    validate(
      need(input$samp.sizeNBA>0,
           message = "Please input a valid number of shots")
    )
    
    namedata <-player.select()
    h1 <- hNBA()
    ftp = namedata$FT/namedata$FTA
    sim1 = valsNBA$sim1
    n4 = nNBA()
    phat = 0
    
    for(i in 1:n4){
      if(sim1[i]==1){
        phat = phat+1
      }
      else{
        phat = phat
      }
    }
    phat = phat/n4
    
    
    # 0.14186
    stanerr1 = sqrt(h1*(1-h1)/n4)
    
    z1 = ( phat- h1)/stanerr1
    z1 = round(z1, digits = 3)
    #paste(round(z1,digits = 3))
    
    if(phat<h1)
    {
      p1 = pnorm(z1,lower.tail = TRUE) 
      
    }
    else{
      p1 = pnorm(z1,lower.tail = FALSE) 
      
    }
    
    if(input$iftestNBA)
    {
      ctable = matrix(c(z1,p1),nrow=1)
      colnames(ctable) = c("z-statistic","p-value")
      ctable
    }
  })
  
  output$samp.table<- renderTable({
    
    sample1 <- playerdata
    
  })
  
}

