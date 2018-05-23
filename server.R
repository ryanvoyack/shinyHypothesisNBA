library(ggplot2)
library(shinyBS)
library(shiny)
library(shinyjs)

playerdata<-read.csv("file:///C:/Users/ryan voyack/Documents/PSU (courses,etc) & Act-Sci (exams, etc)/shiny research/FreeThrows/NBA1617E.csv",header=TRUE)


shinyServer(function(input, output,session) {
  
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
    n1 <-nNBA()
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
    namedata <-player.select()
    h1 <- hNBA()
    paste("Test the hypothesis that the free throw percentage for ",namedata$Player, "is equal to",h1)
    
    
  })
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
    
    #Find the sample proportion
    
    for(i in 1:n1){
      if(sim1[i]==1){
        phat = phat+1
      }
      else{
        phat = phat
      }
    }
    phat = phat/n1
    #Plot it
    par(bg = "lightsteelblue")
    plot(x=NULL,
         y=NULL,
         xlim=c(0, 1),
         ylim=c(0, 1),
         ylab = paste("Proportion"),
         xlab =  paste("The sample proportion of shots made is  ", round(phat, digits = 2)),
         xaxt = "n",
         main = paste("Free Throw Proportion for ",namedata$Player)
    )
    
    abline(h = h3, col = "red", lwd = 3)
    abline(h = phat, col = "green", lwd = 3)
    if(true1 == TRUE){
      abline(h=ftp,col = "blue", lwd =3)
    }
    # x1 = 0:1
    # y1 = 0:1
    # ggplot(data.frame(x1,y1),aes(x=x1, y=y1))+
    #   geom_line(y = h3, color = "red") +
    #   geom_line(y = phat, color = "green")+
    #   ylim(0,1) +
    #   labs(title = paste("Free Throw Proportion for ",namedata$Player),y = "Proportion", x = " ", caption = paste("The sample proportion of shots made is  ", round(phat, digits = 2)))+
    #   if(true1 == TRUE){
    #     geom_line(y = ftp, color = "blue")
    #   }
    
    
  })
  
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
  
})
