library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)

#read in the data set
playerdata<-read.csv("NBA1617E.csv",header=TRUE)

#Filter the player data so that it does not choose a player who has no free throw attempts => no free throw %
index1 = which(((playerdata$FTA >= 1)*(playerdata$FTA<=1000))==1)
playerdata2 = playerdata[index1,]
#create a list of just the players names to be selected from later 
PlayerNames<-playerdata2[,1]

dashboardPage(skin="blue",
              dashboardHeader(title="Hypothesis Testing with NBA data",titleWidth=450),
              
              #Sidebar
              dashboardSidebar(
                width = 260,
                sidebarMenu(
                  
                  menuItem("Overview", tabName = "over", icon = icon("university")),
                  menuItem("Part 1", tabName = "first", icon = icon("dribbble")),
                  menuItem("Part 2", tabName = "second", icon = icon("dribbble")),
                  menuItem("Table", tabName = "fourth", icon = icon("table"))
                )),
              
              #Content within the tabs
              dashboardBody(
                tabItems(
                  
                  tabItem(tabName = "over",
                          
                          fluidRow(
                            #column of length 12 which is the whole width
                            #I include everthing in a column though because this way there are margins and it looks better
                            column(8,
                                   h3("About:"),
                                   h4("In this app the goal is to learn about the reasoning of a hypothesis test about proportions "),
                                   h3("Background:"),
                                   #In this app you will explore data about free throw percentage for NBA players in the 2016-17 season.
                                   h4("This app includes 2016-2017 data for the NBA regular season"),
                                   h3("Instructions:"),
                                   h4("In Part 1 you will look at the distribution of all the players free throw percentages."),
                                   h4("In Part 2 you will explore hypothesis tests about and individual player's free throw percentage"),
                                   
                                   br(),
                                   br(),
                                   img(src="fthrow2.png",height = 250,width =650,algin = "middle")
                                   
                            )
                            
                          )
                  ),
                  
                  #Define the content contained within part 1 ie. tabname "first"
                  tabItem(tabName = "first",
                          fluidRow(
                            #Include LaTeX functioality although I don't think I used it for this
                            withMathJax(),
                            
                            #Column 1 has inputs for how to filter and is of width 4
                            column(4,
                                   
                                   selectInput("filtertype",h2("Select how you would like to filter"),choices = c(GamesPlayed= "games", FreeThrowAttempts="FTA")),
                                   
                                   conditionalPanel("input.filtertype == 'games'",
                                                    sliderInput(inputId = "gamesplayed",
                                                                "Filter on number of games played:",
                                                                min = 0,
                                                                max = 82,
                                                                value = c(0,85)
                                                    )),
                                   conditionalPanel("input.filtertype == 'FTA'",
                                                    sliderInput(inputId = "FTA1",
                                                                "Filter on number of free throws attempted:",
                                                                min = 0,
                                                                max = 881,
                                                                value = c(0,881)
                                                    )),
                                   img(src="Giannis.png",height = 219,width =300,align = "middle")
                                   
                            ),
                            
                            #Column two displays the Histogram of the distrubition of the free throw attempts
                            column(8,
                                   h1("Histogram"),
                                   
                                   plotOutput("histogramNBA")
                            ),
                            
                            #A box with information to get students thinking and transitioning into part 2
                            box(width = 12,background = "blue", h4("Try to think about what the median and mean of FT% are and what range you might expect most of the players to fall in. "))
                            
                          )
                          
                  ),
                  
                  #Define Content in tab 2
                  tabItem(tabName = "second",
                          fluidRow(
                            column(12,
                                   textOutput("text3NBA"),
                                   tags$head(tags$style("#text3NBA{color: black;font-size: 30px;font-style: bold;}")),
                                   br()
                            )
                          ),
                          fluidRow(
                            #This is a text output that displays what the hypothesis is they are testing and who the player is
                            column(4,
                                   #Conditional based on how the user would like to select a player for the hypothesis test
                                   selectInput(inputId = "howToChooseNBA","Would you like to choose a random player or select your own", choices = c(Random = "rand", Select = "sel")),
                                   conditionalPanel("input.howToChooseNBA == 'sel'",
                                                    selectizeInput(inputId = "player","Select your player from the drop down list below:", choices=PlayerNames, multiple=FALSE, options = list(placeholder = 'Select Your Player'),selected = NULL)
                                   ),
                                   
                                   #The H0 value the user would like to test against
                                   numericInput("null.valNBA","Select a value for the null hypothesis. ",min = 0,max = 1,value = 0.74, step = 0.01),
                                   
                                   
                                   #User now selects what their sample size would be ie how many shots they are simulating for the player
                                   #simulates shots based on the players actual FT%
                                   h4("Simulate your player shooting free throws and guess whether or not we can reject the null hypothesis"),
                                   numericInput("samp.sizeNBA",h4("Input the number of shots in the sample:  " ),min = 0,max = 1000,value = 5, step = 5),
                                   
                                   #this text output show what the proportion of free throws made is for their player 
                                   textOutput("text2NBA"),
                                   
                                   #Conditional using checkbox if they want to see the true population proportion is for their player
                                   checkboxInput("trueNBA", h6("Show the true free throw percentage")),
                                   conditionalPanel("input.trueNBA==true",
                                                    textOutput("text1NBA")
                                   ),
                                   checkboxInput("iftestNBA",h6("Show Test Output")),
                                   conditionalPanel("input.iftestNBA==true",
                                                    tableOutput("testtableNBA")
                                   )
                                   
                                   #include an image
                                   #img(src = "fthrow.png", height = 150, width =100)
                                   
                                   
                                   
                            ),
                            
                            column(8,
                                   plotOutput("proportion2NBA"),
                                   bsPopover(id = "proportion2NBA", title = NULL, content = "The red line shows the proportion from the null hypothesis, the green line shows the sample proporiton and the blue line shows the players actual proportion from the season", placement = "left", trigger = "hover",
                                             options = NULL)
                                   #Output some info about the graphs and the conditional part
                                   # h4("The red line shows the proportion from the null hypothesis"),
                                   # h4("The green line shows the sample proportion"),
                                   # conditionalPanel("input.true==true",
                                   #                  h4("The blue line shows the players actual free throw proportion from the 2016-17 season")
                                   # )
                                   
                                   
                            )
                            
                            
                          )
                  ),
                  
                  
                  #The fourth tab just shows all of the data that I used
                  tabItem(tabName = "fourth",
                          fluidRow(
                            
                            column(6,
                                   h3("Data"),
                                   tableOutput("samp.table")
                            )
                          )
                  )
                )
              )
)
