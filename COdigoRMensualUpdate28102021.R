# 1. Script description---------------------------------------------------------

#I pretend to make a set of plots that describe the insecurity scenario in
#Mexico, as well as how it has changed depending on the local government.
#To do so, I want to automatize the generation of the plots (since there are 32
#states in Mexico).
#The main source of information about the number of crimes commited in Mexico
#is the
#Secretariado Ejecutivo del Sistema Nacional de Seguridad Publica
#
#https://www.gob.mx/sesnsp/acciones-y-programas/datos-abiertos-de-incidencia-delictiva
#I downloaded the database of missing persons from the SESNSP.

# 2. Setup ---------------------------------------------------------------------
#First I clean the environment, so I know that this program can run from zero.
rm(list=ls())

#Now I define the libraries that I'll be using. I will be changing this from
#time to time.
library(plotly)

# 3. Imput information ------------------------------------------------------------

#I dowloaded the data in my personal folder. In fact, I downloaded the data
#and then I eliminated some rows to make it fit in a rectangular table.
#Path of the file:
path = "C:\\Users\\Juan\\Documents\\1-Trips\\La4Times\\200919-ReporteIncidenciaDelictiva\\"
fileName = "IDEFC_NM_sep21.csv"
pathFile = paste0(path, fileName)

#NOrnmally, I like to give a name to the first data set I make in an R file.
#That's what rawFile is. It's the table containing the raw information.
rawFile <- read.table(pathFile, sep = ',' , header = TRUE)

#raW File has some defects that should be corrected so we can use it.

#The information must be rearranged. Instead of getting information about missing
#individuals, I want statistical information.
#Specifically, I want to know the jumber of missing persons reported by year
#in each state of Mexico.

#I define the columns that I need to use.
stateName <- 3
yearNames <- 1
january <- 8
december <- 19
Subtipo.delito <- 6
#I define the info I Want to extract.
crimesCounted <- c("Feminicidio","Homicidio doloso")

#First I want to get the names of all the states.
stateNames <- sort(unique(rawFile[[stateName]]))

#I want to get the avaliable years.
years <- sort(unique(rawFile[[yearNames]]))

#AN empty data frame with the desired dimensions is created:
resultsTable <- as.data.frame(matrix(vector(),length(years)*12, length(stateNames)))
names(resultsTable) <- stateNames

eachMonth <- ((years[1]*12):((years[length(years)]+1)*12-1))/12
row.names(resultsTable) <- eachMonth


for(i in 1:length(stateNames)){

  #I get the info of a single state
  stateData <- subset(rawFile, rawFile[[stateName]] == stateNames[i])
  
  for(j in 1:length(years)){
    #I count all the data that fits the criteria of year and type.
    hom.doloso.state <- subset(stateData, stateData[[yearNames]] == years[j] & 
                                  (stateData[[Subtipo.delito]] == crimesCounted[1] |
                                     stateData[[6]] == crimesCounted[2]
                                  )
    )
    
    for(k in 1:12){
      monthlyCases <- sum(as.numeric(as.matrix(hom.doloso.state[january-1+k])), na.rm = TRUE)
      
      #I write this info in the table of results
      resultsTable[(j-1)*12+k,i] <- monthlyCases
    }
  }
}

# Here I restrict the years in the resultsTable to exclude the data not yet indexed.
resultsTable <- subset(resultsTable, rowSums(resultsTable) != 0)




#Now I have a nice table called resultsTable

# 4. We get the information ready for the plots---------------------------------

# How the text should look like.
fontPlotTitle <- list(
  family = "Helvetica",
  size = 15,
  color = "black"
)

fontTitle <- list(
  family = "Helvetica",
  size = 25,
  color = "black"
)

fontTics <- list(
  family = "Helvetica",
  size = 20,
  color = "black"
)

margin <- list(
  l = 150,
  r = 80,
  b = 0,
  t = 200,
  pad = 0
)




# 5. Plotting-------------------------------------------------------------------
#Actually, I try to plot as many plots as possible. I made this for loop so I
#don't have to code how to plot each one.
for (i in 1:16){
  #Here I define the tick spacing in the y axis
  
  ydtick <- floor(max(resultsTable[[i]])/8)
  
  plotTitle <- paste0('Número de homicidios dolosos + feminicidios mensuales en el <br> estado de ',
                      stateNames[i],
                      ' de acuerdo con el <br> Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública en<br> averiguaciones previas iniciadas al mes de septiembre 2021.')
   
  
  p <- plot_ly()
  
  p <- add_lines(p,
                 x= ~row.names(resultsTable),
                 y= ~resultsTable[[i]],
                 color = list(color = "cornflowerblue")
  )
  
  
  p <- layout(p,
              title = plotTitle,
              font = fontPlotTitle,
              xaxis = list(title = 'Año',
                           dtick = 12, 
                           tickmode = "linear",
                           titlefont = fontTitle,
                           tickfont = fontTics,
                           showline = TRUE,
                           gridcolor = toRGB("gray80"),
                           gridwidth = 2,
                           linecolor = toRGB("black"),
                           mirror = "ticks"
              ),
              
              yaxis = list(title ='Número de casos',
                           dtick = ydtick,
                           range = c(0, max(resultsTable[[i]]) + ydtick),
                           tickmode = "linear",
                           titlefont = fontTitle,
                           tickfont = fontTics,
                           showline = TRUE,
                           gridcolor = toRGB("gray80"),
                           gridwidth = 2,
                           linecolor = toRGB("black"),
                           mirror = "ticks"
              ),
              margin = margin,
              showlegend = FALSE
  )
  
  
  #outputFileName <- paste0(path, statesNames[i], ".png")
  
  print(p)
  
}

#My computer (or R, i don't know) cannot keep more than ~20 plots in the memory,
#so, I have to stop here and export the first 16 plots manually, and then go on
#with the second half.

for (i in 17:32){
  #Here I define the tick spacing in the y axis
  
  ydtick <- floor(max(resultsTable[[i]])/8)
  
  plotTitle <- paste0('Número de homicidios dolosos + feminicidios mensuales en el <br> estado de ',
                      stateNames[i],
                      ' de acuerdo con el <br> Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública en<br> averiguaciones previas iniciadas al mes de septiembre 2021.')
  
  
  p <- plot_ly()
  
  p <- add_lines(p,
                 x= ~row.names(resultsTable),
                 y= ~resultsTable[[i]],
                 color = list(color = "cornflowerblue")
  )
  
  
  p <- layout(p,
              title = plotTitle,
              font = fontPlotTitle,
              xaxis = list(title = 'Año',
                           dtick = 12, 
                           tickmode = "linear",
                           titlefont = fontTitle,
                           tickfont = fontTics,
                           showline = TRUE,
                           gridcolor = toRGB("gray80"),
                           gridwidth = 2,
                           linecolor = toRGB("black"),
                           mirror = "ticks"
              ),
              
              yaxis = list(title ='Número de casos',
                           dtick = ydtick,
                           range = c(0, max(resultsTable[[i]]) + ydtick),
                           tickmode = "linear",
                           titlefont = fontTitle,
                           tickfont = fontTics,
                           showline = TRUE,
                           gridcolor = toRGB("gray80"),
                           gridwidth = 2,
                           linecolor = toRGB("black"),
                           mirror = "ticks"
              ),
              margin = margin,
              showlegend = FALSE
  )
  
  
  #outputFileName <- paste0(path, statesNames[i], ".png")
  
  print(p)
  
}

  