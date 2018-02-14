#TEMP: Debug in RStudio
fileRda = "C:/Users/boefraty/projects/PBI/R/tempData.Rda"
if(file.exists(dirname(fileRda)))
{
  if(Sys.getenv("RSTUDIO")!="")
    load(file= fileRda)
  else
    save(list = ls(all.names = TRUE), file=fileRda)
}



############### LIBRARY DECLARATIONS ###############
source('./r_files/flatten_HTML.r')
source('./r_files/utils.r')


libraryRequireInstall("ggplot2")
libraryRequireInstall("grid")
libraryRequireInstall("gridExtra")
libraryRequireInstall("wesanderson")
libraryRequireInstall("RColorBrewer")
libraryRequireInstall("dplyr")
libraryRequireInstall("plotly")

############ INTERNAL FUNCTIONS #########

# check if date in handable format
# TDOD for next version: support multiple date formats
dateInCorrectFormat <- function(date) {
  d <- try(as.Date(date, format = "%Y-%m-%dT%H:%M:%OS"))
  if(class(d) == "try-error" | is.na(d)){
    d <- try(as.POSIXct(date))
  }
  
  return (class(d) != "try-error" & !is.na(d))
}


# Get segment size so that they do not overlap on resize / large number of resources.
getSegmentSize = function(numCols, numRows, orientation = "horizontal", maxW = 25, minW = 1)
{
  convFactor = 20 # unit conversion
  hw = par()$din
  if(orientation == "horizontal")
    segSize = hw[2]/numCols
  else
    segSize = 0.5* hw[1]/numRows # take legend into account 
  
  segSize = min(max(segSize*convFactor, minW),maxW)
  
  return(segSize)
}

# Sort the dataset. A-Z (az), Z-A (za), total duration (total_duration) and user count (user_count)
sortDataset = function(dataset, sorting = "az", orientation="horizontal")
{
  switch(sorting,
         "az" = {
           if(settings_orientation == "horizontal"){
             dataset$Resource  <- factor(dataset$Resource, levels= unique(dataset[rev(order(dataset$Resource)), "Resource"]))
           } else {
             dataset$Resource  <- factor(dataset$Resource, levels= unique(dataset[order(dataset$Resource), "Resource"]))
           }
         },
         "za" = {
           if(settings_orientation == "horizontal"){
             dataset$Resource  <- factor(dataset$Resource, levels= unique(dataset[order(dataset$Resource), "Resource"]))
           } else {
             dataset$Resource  <- factor(dataset$Resource, levels= unique(dataset[rev(order(dataset$Resource)), "Resource"]))
           }
           
         },
         "total_duration" = {
           dataset$duration <- difftime(dataset$End, dataset$Start, units="secs")
           dataset$duration[is.na(dataset$duration)] <- 0 
           temp <- dataset %>% group_by(Resource) %>% summarize(t=sum(duration)) %>% arrange(t)
           if(settings_orientation == "horizontal"){
             dataset$Resource  <- factor(dataset$Resource, levels= (temp$Resource))
           } else {
             dataset$Resource  <- factor(dataset$Resource, levels= rev(temp$Resource))
           }
         },
         "user_count" = {
           temp <- dataset %>% group_by(Resource) %>% summarize(n=n()) %>% arrange(n)
           if(settings_orientation == "horizontal"){
             dataset$Resource  <- factor(dataset$Resource, levels= temp$Resource)
           } else {
             dataset$Resource  <- factor(dataset$Resource, levels= rev(temp$Resource))
           }
         })
  return(dataset)
}


GetCorrectTooltip = function(tipText,newTips)
{
  #parse token and replace by line with correct myTempId
  strid = strsplit(tipText, 'myTempId:', fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]][2]
  myid = as.numeric(strid)
  if(!is.na(myid))
    return(newTips[myid])
  
  return(NA)
  
}


############ INPUT / DATE VALIDATION #########

input_valid <- TRUE
dates_valid <- TRUE

message <- ""

settings_orientation = 'horizontal'
if(exists('vizSettings_orientation'))
  settings_orientation = vizSettings_orientation


settings_sorting = 'az'
if(exists('vizSettings_sorting'))
  settings_sorting = vizSettings_sorting

settings_colorPalette = 'Accent'
if(exists('vizSettings_colorPalette'))
  settings_colorPalette = vizSettings_colorPalette

settings_legendCols = 'auto'
if(exists('vizSettings_legendCols'))
  settings_legendCols = vizSettings_legendCols

settings_percentile = 0.5
if(exists('vizSettings_percentile'))
  settings_percentile = (vizSettings_percentile+1)/100


removeMargin = TRUE
if(exists('vizSettings_removeMargin'))
  removeMargin = vizSettings_removeMargin

abreviateLengthUser = 7
if(exists('vizSettings_abreviateLengthUser'))
  abreviateLengthUser = round(as.numeric(vizSettings_abreviateLengthUser))

abreviateLengthResource = 3
if(exists('vizSettings_abreviateLengthResource'))
  abreviateLengthResource = round(as.numeric(vizSettings_abreviateLengthResource))

userFormat = "%m/%d/%y %H:%M"
if(exists('vizSettings_userFormatX'))
  userFormat = vizSettings_userFormatX


maxUsers = 150
maxResources = 100
maxRows = 500
withLegend = TRUE
if(!goodPlotDimension(4.25,0))
  withLegend = FALSE

if(!goodPlotDimension(2,2))
{
  input_valid <- FALSE
  message <- paste(message, "Plot area is too small")
}

if (!(exists("Resource") && exists("User") && exists("Start") && exists("End")) )
{
  input_valid <- FALSE
  message <- paste(message, "Resource, User, Start and End must be defined")
}

if(input_valid)
{
  if(length(unique(User[,1])) > maxUsers)
  {
    input_valid <- FALSE
    message <- paste(message, "Maximum number of unique users is ", maxUsers)
  }
  if(length(unique(Resource[,1])) > maxResources)
  {
    input_valid <- FALSE
    message <- paste(message, "Maximum number of unique resources is ", maxResources)
  }
  if(nrow(Resource) > maxRows)
  {
    input_valid <- FALSE
    message <- paste(message, "Maximum rows is ", maxRows)
  }
  
}

if (!exists("Tooltips")) {
  Tooltips <- NULL
}


if (input_valid) {
  # Assign columns to dataset
  dataset <- cbind(Resource, User, Start, End)
  # Rename columns
  colnames(dataset) <- c("Resource", "User", "Start", "End")
  dataset = cbind(dataset,myTempId = seq(1,nrow(dataset)))
  
  if(!is.null(Tooltips))
  { 
    Tooltips1 = subset(Tooltips, select = setdiff(colnames(Tooltips),colnames(dataset)))
    dataset = cbind(dataset,Tooltips1)
  }
 
  # Remvoe NA's
#  dataset <- na.omit(dataset)
  dates_valid <-   (dateInCorrectFormat(dataset$Start[1]) &
                      dateInCorrectFormat(dataset$End[1]))
} 

if(!dates_valid)
  message <- paste(message, "Wrong format for date/time")
 


############ INPUT / DATE VALIDATION #########
if(input_valid && dates_valid){
  
    dataset$Start <-
      as.POSIXct(dataset$Start, format = "%Y-%m-%dT%H:%M:%OS")
    
    dataset$End <-
      as.POSIXct(dataset$End,  format = "%Y-%m-%dT%H:%M:%OS")
    
    colourCount <- length(unique(dataset$User))
    
    getPalette <- colorRampPalette(brewer.pal(8, settings_colorPalette))
    
    segSize = settings_percentile * getSegmentSize(length(unique(dataset$Resource)), length(unique(dataset$User)), orientation = settings_orientation)
    
    dataset1 <- sortDataset(dataset, sorting = settings_sorting, orientation = settings_orientation)
    
    dataset1$User <- abbreviate(dataset1$User, abreviateLengthUser)
    dataset1$Resource <- abbreviate(dataset1$Resource, abreviateLengthResource)
    
    ncolLegend = 1
    
    if(settings_orientation == "horizontal"){
      g = ggplot(dataset1, aes(x = Start, xend = End, y = Resource, yend = Resource, color = User, id = myTempId)) +
        geom_segment(size = segSize) +
        xlab("Time") +
        ylab(cutStr2Show(names(Resource), abbrTo = 50)) +
        labs(color=names(User))+
        theme_bw()  +
        scale_colour_manual(values = getPalette(colourCount)) + 
        theme(legend.direction ="vertical",legend.position = "right") + guides(color=guide_legend(ncol=ncolLegend))
      
    } else {
      g = ggplot(dataset1, aes(x = Resource, xend = Resource, y = Start, yend = End, color = User, id = myTempId)) +
        geom_segment(size = segSize) +
        ylab("Time") +
        xlab(cutStr2Show(names(Resource), abbrTo = 50)) +
        labs(color=names(User))+
        theme_bw() + 
        scale_colour_manual(values = getPalette(colourCount)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+ guides(color=guide_legend(ncol=ncolLegend))
    }
    if(!withLegend)
     g = g + theme(legend.position="none")
    
  } else {
    g = ggplot()
    g = g + labs(title = message , caption = NULL) + theme_bw()  + 
      theme(plot.title  = element_text(hjust = 0.5, size = 8), 
            axis.title  =element_text(size =  10),
            axis.text = element_text(size =  5),
            panel.border = element_blank())
  }


############# Create and save widget ###############
p = ggplotly(g);

disabledButtonsList <- list('toImage', 'sendDataToCloud', 'zoom2d', 'pan', 'pan2d', 'select2d', 'lasso2d', 'hoverClosestCartesian', 'hoverCompareCartesian')
p$x$config$modeBarButtonsToRemove = disabledButtonsList

p <- config(p, staticPlot = FALSE, editable = FALSE, sendData = FALSE, showLink = FALSE,
               displaylogo = FALSE,  collaborate = FALSE, cloud=FALSE)




if(input_valid && dates_valid)
{
  ddd = dataset
  ddd$myTempId = NULL
  
  
  ddd$Start = format(ddd$Start,  userFormat)
  ddd$End = format(ddd$End,  userFormat)
  
  ntt = generateNiceTooltips(ddd)
   #tooltips on plot
   for (i in seq(1,length(p$x$data)))
   {
     ttt = p$x$data[[i]]$text
     for (j in seq(1,length.out = length(ttt)))
     {
       if(!is.na(ttt[j]))
         p$x$data[[i]]$text[j] = GetCorrectTooltip(ttt[j],ntt)
     }
    
   }
  
}

internalSaveWidget(p, 'out.html');

if(removeMargin)
  ReadFullFileReplaceString('out.html', 'out.html', ',"padding":40,', ',"padding":0,')
####################################################


#DEBUG 
if(Sys.getenv("RSTUDIO")!="")
  print(p)
