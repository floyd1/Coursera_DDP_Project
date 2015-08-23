# References from the link below
# https://gist.github.com/josecarlosgonz/6417633#file-googlemapsandr-md
# http://www.r-bloggers.com/using-google-maps-api-and-r/

library(shiny)
library(RCurl)
library(RJSONIO)
library(plyr)
library(RgoogleMaps)

url <- function(address, return.call = "json", sensor = "false") {
    root <- "http://maps.google.com/maps/api/geocode/"
    u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
    return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
    if(verbose) cat(address,"\n")
    u <- url(address)
    doc <- getURL(u)
    x <- fromJSON(doc,simplify = FALSE)
    if(x$status=="OK") {
        lat <- x$results[[1]]$geometry$location$lat
        lng <- x$results[[1]]$geometry$location$lng
        location_type <- x$results[[1]]$geometry$location_type
        formatted_address <- x$results[[1]]$formatted_address
        return(c(lat, lng, location_type, formatted_address))
    } else {
        return(c(NA,NA,NA, NA))
    }
}

shinyServer(
    function(input, output) {
        observe({
            input$goButton
            isolate({
                address <- input$address
                addresses <- strsplit(input$addressArea, "\n")
                addresses <- unlist(addresses)
                locations <- ldply(addresses, function(x) geoCode(x))
                locations <- cbind(addresses,locations)
                names(locations) <- c("Address Entered","Latitude","Longtitude","Location Type", "Mapped Address")
                output$locations <- renderTable({locations})

                
                markers <- ""
                for(i in 1:nrow(locations)){
                    markers <- paste0(markers, "&markers=color:blue|label:''|", locations[i,2], ",", locations[i,3])
                }

                output$myImage <- renderImage({
                    # A temp file to save the output.
                    # This file will be removed later by renderImage
                    outfile <- tempfile(fileext='.png')
                    
                    # Generate the PNG
                    png(outfile, width=800, height=500)
                    
                    MyMap <- GetMap(center=c(0, 0),zoom=1,markers = markers, destfile = outfile)
                    RgoogleMaps::PlotOnStaticMap(MyMap)
                    dev.off()
                    
                    # Return a list containing the filename
                    list(src = outfile,
                         contentType = 'image/png',
                         width = 800,
                         height = 500,
                         alt = "This is alternate text")
                }, deleteFile = TRUE)
                output$downloadData <- downloadHandler(
                    filename = function() {
                        paste('data-', Sys.Date(), '.csv', sep='')
                    },
                    content = function(file) {
                        write.csv(locations, file)
                    }
                )
            })
            
        })
    }
)