library(shiny)
shinyUI(fluidPage(
    fluidRow(
        headerPanel("Developing Data Products Course Project"),
        titlePanel('Google Maps Geocode Query Tool'),
        helpText("Enter the addresses you want to query in the text area under the Input sectionbelow. Please put only 1 address per line. A maximum of 10 addresses i.e. 10 lines can be queried at one time. Click on the Go! button to start the query."),
        helpText("You can then view and/or download the results in the Output section below."),
        helpText("An example has been pre-populated in the app."),
        h3('Input'),
        helpText("Note: Put only 1 address per line. A maximum of 10 addresses i.e. 10 lines can be queried at one time."),
        HTML('<textarea id="addressArea" rows="10" cols="500">New York City</textarea>'),
        br(),
        actionButton("goButton", "Go!"),
        hr()
    ),
    fluidRow(
        h3('Output'),
        h4('Locations identified'),
        tableOutput("locations"),
        helpText("You can save the above output as a CSV file by clicking on the Download button below."),
        downloadButton('downloadData', 'Download'),
        imageOutput("myImage")
    )
))