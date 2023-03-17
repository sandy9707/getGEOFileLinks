library(shiny)
library(xml2)
library(DT)
library(shinyjs)

# Define UI
ui <- fluidPage(
    useShinyjs(),
    titlePanel("GEO File Downloader"),
    sidebarLayout(
        sidebarPanel(
            textInput("geoID", "Enter a GEO accession number:"),
            uiOutput("fileType"),
            actionButton("submitBtn", "Submit")
        ),
        mainPanel(
            DTOutput("fileList")
        )
    )
)

# Define server
server <- function(input, output) {
    # Define function to get directory listing
    getDirListing <- function(url) {
        a <- read_html(url)
        fnames <- grep("^G", xml_text(xml_find_all(a, "//a/@href")), value = TRUE)
        return(fnames)
    }

    # Determine file type options based on input type
    output$fileType <- renderUI({
        if (grepl("^GSE", input$geoID)) {
            selectInput("fileOption", "Choose file type:", c("matrix", "suppl"))
        } else if (grepl("^GPL", input$geoID)) {
            selectInput("fileOption", "Choose file type:", "annot")
        }
    })

    # Get list of files and display in table when submit button is clicked
    output$fileList <- renderDT(
        {
            req(input$submitBtn)
            if (input$fileOption == "matrix") {
                gdsurl <- "https://ftp.ncbi.nlm.nih.gov/geo/series/%s/%s/matrix/"
            } else if (input$fileOption == "suppl") {
                gdsurl <- "https://ftp.ncbi.nlm.nih.gov/geo/series/%s/%s/suppl/"
            } else if (input$fileOption == "annot") {
                gdsurl <- "https://ftp.ncbi.nlm.nih.gov/geo/platforms/%s/%s/annot/"
            }
            stub <- gsub("\\d{1,3}$", "nnn", input$geoID, perl = TRUE)
            b <- getDirListing(sprintf(gdsurl, stub, input$geoID))
            ret <- list()
            for (x in 1:length(b)) {
                ret[[x]] <- sprintf(
                    paste0(gdsurl, "%s"),
                    stub, input$geoID, b[x]
                )
            }
            merge <- c(unlist(ret))
            output <- data.frame("File Name" = merge)
            output$file_link <- sprintf("<button class='btn btn-primary' onclick='copyToClipboard(\"%s\")'>Copy Link</button>", output$File.Name)
            output <- cbind(output$file_link, output)
            output <- output[, c(2, 1)]
            output
        },
        escape = FALSE,
        rownames = FALSE,
        options = list(
            dom = "Bfrtip",
            buttons = list("copy")
        )
    )
}

# Run the application
shinyApp(ui = ui, server = server)
