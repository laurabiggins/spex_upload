library(shiny)

data_location <- "inst/extdata/"

bab_light_blue <- "#00aeef"
bab_dark_blue <- "#1d305f"

# UI ----
ui <- tagList(
  
  fluidPage(
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "spex.css")
    ),
    theme = bslib::bs_theme(
      bg = bab_dark_blue,
      fg = "white",
      primary = bab_light_blue,
      secondary = bab_light_blue
    ),
    
    img(id="BI_logo1", src="images/BI_logo_grey.png", alt="BI logo"),
    
    h1(id="main_title", "Upload new dataset"),
    p("Upload a dataset from your computer to spex. Details about each field and the required file formats can be found by clicking on the accompanying info icons"),
    br(),
    
    wellPanel(
      fluidRow(
        column(
          width = 8, 
          textInput(
            inputId = "dataset_name",
            label = c("Enter dataset name")
          ),
        ),
        column(
          width = 2, 
          img(id="dataset_name_info", src="images/info1.png", class="info_logo")
        )
      ),
      br(),
      fluidRow(
        column(
          width = 8,
          fileInput(
            inputId = "data_filepath",
            label = "Choose data file"
          )
        ),
        column(
          width = 2, 
          img(id="data_filepath_info", src="images/info1.png", class="info_logo")
        )
      ),
      fluidRow(
        column(
          width = 8,
          fileInput(
            inputId = "metadata_filepath",
            label = "Choose meta data file"
          )
        ),
        column(
          width = 2, 
          img(id="metadata_filepath_info", src="images/info1.png", class="info_logo")
        )
      ),
      fluidRow(
        column(
          width = 8, 
          selectInput(
            inputId = "data_type",
            label = c("Select data type"), 
            choices = c("RNA-seq", "ChIP-seq", "ATAC-seq", "Bisulfite-seq", "RRBS", "Other" ), 
            multiple = TRUE,
          ),
        ),
        column(
          width = 2, 
          img(id="datatype_info", src="images/info1.png", class="info_logo")
        )
      ),
      fluidRow(
        column(
          width = 8, 
          textInput(
            inputId = "citation",
            label = "Enter citation"
          ),
        ),
        column(
          width = 2, 
          img(id="citation_info", src="images/info1.png", class="info_logo")
        )
      ),
      fluidRow(
        column(
          width = 8, 
          textAreaInput(
            inputId = "summary",
            label = "Enter summary information",
            height = "200px"
          ),
        ),
        column(
          width = 2, 
          img(id="summary_info", src="images/info1.png", class="info_logo")
        )
      ),
      actionButton(
        inputId = "go",
        label = "Go"
      ),
      actionButton("browser", "browser")
    ),
    includeHTML("www/modals.html"),

    tags$script(src = "spex_upload.js")
  )
)

server <- function(input, output, session) {

  observeEvent(input$browser, browser())
  
  rv <- reactiveValues()
  
  observeEvent(input$go, {
    
    # validation ---
    ## dataset name ----
    
    if(nchar(input$dataset_name) > 1) {
      rv$ds_name <- input$dataset_name
      shinyFeedback::hideFeedback("dataset_name")
    } else {
      shinyFeedback::feedbackWarning(
        inputId = "dataset_name",
        show = nchar(input$dataset_name) <= 1,
        text = "Please enter a name for the dataset."
      )
    }

    ## data type ----
    if(isTruthy(input$data_type)){
      rv$ds_data_type <- input$data_type
      shinyFeedback::hideFeedback("data_type")
    } else {
      shinyFeedback::feedbackWarning(
        inputId = "data_type",
        show = !isTruthy(input$data_type),
        text = "Please select a data type."
      )
    }
    
    ## citation ----
    if(isTruthy(input$citation)){
      rv$ds_citation  <- input$citation
      shinyFeedback::hideFeedback("citation")
    } else {
      shinyFeedback::feedback(
        inputId = "citation",
        show = !isTruthy(input$citation),
        text = "No citation supplied, this will be left blank.",
        color = "yellow"
      )
      rv$ds_citation <- ""
    }
    
    ## summary ----
    if(isTruthy(input$summary)){
      rv$ds_summary  <- input$summary
      shinyFeedback::hideFeedback("summary")
    } else {
      shinyFeedback::feedback(
        inputId = "summary",
        show = !isTruthy(input$summary),
        text = "No summary information supplied, this will be left blank.",
        color = "yellow"
      )
      rv$ds_summary <- ""
    }
    
    ## metadata processing ----
    if(!isTruthy(input$metadata_filepath)){
      shinyFeedback::feedbackWarning(
        inputId = "metadata_filepath",
        show = !isTruthy(input$metadata_filepath),
        text = "Please select a metadata file."
      )
    } else {
      if(file.exists(input$metadata_filepath$datapath) & 
        tools::file_ext(input$metadata_filepath$datapath) %in% c("tsv", "txt", "csv")) {
        rv$meta_file <- switch(tools::file_ext(input$metadata_filepath$datapath), 
                            tsv = ,
                            txt = readr::read_tsv(input$metadata_filepath$datapath),
                            csv = readr::read_csv(input$metadata_filepath$datapath),
                            stop("Unknown file extension on data file")
        )
        shinyFeedback::hideFeedback("metadata_filepath")
        shinyFeedback::feedbackSuccess(
          inputId = "metadata_filepath",
          show = TRUE,
          text = "File successfully uploaded"
        )
      } else {
          shinyFeedback::hideFeedback("metadata_filepath")
          
          if(!file.exists(input$metadata_filepath$datapath)){
            shinyFeedback::feedbackWarning(
              inputId = "metadata_filepath",
              show = !file.exists(input$metadata_filepath$datapath),
              text = paste0("Couldn't locate file: ", input$metadata_filepath$datapath)
            )
          } else {
            shinyFeedback::feedbackWarning(
              inputId = "metadata_filepath",
              show = !tools::file_ext(input$metadata_filepath$datapath) %in% c("tsv", "txt", "csv"),
              text = paste0("Metadata file type must be one of tsv, txt or csv, file type specified was ", tools::file_ext(input$metadata_filepath$datapath))
            )
          }
        }
    }        

    
    # other_columns <- colnames(meta_file)[-1]
    # 
    # new_folder_path <- paste0("inst/extdata/", new_folder_name)
    # dir.create(new_folder_path)
    # outfile_meta <- paste0(new_folder_path, "/metadata.rds")
    
    
    
    
    x <- jsonlite::fromJSON(txt = "inst/extdata/updated_arrays.txt")
    
    new_ds <- c(rv$ds_name, rv$ds_citation, rv$ds_summary_info, rv$ds_data_type)
    x$data <- rbind(x$data, new_ds)
    jsonlite::write_json(x, path = "inst/extdata/updated_arrays.txt")
  
  })
  
  
  
  
  observeEvent(input$metadata_filepath$datapath, {
    if(isTruthy(input$metadata_filepath)){
      shinyFeedback::hideFeedback("metadata_filepath")
      
      if(tools::file_ext(input$metadata_filepath$datapath) %in% c("tsv", "txt", "csv")){
        shinyFeedback::feedbackSuccess(
          inputId = "metadata_filepath",
          show = TRUE,
          text = "Happy with the file type"
        )
      } else {
        shinyFeedback::feedbackWarning(
          inputId = "metadata_filepath",
          show = !tools::file_ext(input$metadata_filepath$datapath) %in% c("tsv", "txt", "csv"),
          text = paste0(
            "Metadata file type must be one of tsv, txt or csv, file type specified was ",
            tools::file_ext(input$metadata_filepath$datapath)
          )
        )
      }
    }
  })
  
  
  
  
  
  
# js checks for each field
#
# import data file - we'll need a load of validity checks,
# 
# import metadata
# 
# write these out as RData files
# 
# append to a javascript array file 
# https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html
  
}

shinyApp(ui, server)