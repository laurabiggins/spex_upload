library(shiny)
library(magrittr)

# TODO:
# DONE 1. error message when meta and data files are the same - add this to 
# observeEvent(input$data_filepath$datapath, {
# so it appears before Go is pressed.
# 
# 2. Confirmation when upload has been successful. - include link to spex main
# 
# 3. Condense the alerts into 1 information message, with a continue or cancel.
# Add more info here - not just the bad stuff but general info. 
# 
# 4. Add more detail to info pop-ups.
# 
# 5. Write overall documentation so it could be sent to bioinf people to test out.
# 
# 6. If folder already exists with that name - what do we do? Message saying rename the 
# new folder. If you think the other one should be deleted, please email ....
# I don't think we should allow existing ones to be overwritten.

data_location <- "inst/extdata/"

bab_light_blue <- "#00aeef"
bab_dark_blue <- "#1d305f"

# libraries
# shinyFeedback, shinyjs, shinyalert

# UI ----
ui <- tagList(
  
  fluidPage(
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    #tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "spex.css"),
    #),
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
    actionButton(
      inputId = "test",
      label = "test"
    ),
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
            height = "150px"
          ),
        ),
        column(
          width = 2, 
          img(id="summary_info", src="images/info1.png", class="info_logo")
        )
      ),
      fluidRow(
        column(
          width = 8, 
          checkboxInput(
            inputId = "log_transform",
            label = "Allow log transformation",
            value = TRUE
          ),
        ),
        column(
          width = 2, 
          img(id="log_transform_info", src="images/info1.png", class="info_logo")
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
  
  output$test_plot <- renderPlot(plot(1:10), width = 400, height = 400)
  
  observeEvent(input$test, {
    
    
    
    temp_text <- "<div class='potentialIssues'><p style=\"font-size:30px; \">Potential issues detected: </p><p font-size:16px;\">Duplicate feature names found. These will be removed. \n To avoid this, select cancel, then reformat the data outside this tool and re-upload.<br><br>1 NA value found in feature names, this will be removed. To avoid this, select cancel, then reformat the data outside this tool and re-upload.<br><br>Found 1 sample name in metadata that was not in the dataset and will be removed. <br> Sample name being removed is: EXTRA<br><br>Found 5 sample names in the dataset that were not in the metadata and will be removed. <br> Columns being kept are: NAIVE_A1, NAIVE_A2, NAIVE_E1, NAIVE_E2, PRIMED_A1, PRIMED_A2, PRIMED_E1, PRIMED_E2<br>Columns being removed are: Protein names, Gene names, Pep Count, log2_fc naive primed, Biological process<br><br> 
    </div>
    <br>
    <div class='summaryText'>
    <p style=\"font-size:30px;\">Summary dataset information: </p><p style=\"font-size:16px;\">Dataset has 14 rows (observations/features) and 8 columns (samples). <br> Metadata information supplied includes 3 columns: sample_name, condition, cell line.</p>"
    
    temp_table <- tableHTML::tableHTML(
      head(cars),
      rownames = FALSE,
      border = 2,
      class = "modal_conf1"
    )
    
    temp_output <- paste0(temp_text, "<br>", temp_table, "</div>")
    
    shinyalert::shinyalert(
     # html = TRUE,
      # text = tagList(
      #   temp_text#,
      #   plotOutput("test_plot", inline = TRUE),
      # ),
      #text = c(temp_text, temp_table),
      text = temp_output,
      html = TRUE,
      size = "l",
      className = "shinyalertmodal",
      imageWidth = 20
    )
  })
  
  
    observeEvent(input$go, {
    
    # validation 1 - check input fields look ok ---
    ## dataset name ----

    rv$issues_log <- ""
    
    if(nchar(input$dataset_name) > 1) {
     
      shinyFeedback::hideFeedback("dataset_name")
      
      new_folder_path <- paste0("inst/extdata/", input$dataset_name)
      if(dir.exists(new_folder_path)){
        rv$ds_name <- NULL
        shinyFeedback::feedbackDanger(
          inputId = "dataset_name",
          show = TRUE,
          text = "A dataset of this name already exists and will not be overwritten. 
          To continue with the upload of your dataset, enter a different name."
        )
      } else rv$ds_name <- input$dataset_name
      
    } else {
      rv$ds_name <- NULL
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
      shinyFeedback::feedback(
        inputId = "data_type",
        show = !isTruthy(input$data_type),
        text = "No data type selected, this will be left blank.",
        color = "yellow"
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
    
    ## metadata  ----
    if(!isTruthy(input$metadata_filepath)){
      shinyFeedback::feedbackWarning(
        inputId = "metadata_filepath",
        show = !isTruthy(input$metadata_filepath),
        text = "Please select a metadata file."
      )
    } 
    
    ## dataset ----
    if(!isTruthy(input$data_filepath)){
      shinyFeedback::feedbackWarning(
        inputId = "data_filepath",
        show = !isTruthy(input$data_filepath),
        text = "Please select a data file."
      )
    } 
    
    # stop if required fields aren't populated
    req(rv$ds_name)
    #req(nchar(input$dataset_name) > 1)
    req(isTruthy(input$metadata_filepath))
    req(isTruthy(input$data_filepath))
    
    req(input$metadata_filepath$name != input$data_filepath$name)
    
    ## check files exist ----
    if(!file.exists(input$metadata_filepath$datapath)){
      shinyFeedback::hideFeedback("metadata_filepath")
      shinyFeedback::feedbackWarning(
        inputId = "metadata_filepath",
        show = !file.exists(input$metadata_filepath$datapath),
        text = paste0("Couldn't locate file: ", input$metadata_filepath$datapath)
      )
    }
    
    if(!file.exists(input$data_filepath$datapath)){
      shinyFeedback::hideFeedback("data_filepath")
      shinyFeedback::feedbackWarning(
        inputId = "data_filepath",
        show = !file.exists(input$data_filepath$datapath),
        text = paste0("Couldn't locate file: ", input$data_filepath$datapath)
      )
    }  
    
    req(file.exists(input$metadata_filepath$datapath))
    req(file.exists(input$data_filepath$datapath))
    req(tools::file_ext(input$data_filepath$datapath) %in% c("tsv", "txt", "csv"))
    req(tools::file_ext(input$metadata_filepath$datapath) %in% c("tsv", "txt", "csv"))
    
    ## metadata import ----
    meta_file <- switch(tools::file_ext(input$metadata_filepath$datapath), 
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
 
    ## dataset import ----
    dataset <- switch(tools::file_ext(input$data_filepath$datapath), 
                           tsv = ,
                           txt = readr::read_tsv(input$data_filepath$datapath),
                           csv = readr::read_csv(input$data_filepath$datapath),
                           stop("Unknown file extension on data file")
    )
    shinyFeedback::hideFeedback("data_filepath")
    shinyFeedback::feedbackSuccess(
      inputId = "data_filepath",
      show = TRUE,
      text = "File successfully uploaded"
    )

    if(!isTruthy(dataset)){
      shinyalert::shinyalert("Couldn't import dataset for some reason.", type = "error")
    }
    if(!isTruthy(meta_file)){
      shinyalert::shinyalert("Couldn't import metadata file for some reason.", type = "error")
    }
    
    req(dataset)
    req(meta_file)
    
    feature_column <- colnames(dataset)[1]
    # change this to info message
    print(paste0("Using the 1st column (named ", feature_column, ") as the feature names column."))
    
    ## initial dataset processing ----
    # remove duplicates in feature names
    if(anyDuplicated(dataset[[feature_column]]) > 0){
      msg <- "Duplicate feature names found. These will be removed. 
        To avoid this, select cancel, then reformat the data outside this tool and re-upload.<br><br>"
      rv$issues_log <- paste0(rv$issues_log, msg)
      dataset <- dataset %>% 
        dplyr::distinct(.data[[feature_column]], .keep_all = TRUE)
    }
    
    # check for NAs in feature names
    if(any(is.na(dataset[[feature_column]]))){
      n_na <- sum(is.na(dataset[[feature_column]]))
      msg <- dplyr::if_else(
        n_na == 1,
        "1 NA value found in feature names, this will be removed. ",
        paste0(
          n_na, 
          " NA values found in feature names, these will be removed. "
        )
      )
      msg <- paste0(msg, "To avoid this, select cancel, then reformat the data outside this tool and re-upload.<br><br>")
      rv$issues_log <- paste0(rv$issues_log, msg)
      dataset <- tidyr::drop_na(dataset, .data[[feature_column]])
    }
    
    # convert feature names to row names
    dataset <- dataset %>%
      tibble::column_to_rownames(feature_column)

    
    # check sample names match with meta and data files ----
    n_matched_samples <- sum(colnames(dataset) %in% meta_file[[1]])
    
    if(n_matched_samples < 2){
      shinyalert::shinyalert(
        paste0(
          "Found ", 
          n_matched_samples,  
          " sample name(s) that matched between metadata and dataset. 
          A minimum of 2 are required. Check the sample names in the files and try again."
        ),
        type = "error",
        closeOnClickOutside = TRUE,
        className = "shinyalertmodal"
      )
    }
    req(n_matched_samples >= 2)
    
    # # remove any columns that aren't in the data file
    if(any(! meta_file[[1]] %in% colnames(dataset))){
      n_not_in <- sum(!meta_file[[1]] %in% colnames(dataset))
      inner_msg <- dplyr::if_else(
        n_not_in == 1,
        " sample name in metadata that was not in the dataset and will be removed. <br> Sample name being removed is: ",
        " sample names in metadata that were not in the dataset and will be removed. <br> Sample names being removed are: ",
      )
      msg <- paste0(
        "Found ", 
        n_not_in, 
        inner_msg,
        paste0(meta_file[[1]][!meta_file[[1]] %in% colnames(dataset)], collapse = ", "),
        "<br><br>"
      )
      rv$issues_log <- paste0(rv$issues_log, msg)
      meta_file <- meta_file[meta_file[[1]] %in% colnames(dataset), ]
    }
    
    #  remove any sample name columns that aren't in the metadata file
    if(any(! colnames(dataset) %in% meta_file[[1]])) {
      n_not_in <- sum(!colnames(dataset) %in% meta_file[[1]])
      inner_msg <- dplyr::if_else(
        n_not_in == 1,
        " sample name in the dataset that was not in the metadata and will be removed. <br> Columns being kept are: ", 
        " sample names in the dataset that were not in the metadata and will be removed. <br> Columns being kept are: ", 
      )
      msg <- paste0(
        "Found ", 
        n_not_in, 
        inner_msg, 
        paste0(colnames(dataset)[colnames(dataset) %in% meta_file[[1]]], collapse = ", "),
        "<br>Columns being removed are: ", 
        paste0(colnames(dataset)[!colnames(dataset) %in% meta_file[[1]]], collapse = ", "),
        "<br><br>"
      )
      rv$issues_log <- paste0(rv$issues_log, msg)
      dataset <- dplyr::select(dataset, all_of(meta_file[[1]]))
    }
    
    # assemble info msg ----
    
    if(sum(nchar(rv$issues_log) == 0)){
      rv$issues_log <- '<div class="potentialIssues"><p style="font-size:30px;">No potential issues detected</p></div>'
    } else {
      rv$issues_log <- paste0(
        '<div class="potentialIssues"><p style="font-size:30px;">Potential issues detected: </p><p font-size:16px;">',
        rv$issues_log,
        '</div>'
      )
    }
    
    general_info <- paste0(
      '<p style="font-size:30px;"> Summary dataset information: </p><p style="font-size:16px;"> Dataset has ',
      nrow(dataset),
      " rows (observations/features) and ",
      ncol(dataset), 
      " columns (samples). <br> Metadata information supplied includes ",
      ncol(meta_file),
      " columns: ",
      paste0(colnames(meta_file), collapse = ", "),
      ".</p>"
    )

    table_dataset <- tableHTML::tableHTML(
      head(dataset, n = 5),
      rownames = TRUE,
      border = 2,
      class = "modal_conf1"
    )
    table_metadata <- tableHTML::tableHTML(
      head(meta_file, n = 5),
      rownames = FALSE,
      border = 2,
      class = "modal_conf1"
    )
    
    modal_output <- paste0(
      rv$issues_log, 
      '<br><div class="summaryText">', 
      general_info, 
      '<br>',
      table_dataset, 
      "<br>", 
      table_metadata,
      '</div>'
    )
    
    # option to continue if there is any info in the summary log - there should always be info in it.
    
    ## modal confirmation 1 ----
      shinyalert::shinyalert(
        modal_output,
        inputId = "info_log_confirmation",
        showCancelButton = TRUE,
        closeOnClickOutside = FALSE,
        className = "shinyalertmodal",
        html = TRUE,
        size = "l",
       
      )
    rv$data_file <- dataset
    rv$metadata_file <- meta_file
  })
  
  # This occurs when the user is happy that the information in the modal looks ok.
  # This is just checking that the new file path is ok - should probably happen before.
  observeEvent(input$info_log_confirmation, {
    
    if(input$info_log_confirmation){
      
      print("confirmed, please go ahead")
      # save metadata and dataset as .rds files -----
      req(processed_dataset())
      req(processed_metadata())
      
      new_folder_path <- paste0("inst/extdata/", rv$ds_name)
      
      dir.create(new_folder_path)
      
      outfile_meta <- paste0(new_folder_path, "/metadata.rds")
      saveRDS(processed_metadata(), outfile_meta)
      outfile_data <- paste0(new_folder_path, "/dataset.rds")  
      saveRDS(processed_dataset(), outfile_data)
      
      # write out json file ----
      x <- jsonlite::fromJSON(txt = "inst/extdata/updated_arrays.txt")
      new_ds <- c(rv$ds_name, rv$ds_citation, rv$ds_summary_info, rv$ds_data_type)
      x$data <- rbind(x$data, new_ds)
      jsonlite::write_json(x, path = "inst/extdata/updated_arrays.txt")
      
      shinyalert::shinyalert(
        "Dataset successfully uploaded, you should now be able to view this in spex",
        type = "success"
      )
      
    } else {
      print("cancelled")
    }
    
  })
  
  
  sample_vector <- reactive({
    # check if the data should be log transformed
    data_vector <- unlist(rv$data_file)
    
    sample_size <- dplyr::if_else(
      length(data_vector) <= 5000, 
      length(data_vector), 
      as.integer(5000)
    )
    sample(data_vector, size = sample_size)
  })
    
  output$qqnorm_plots <- renderPlot(qqnorm(sample_vector()), width = 400, height = 400)
  
  ## dataset processing ----
  processed_dataset <- reactive({
 
    dataset <- rv$data_file
    # 
    # # check if the data should be log transformed
    # data_vector <- unlist(dataset)
    # 
    # sample_size <- dplyr::if_else(length(data_vector) <= 5000, length(data_vector), as.integer(5000))
    # 
    # sample_vector <- sample(data_vector, size = sample_size)
    shapiro_result <- shapiro.test(sample_vector())
    
    if(shapiro_result$p.value < 0.01) {
      #print("Data looks like it should be log transformed.")
    
     # qqnorm(sample_vector)
      #qqnorm(log2(sample_vector))
      
      #browser()
      # this doesn't work properly - maybe it needs to not be in the reactive??
      shinyalert::shinyalert(
        html = TRUE,
        text = tagList(
          plotOutput("test_plot")
          #"Data was log2 transformed.",
          #plotOutput("qqnorm_plots")#, inline = TRUE)
        )
      )
      
      #output$qqnorm_plots <- renderPlot(qqnorm(sample_vector))
      
      dataset <- log2(dataset)
 
      if(any(dataset == "-Inf")) {
        print("-Inf values created, we'll change these to 0")
        dataset[dataset == "-Inf"] <-  0
      }
      print("Data has been log2 transformed.")
      #print(head(dataset))
    }
    dataset
  })
  
  
  ## metadata processing ----
  processed_metadata <- reactive({
    metadata <- process_metadata(
      meta_dataset = rv$metadata_file
    ) 
  })

 # observeEvent(input$metadata_filepath$datapath, {
 # need observe here as we need to update if data_filepath changes
  observe({
    if(isTruthy(input$metadata_filepath)){
      shinyFeedback::hideFeedback("metadata_filepath")
      
      # check meta and data files have different names ---
      if(isTruthy(input$data_filepath)) { 
        if(input$metadata_filepath$name == input$data_filepath$name){
          shinyFeedback::hideFeedback("metadata_filepath")
          shinyFeedback::feedbackDanger(
            inputId = "metadata_filepath",
            show = TRUE,
            text = "Data file and metadata file have the same name, 
              please choose a different file."
          )
          shinyFeedback::hideFeedback("data_filepath")
          shinyFeedback::feedbackDanger(
            inputId = "data_filepath",
            show = TRUE,
            text = "Data file and metadata file have the same name, 
              please choose a different file."
          )
        }
        req(input$metadata_filepath$name != input$data_filepath$name)
      }
      
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
  
 # observeEvent(input$data_filepath$datapath, {
 # # need observe here as we need to update if metadata_filepath changes
 observe({
    if(isTruthy(input$data_filepath)){
      #browser()
      shinyFeedback::hideFeedback("data_filepath")
      
      # check meta and data files have different names ---
      if(isTruthy(input$metadata_filepath)){
        if (input$metadata_filepath$name == input$data_filepath$name){
          shinyFeedback::hideFeedback("metadata_filepath")
          shinyFeedback::feedbackDanger(
            inputId = "metadata_filepath",
            show = TRUE,
            text = "Data file and metadata file have the same name, 
            please choose a different file."
          )
          shinyFeedback::hideFeedback("data_filepath")
          shinyFeedback::feedbackDanger(
            inputId = "data_filepath",
            show = TRUE,
            text = "Data file and metadata file have the same name, 
            please choose a different file."
          )
        } 
        req(input$metadata_filepath$name != input$data_filepath$name)
      }  
         
      if(tools::file_ext(input$data_filepath$datapath) %in% c("tsv", "txt", "csv")){
        shinyFeedback::feedbackSuccess(
          inputId = "data_filepath",
          show = TRUE,
          text = "Happy with the file type"
        )
      } else {
        shinyFeedback::feedbackWarning(
          inputId = "data_filepath",
          show = !tools::file_ext(input$data_filepath$datapath) %in% c("tsv", "txt", "csv"),
          text = paste0(
            "Data file type must be one of tsv, txt or csv, file type specified was ",
            tools::file_ext(input$data_filepath$datapath)
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