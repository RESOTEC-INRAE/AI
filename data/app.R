#############################
#     USER CONFIGURATION    #
#############################

CSV_NAME <- "annotations.csv"   # Name of the CSV file where annotations are saved
ID_COLUMN <- "image"          # Name of the ID column (unique identifier for each image)

ANNOTATION_VARS <- c(
  "human",
  "anthropic",
  "vegetation",
  "rock",
  "snow"
  # Add more variables here if needed
)

IMAGE_EXTENSIONS <- c("jpg", "jpeg")  # Allowed image formats

#############################
#        APPLICATION        #
#############################

library(shiny)
library(shinyFiles)

# ---------------------------
# UI (User Interface)
# ---------------------------
ui <- fluidPage(
  titlePanel("Interactive Image Annotation"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Folder selection (manual input + browse button)
      fluidRow(
        column(
          8,
          textInput("img_dir", "Image folder:", value = "")
        ),
        column(
          4,
          div(
            style = "margin-top: 26px;",
            shinyDirButton("browse", "Browse", "Select a folder")
          )
        )
      ),
      
      # Button to load or resume annotation session
      actionButton("load_images", "Load / Resume"),
      hr(),
      
      # Dynamic annotation checkboxes
      uiOutput("annotation_ui"),
      
      # Navigation buttons
      fluidRow(
        column(4, actionButton("prev_img", "◀ Previous")),
        column(4, actionButton("next_img", "Next ▶")),
        column(4, actionButton("stop", "STOP"))
      )
    ),
    
    # Main panel: image display
    mainPanel(
      textOutput("img_name"),
      imageOutput("image", height = "600px")
    )
  )
)

# ---------------------------
# Server Logic
# ---------------------------
server <- function(input, output, session) {
  
  # Define accessible directories
  volumes <- c(Home = path.expand("~"), Root = "/")
  
  # Enable folder browsing
  shinyDirChoose(input, "browse", roots = volumes, session = session)
  
  # Reactive values (store app state)
  rv <- reactiveValues(
    images = character(),  # list of image paths
    index = 1,             # current image index
    results = NULL,        # annotation dataframe
    csv_path = NULL        # path to CSV file
  )
  
  # ---------------------------
  # Dynamic annotation UI
  # ---------------------------
  output$annotation_ui <- renderUI({
    tagList(
      lapply(ANNOTATION_VARS, function(var) {
        checkboxInput(var, var, value = FALSE)
      })
    )
  })
  
  # ---------------------------
  # Update folder path when browsing
  # ---------------------------
  observeEvent(input$browse, {
    dir <- parseDirPath(volumes, input$browse)
    if (length(dir) > 0) {
      updateTextInput(session, "img_dir", value = dir)
    }
  })
  
  # ---------------------------
  # Load or resume session
  # ---------------------------
  observeEvent(input$load_images, {
    req(dir.exists(input$img_dir))
    
    # Build regex pattern for image files
    pattern <- paste0("\\.(", paste(IMAGE_EXTENSIONS, collapse = "|"), ")$")
    
    # Get list of images
    rv$images <- list.files(
      input$img_dir,
      pattern = pattern,
      full.names = TRUE
    )
    
    # Define CSV path
    rv$csv_path <- file.path(input$img_dir, CSV_NAME)
    
    if (file.exists(rv$csv_path)) {
      # Resume existing annotations
      rv$results <- read.csv(rv$csv_path, stringsAsFactors = FALSE)
      
      # Resume from last annotated image
      last_id <- rv$results[[ID_COLUMN]][nrow(rv$results)]
      rv$index <- match(
        last_id,
        tools::file_path_sans_ext(basename(rv$images))
      )
    } else {
      # Create new annotation dataframe
      rv$results <- data.frame(
        setNames(list(character()), ID_COLUMN),
        stringsAsFactors = FALSE
      )
      
      # Add annotation columns
      for (v in ANNOTATION_VARS) {
        rv$results[[v]] <- integer()
      }
      
      rv$index <- 1
    }
  })
  
  # ---------------------------
  # Display image
  # ---------------------------
  output$image <- renderImage({
    req(length(rv$images) > 0)
    req(rv$index >= 1 && rv$index <= length(rv$images))
    
    list(
      src = rv$images[rv$index],
      contentType = "image/jpeg",
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  # ---------------------------
  # Display image name and position
  # ---------------------------
  output$img_name <- renderText({
    req(length(rv$images) > 0)
    
    paste0(
      "Image ", rv$index, " / ", length(rv$images),
      " : ", basename(rv$images[rv$index])
    )
  })
  
  # ---------------------------
  # Load existing annotation values
  # ---------------------------
  observe({
    req(length(rv$images) > 0)
    req(rv$index >= 1 && rv$index <= length(rv$images))
    
    id <- tools::file_path_sans_ext(basename(rv$images[rv$index]))
    row <- rv$results[rv$results[[ID_COLUMN]] == id, , drop = FALSE]
    
    # Update checkboxes
    for (v in ANNOTATION_VARS) {
      if (nrow(row) == 1) {
        updateCheckboxInput(session, v, value = as.logical(row[[v]]))
      } else {
        updateCheckboxInput(session, v, value = FALSE)
      }
    }
  })
  
  # ---------------------------
  # Save annotation ONLY when clicking "Next"
  # ---------------------------
  observeEvent(input$next_img, {
    req(length(rv$images) > 0)
    
    id <- tools::file_path_sans_ext(basename(rv$images[rv$index]))
    
    # Remove existing row for this image
    rv$results <- rv$results[rv$results[[ID_COLUMN]] != id, ]
    
    # Create new annotation row
    new_row <- data.frame(
      setNames(list(id), ID_COLUMN),
      stringsAsFactors = FALSE
    )
    
    for (v in ANNOTATION_VARS) {
      new_row[[v]] <- as.integer(input[[v]])
    }
    
    # Append new row
    rv$results <- rbind(rv$results, new_row)
    
    # Save to CSV
    write.csv(rv$results, rv$csv_path, row.names = FALSE)
    
    # Move to next image
    if (rv$index < length(rv$images)) {
      rv$index <- rv$index + 1
    }
  })
  
  # ---------------------------
  # Go to previous image
  # ---------------------------
  observeEvent(input$prev_img, {
    if (rv$index > 1) rv$index <- rv$index - 1
  })
  
  # ---------------------------
  # Stop session
  # ---------------------------
  observeEvent(input$stop, {
    showModal(modalDialog(
      title = "Session stopped",
      "Annotations saved. You can resume later.",
      easyClose = TRUE
    ))
  })
}

# Launch the app
shinyApp(ui, server)