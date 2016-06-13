miRcompShinyApp <- function() {
  appDir <- system.file("shinyapp", "app.R", package = "miRcomp")
  if (appDir == "") {
    stop("Could not find shinyapp directory. Try re-installing `miRcomp`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}