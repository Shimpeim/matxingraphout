#' Launch the matxingraphout interactive web interface
#'
#' Opens a Shiny web-browser GUI for building and visualising graphs without
#' writing R code.  Provides an editable node table, adjacency-matrix grid,
#' overlay-edge support, all layout and styling parameters, and live SVG / DOT
#' / Mermaid / topology output with one-click downloads.
#'
#' @param ... Arguments passed to [shiny::runApp()] (e.g. `port`, `launch.browser`).
#'
#' @return Invisibly, the return value of [shiny::runApp()].
#' @export
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
run_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE))
    stop("The 'shiny' package is required. Install it with: install.packages('shiny')",
         call. = FALSE)
  if (!requireNamespace("DT", quietly = TRUE))
    stop("The 'DT' package is required. Install it with: install.packages('DT')",
         call. = FALSE)

  app_dir <- system.file("shinyapp", package = "matxingraphout")
  if (!nzchar(app_dir))
    stop("Could not locate the shinyapp directory. Try reinstalling the package.",
         call. = FALSE)

  shiny::runApp(app_dir, ...)
}
