
<!-- README.md is generated from README.Rmd. Please edit that file -->

# matxingraphout

<!-- badges: start -->

<!-- badges: end -->

**Convert Adjacency Matrices to SVG, DOT, and Mermaid Graphs**


Takes an adjacency matrix and a node-property table and returns a rendered SVG file, Graphviz DOT source, and Mermaid flowchart source.

No external package dependencies except for

    shiny (>= 1.7.0),
    DT (>= 0.28).

The graphics helpers and Shiny app were developped with Sonnet 4.6 on Claude Code v.2.1.84. 

## Installation

You can install the development version of matxingraphout like so:

``` r
remotes::install_github("Shimpeim/matxingraphout", force = TRUE)
require(matxingraphout)

# And then, open Shiny app
run_app()
```

## Example

Click! Click! and Click!

The "Render Graph" button below "Settings" panel generate a graph on the right side of your browser.

You can add "centroid" for curvature of edges by mouse click (before it please click "Place/Remove centroid"). 
