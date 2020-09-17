setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(DiagrammeR)
library(dplyr)
library(DiagrammeRsvg)
library(rsvg)

grViz("digraph flowchart {
      graph [compound = true, nodesep = .5, ranksep = .25, color = darkgrey]
      
      node [font = times, shape = rectangle]        
      
# subgraph for R information
      subgraph cluster_excluded {
fillcolor = white
      graph[compound = true, shape = rectangle]
      style = dashed
      node [fixedsize = true, width = 3.5, margin = 0.25]
      edge [color = white, arrowhead = none, arrowtail = none, length = 0.5]
      label = 'Excluded \\n (n = 44)'
      A[label = 'No photograph or only one witness \\n (n=18)']
      B[label = 'Returned to collection of origin \\n (n=24)']
      C[label = 'Records from Northern Ireland \\n (n=2)']
      A -> B -> C
      }

# subgraph for R information
      subgraph cluster_included {
      graph[shape = rectangle]
      style = dashed
      node [fixedsize = true, width = 3.5, margin = 0.25]
      edge [color = white, arrowhead = none, arrowtail = none, length = 0.5]
      label = 'Included \\n (n = 95)'
      D[label = 'Media sources \\n (n=64)']
      E[label = 'Local Environmental Records \\n (n=18)']
      F[label = 'National Biodiversity Network \\n (n=7)']
      G[label = 'Yalden (2013) \\n (n=6)']      
      D -> E -> F -> G
      }

      H[label = 'Individual wallaby sightings \\n (n=139)']
      
H -> A [lhead=cluster_excluded, arrowhead = none]
H -> D [lhead=cluster_included, arrowhead = none]

      }
      ") %>%
  export_svg %>% charToRaw %>% rsvg_png("../figures/20200917_flowchart.png", width = 5800, height = 3208)

