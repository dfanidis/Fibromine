# Fibromine <img src="./logo/FibromineLogo_v0.3.png" align="right" width=120 height=120 alt="" />

<!-- badges: start -->
  ![GitHub](https://img.shields.io/github/license/dfanidis/Fibromine)
  ![GitHub repo size](https://img.shields.io/github/repo-size/dfanidis/Fibromine)
  ![GitHub issues](https://img.shields.io/github/issues/dfanidis/Fibromine)
  ![Lifecycle:Maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)
<!-- badges: end -->

A Shiny-powered application for fibrosis-related data integration and mining aiming to the acceleration of pulmonary fibrosis research.

## Features

- On the fly integration of differential expression analysis data from a great set of transcriptomic and proteomic datasets
- Exploration of differential gene/protein expression motifs across all supported datasets   
- Real-time creation of fibrosis-specific protein-protein interaction networks
- Rich 3rd party annotation and datasets metadata
- Pathway analysis tool of consensus deregulated genes using the KEGG and GO databases as well as a collection of COVID-19 specific gene sets

## R packages and JS libraries used

```
shiny 
shinyjs
shinyBS 
shinydashboard
shinycssloaders 
DT 
heatmaply 
RSQLite
reshape 
igraph 
visNetwork
htmlwidgets 
openxlsx 
rjson
rintrojs
httr
enrichR
datatables-rowsgroup (Alexey Shildyakov)
```