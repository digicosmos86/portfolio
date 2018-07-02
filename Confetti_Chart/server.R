
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)
library(dplyr)
library(data.table)

data_all <- fread('data/data_all.csv', data.table=FALSE)
data_all <- within(data_all, {
  require(lubridate)
  day <- ymd(day)
  page <- as.factor(page)
  event <- as.factor(event)
})

data_all <- by(data_all, as.factor(data_all$teacher), function(x){
  x
})

color_scale <- c(
  '#e31a97',
  '#eefb3d',
  '#74ed33',
  '#02ec71',
  '#5522df',
  '#f74507',
  '#47b1e1'
)

pages <-  c(
  'Article',
  'Create-Project',
  'Dashboard',
  'Edit-Project',
  'Explore',
  'Project-Transition',
  'Read-Project'
)

shape_scale <- c(
  'circle',
  'square',
  'triangle-up',
  'triangle-down',
  'diamond',
  'cross'
)

events <- c(
  'Adjust',
  'Edit',
  'Interact',
  'Read',
  'Social',
  'Support'
)

shinyServer(function(input, output) {

  x <- reactive({
    tsubset <- data_all[[as.character(input$teacher)]] %>%
      filter(day>=as.POSIXct(input$dtrange[1]), day<=as.POSIXct(input$dtrange[2])) %>%
      mutate(label=as.factor(label)) %>%
      filter(ell %in% as.integer(input$ell)) %>%
      filter(iep %in% as.integer(input$iep)) %>%
      filter(elapsed>=input$elapsed[1], elapsed<=input$elapsed[2]) %>%
      filter(event %in% events[as.integer(input$events)]) %>%
      filter(page %in% pages[as.integer(input$pages)])
  })
  ggvis(x, ~label, ~elapsed) %>% layer_points(fill=~page, shape=~event, size:=10) %>% 
    add_legend(scales='fill') %>% 
    add_legend(scales = "shape", properties = legend_props(legend = list(y = 150))) %>%
    scale_numeric("y", reverse=TRUE, label="Elapsed Time from The First Event of the Day") %>%
    scale_nominal("fill", domain = pages, range=color_scale) %>%
    scale_nominal("shape", domain = events, range=shape_scale) %>%
    hide_axis('x') %>%
    set_options(width=1200, height=600, duration=0, renderer='canvas') %>%
    bind_shiny('confetti')
})
