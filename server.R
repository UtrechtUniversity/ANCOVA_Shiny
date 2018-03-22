

server <- function(input, output, session) {

  dat <- data.frame(cbind(A = c(0, 1, 0, 1),
                          B = c(1, 1, 0, 0),
                          mu = c(1, 2, 1.2, 1.8),
                          se = c(.5, 2, .3, .6)))
  rownames(dat) <- paste0("Group_", 1:nrow(dat))

  # Dataset <- reactive({
  #   dat
  # })


  output$anova_plot <- renderHighchart({

    # dat <- Dataset()

    highchart() %>%
      hc_chart(animation = FALSE) %>%
      hc_add_theme(hc_theme_economist()) %>%
      hc_legend(labelFormat = "A") %>%
      hc_title(text = "ANOVA") %>%
      hc_tooltip(valueDecimals = 2) %>%
      hc_yAxis(max = max(dat$mu) + diff(range(dat$mu)),
               min = min(dat$mu) - diff(range(dat$mu))) %>%
      hc_subtitle(text = "Points here can be dragged.") %>%
      hc_xAxis(categories = rownames(dat), title = list(text = "Mean")) %>%
      hc_plotOptions(
        series = list(
          point = list(
            events = list(
              drop = JS("function(event){
                      Shiny.onInputChange('drop_result', this.y);
                      }")
            )
          ),
          stickyTracking = FALSE
        ),
        column = list(
          stacking = "normal"
        ),
        line = list(
          cursor = "ns-resize"
        )
      ) -> h1

    if (length(input$new_y) > 0) dat[1,1] <- input$new_y


    for (Auniq in unique(dat$A))
      h1 <- hc_add_series(h1,
        data = dat[dat$A == Auniq, "mu"],
        draggableY = TRUE
      )
    return(h1)
  })


  makeReactiveBinding("outputText")

  outputText <- "Nothing yet."

  observeEvent(input$drop_result, {
    outputText <<- paste(round(input$drop_result,2))
  })

  output$text <- renderText({
    outputText
  })

  output$dattab <- renderTable({dat})

}

