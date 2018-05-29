

server <- function(input, output, session) {

  dat <- data.frame(cbind(Condition = rep(c("Treatment", "Control"), 2),
                          Gender = rep(c("Female", "Male"), each = 2)),
                    cbind(mu = c(1, 2, 1.2, 1.8)))
  rownames(dat) <- paste0("Group_", 1:nrow(dat))

  changed_dat <- dat

  # Dataset <- reactive({
  #   dat
  # })

  compute_aov <- function(data) {
    aovdat <- rbind(
      cbind("Treatment", "Female", rnorm(25, mean = data[1, 3])),
      cbind("Control", "Female",   rnorm(25, mean = data[2, 3])),
      cbind("Treatment", "Male",   rnorm(25, mean = data[3, 3])),
      cbind("Control", "Male",     rnorm(25, mean = data[4, 3]))
    )
    aovdat <- as.data.frame(aovdat)
    aovdat[, 3] <- as.numeric(aovdat[, 3])
    colnames(aovdat) <- c("Condition", "Gender", "x")
    aov(x ~ Condition + Gender, data = aovdat)
  }


  output$anova_plot <- renderHighchart({

    # dat <- Dataset()
    dropFunction <- JS("function(event){
                      Shiny.onInputChange('drop_result', [this.y, this.series.name, this.x]);}")

    highchart() %>%
      hc_chart(animation = FALSE) %>%
      hc_add_theme(hc_theme_sandsignika()) %>%
      hc_title(text = "ANOVA") %>%
      hc_tooltip(valueDecimals = 2) %>%
      hc_yAxis(max = max(dat$mu) + diff(range(dat$mu)),
               min = min(dat$mu) - diff(range(dat$mu))) %>%
      hc_subtitle(text = "2x2 Anova model") %>%
      hc_xAxis(categories = c("Female", "Male"), title = list(text = "Mean")) %>%
      hc_add_series(name = "Treatment", data = dat[dat$Condition == "Treatment", "mu"],
                    draggableY = TRUE) %>%
      hc_add_series(name = "Control", data = dat[dat$Condition == "Control", "mu"],
                    draggableY = TRUE) %>%
      hc_legend(labelFormatter = JS("function(e) {return this.name;}")) %>%
      hc_plotOptions(
        series = list(
          point = list(
            events = list(drop = dropFunction)
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


    return(h1)
  })


  makeReactiveBinding("outputText")
  makeReactiveBinding("changed_dat")
  makeReactiveBinding("anova_tab")

  outputText <- "Nothing yet."

  anova_tab <- compute_aov(dat)


  observeEvent(input$drop_result, {
    newy <- round(as.numeric(input$drop_result[1]), 1)
    cond <- input$drop_result[2]
    gend <- ifelse(as.numeric(input$drop_result[3]), "Male", "Female")
    outputText <<- paste0("Hey! You've just moved the mean of ", tolower(gend), "s from the ", tolower(cond),
                         " condition to ", newy, ".")
    changed_dat[(dat$Condition == cond) & (dat$Gender == gend), "mu"] <<- newy

    anova_tab <<- compute_aov(changed_dat)

  })

  output$text <- renderText({
    outputText
  })

  output$dattab <- renderTable({changed_dat})


  output$anova_results <- renderTable({
    as.data.frame(summary(anova_tab)[[1]])
    }, rownames = TRUE)

  output$anova_text <- renderText({
    capture.output(summary(anova_tab))
    })


}

