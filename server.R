
data("citytemp")

server <- function(input, output, session) {

  output$anova_plot <- renderChart2({

    highchart() %>%
      hc_chart(animation = FALSE) %>%
      hc_title(text = "draggable points demo") %>%
      hc_subtitle(text = "Drang my points plz") %>%
      hc_xAxis(categories = month.abb) %>%
      hc_plotOptions(
        series = list(
          point = list(
            events = list(
              drop = JS("function(){
                    alert(this.series.name + ' ' + this.category + ' ' + Highcharts.numberFormat(this.y, 2))
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
      ) %>%
      hc_add_series(
        data = citytemp$berlin,
        draggableY = TRUE
      )
  })

}

