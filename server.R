# function derived from the highlightHTMLcells() function of the highlightHTML package
colortable <- function(htmltab, css, style="table-condensed table-bordered"){
  tmp <- str_split(htmltab, "\n")[[1]]
  CSSid <- gsub("\\{.+", "", css)
  CSSid <- gsub("^[\\s+]|\\s+$", "", CSSid)
  CSSidPaste <- gsub("#", "", CSSid)
  CSSid2 <- paste(" ", CSSid, sep = "")
  ids <- paste0("<td id='", CSSidPaste, "'")
  for (i in 1:length(CSSid)) {
    locations <- grep(CSSid[i], tmp)
    tmp[locations] <- gsub("<td", ids[i], tmp[locations])
    tmp[locations] <- gsub(CSSid2[i], "", tmp[locations],
                           fixed = TRUE)
  }
  htmltab <- paste(tmp, collapse="\n")
  Encoding(htmltab) <- "UTF-8"
  list(
    tags$style(type="text/css", paste(css, collapse="\n")),
    tags$script(sprintf(
      '$( "table" ).addClass( "table %s" );', style
    )),
    HTML(htmltab)
  )
}

server <- function(input, output, session) {


  aov_dat <- data.frame(cbind(Condition = rep(c("Treatment", "Control"), 2),
                          Gender = rep(c("Female", "Male"), each = 2)),
                    cbind(Mean = c(3, 2, 2, 3)))
  rownames(aov_dat) <- paste0("Group_", 1:nrow(aov_dat))

  changed_aov_dat <- aov_dat

  svar <- function(x) mean((x - mean(x))^2)


  compute_aov <- function(mus, n = 60) {

    MSb <-  svar(mus) * n
    MSg <-  svar(c(sum(mus[1:2]),     sum(mus[3:4]))     / 2) * n
    MSc <-  svar(c(sum(mus[c(1, 3)]), sum(mus[c(2, 4)])) / 2) * n
    MSint <-  MSb - MSg - MSc

    p_vec <- pf(q = c(MSc, MSg, MSint), df1 = 1, df2 = n - 4, lower.tail = FALSE)

    out <- cbind(c("Condition", "Gender", "Condition:Gender"),
                 sprintf("%.1f", c(MSc, MSg, MSint)),
                 1,
                 n - 4,
                 sprintf("%.3f", p_vec, 3))
    colnames(out) <- c(" ", "F-value", "df 1", "df 2", "p-value")
    out
  }



  output$anova_plot <- renderHighchart({

    dropFunction <- JS("function(event){
                      Shiny.onInputChange('drop_result_aov', [this.y, this.series.name, this.x]);}")

    highchart() %>%
      hc_chart(animation = FALSE) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_title(text = "Drag-around ANOVA") %>%
      hc_tooltip(valueDecimals = 1) %>%
      hc_yAxis(min = 0, max = 5) %>%
      hc_subtitle(text = "2x2 Anova model") %>%
      hc_xAxis(categories = c("Female", "Male"), title = list(text = "Gender")) %>%
      hc_add_series(name = "Treatment", data = aov_dat[aov_dat$Condition == "Treatment", "Mean"],
                    draggableY = TRUE) %>%
      hc_add_series(name = "Control", data = aov_dat[aov_dat$Condition == "Control", "Mean"],
                    draggableY = TRUE) %>%
      hc_legend(labelFormatter = JS("function(e) {return this.name;}")) %>%
      hc_plotOptions(
        series = list(
          point = list(
            events = list(drag = dropFunction)
          ),
          dragPrecisionY = .1,
          dragMinY = 0,
          dragMaxY = 5,
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

  output$ancova_plot <- renderHighchart({

    dropFunction <- JS("function(event){
                      Shiny.onInputChange('drop_result_anco', [this.y, this.series.name, this.x]);}")

    highchart() %>%
      hc_chart(animation = FALSE, type = "line") %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_title(text = "Drag-around ANCOVA") %>%
      hc_tooltip(headerFormat = "", valueDecimals = 2) %>%
      hc_yAxis(min = 0, max = 5) %>%
      hc_xAxis(min = 0, max = 1) %>%
      hc_subtitle(text = "Ancova model") %>%
      hc_legend(labelFormatter = JS("function(e) {return this.name;}")) %>%
      hc_add_series(name = "Treatment", data = 1:2, type = "line", draggableY = TRUE) %>%
      hc_add_series(name = "Control", data = 2:1, type = "line", draggableY = TRUE) %>%
      hc_plotOptions(
        series = list(
          point = list(
            events = list(drag = dropFunction)
          ),
          dragPrecisionY = .05,
          dragPrecisionY = .05,
          dragMinY = 0,
          dragMaxY = 5,
          dragMinX = 0,
          dragMaxX = 1,
          stickyTracking = FALSE
        ),
        column = list(
          stacking = "normal"
        ),
        line = list(
          cursor = "ns-resize"
        )
      ) -> h2


    return(h2)
  })


  makeReactiveBinding("outputText")
  makeReactiveBinding("changed_aov_dat")
  makeReactiveBinding("changed_anco_dat")
  makeReactiveBinding("anova_tab")
  makeReactiveBinding("ancova_tab")

  # outputText <- "Nothing yet."
  outputText <- ""

  anova_tab <- compute_aov(aov_dat[, 3])



  observeEvent(input$drop_result_aov, {
    newy <- round(as.numeric(input$drop_result_aov[1]), 1)
    cond <- input$drop_result_aov[2]
    gend <- ifelse(as.numeric(input$drop_result_aov[3]), "Male", "Female")
    # outputText <<- paste0("Hey! You've just moved the mean of ", tolower(gend), "s from the ", tolower(cond),
                         # " condition to ", newy, ".")
    changed_aov_dat[(aov_dat$Condition == cond) & (aov_dat$Gender == gend), "Mean"] <<- newy

    anova_tab <<- compute_aov(changed_aov_dat[, 3], n = input$n_anova)
    # anova_tab[]

  })

  observeEvent(input$n_anova, {
    newy <- round(as.numeric(input$drop_result_aov[1]), 1)
    cond <- input$drop_result_aov[2]
    gend <- ifelse(as.numeric(input$drop_result_aov[3]), "Male", "Female")
    changed_aov_dat[(aov_dat$Condition == cond) & (aov_dat$Gender == gend), "Mean"] <<- newy

    anova_tab <<- compute_aov(changed_aov_dat[, 3], n = input$n_anova)
  })


  # observeEvent(input$drop_result_anco, {
  #   newy <- round(as.numeric(input$drop_result_anco[1]), 1)
  #   cond <- input$drop_result_anco[2]
  #   changed_anco_dat[(anco_dat$Condition == cond), "Mean"] <<- newy
  #
  #   ancova_tab <<- compute_aov(changed_anco_dat[, 3], n = input$n_ancova)
  #
  # })

  # observeEvent(input$n_ancova, {
  #   newy <- round(as.numeric(input$drop_result_aov[1]), 1)
  #   cond <- input$drop_result_aov[2]
  #   gend <- ifelse(as.numeric(input$drop_result_aov[3]), "Male", "Female")
  #   # outputText <<- paste0("Hey! You've just moved the mean of ", tolower(gend), "s from the ", tolower(cond),
  #   # " condition to ", newy, ".")
  #   changed_dat[(dat$Condition == cond) & (dat$Gender == gend), "Mean"] <<- newy
  #
  #   anova_tab <<- compute_aov(changed_dat[, 3], n = input$n_anova)
  #   # anova_tab[]
  # })

  output$text <- renderText({outputText})

  output$aov_dattab <- renderTable({
    cbind(changed_aov_dat[, 1:2],
          Mean = sprintf("%.1f", changed_aov_dat[, 3]), n = sprintf("%.0f", input$n_anova))
    },
    digits = c(0, 0, 1, 0))


  #### ANOVA RESULTS
  output$anova_results <- renderUI({

    # define CSS tags
    css <- c("#sigcol {background-color: #e6ffb3;}",
             "#inscol {background-color: #ff9999;}")
    # example data frame
    # add the tag inside the cells

    sig <- anova_tab[,"p-value"] < .05

    anova_tab_out <- apply(anova_tab, 2, function(x) paste(x, ifelse(sig, "#sigcol", "#inscol")))

    # generate html table with pander package and markdown package
    htmltab <- markdownToHTML(
      text = pandoc.table.return(
        anova_tab_out,
        style="rmarkdown", split.tables=Inf
      ),
      fragment.only=TRUE
    )
    colortable(htmltab, css)
  })



  #### ANCOVA RESULTS
  output$ancova_results <- renderUI({

    # define CSS tags
    css <- c("#sigcol {background-color: #e6ffb3;}",
             "#inscol {background-color: #ff9999;}")
    # example data frame
    # add the tag inside the cells

    sig <- anova_tab[,"p-value"] < .05

    anova_tab_out <- apply(anova_tab, 2, function(x) paste(x, ifelse(sig, "#sigcol", "#inscol")))

    # generate html table with pander package and markdown package
    htmltab <- markdownToHTML(
      text = pandoc.table.return(
        anova_tab_out,
        style="rmarkdown", split.tables=Inf
      ),
      fragment.only=TRUE
    )
    colortable(htmltab, css)
  })



}

