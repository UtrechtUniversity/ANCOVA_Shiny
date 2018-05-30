library(pander)
library(markdown)
library(stringr)
library(shiny)

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





  dat <- data.frame(cbind(Condition = rep(c("Treatment", "Control"), 2),
                          Gender = rep(c("Female", "Male"), each = 2)),
                    cbind(mu = c(3, 2, 2, 3)))
  rownames(dat) <- paste0("Group_", 1:nrow(dat))

  changed_dat <- dat

  # Dataset <- reactive({
  #   dat
  # })

  # Number per group
  # n_g <- 25

  # gen_exact_data <- function(n = 25, mean = 0, sd = 1) {
    # as.numeric(sd * (scale(rnorm(n = n)) + mean))
  # }

  # compute_aov_old <- function(data) {
  #   aovdat <- rbind(
  #     cbind("Treatment", "Female", gen_exact_data(25, mean = data[1, 3])),
  #     cbind("Control", "Female",   gen_exact_data(25, mean = data[2, 3])),
  #     cbind("Treatment", "Male",   gen_exact_data(25, mean = data[3, 3])),
  #     cbind("Control", "Male",     gen_exact_data(25, mean = data[4, 3]))
  #   )
  #   aovdat <- as.data.frame(aovdat)
  #   aovdat[, 3] <- as.numeric(aovdat[, 3])
  #   colnames(aovdat) <- c("Condition", "Gender", "x")
  #   aov(x ~ Condition * Gender, data = aovdat)
  # }

  svar <- function(x) mean((x - mean(x))^2)


  compute_aov <- function(mus) {

    MSb <-  svar(mus) * 100
    MSg <-  svar(c(sum(mus[1:2]),     sum(mus[3:4]))     / 2) * 100
    MSc <-  svar(c(sum(mus[c(1, 3)]), sum(mus[c(2, 4)])) / 2) * 100
    MSint <-  MSb - MSg - MSc

    p_vec <- pf(q = c(MSc, MSg, MSint), df1 = 1, df2 = 96, lower.tail = FALSE)

    out <- cbind(c("Condition", "Gender", "Condition:Gender"),
                 sprintf("%.1f", c(MSb, MSg, MSc)),
                 sprintf("%.3f", p_vec, 3))
    colnames(out) <- c(" ", "F-value", "p-value")
    out
  }



  output$anova_plot <- renderHighchart({

    # dat <- Dataset()
    dropFunction <- JS("function(event){
                      Shiny.onInputChange('drop_result', [this.y, this.series.name, this.x]);}")

    highchart() %>%
      hc_chart(animation = FALSE) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_title(text = "Drag-around ANOVA") %>%
      hc_tooltip(valueDecimals = 2) %>%
      hc_yAxis(min = 0, max = 5) %>%
      hc_subtitle(text = "2x2 Anova model") %>%
      hc_xAxis(categories = c("Female", "Male"), title = list(text = "Gender")) %>%
      hc_add_series(name = "Treatment", data = dat[dat$Condition == "Treatment", "mu"],
                    draggableY = TRUE) %>%
      hc_add_series(name = "Control", data = dat[dat$Condition == "Control", "mu"],
                    draggableY = TRUE) %>%
      hc_legend(labelFormatter = JS("function(e) {return this.name;}")) %>%
      hc_plotOptions(
        series = list(
          point = list(
            events = list(drag = dropFunction)
          ),
          dragPrecisionY = .5,
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


  makeReactiveBinding("outputText")
  makeReactiveBinding("changed_dat")
  makeReactiveBinding("anova_tab")

  # outputText <- "Nothing yet."
  outputText <- ""

  anova_tab <- compute_aov(dat[, 3])


  observeEvent(input$drop_result, {
    newy <- round(as.numeric(input$drop_result[1]), 1)
    cond <- input$drop_result[2]
    gend <- ifelse(as.numeric(input$drop_result[3]), "Male", "Female")
    # outputText <<- paste0("Hey! You've just moved the mean of ", tolower(gend), "s from the ", tolower(cond),
                         # " condition to ", newy, ".")
    changed_dat[(dat$Condition == cond) & (dat$Gender == gend), "mu"] <<- newy

    anova_tab <<- compute_aov(changed_dat[, 3])
    # anova_tab[]

  })

  output$text <- renderText({outputText})

  output$dattab <- renderTable({changed_dat}, digits = 1)


  output$anova_results <- renderUI({
    # define CSS tags
    css <- c("#sigcol {background-color: #e6ffb3;}",
             "#inscol {background-color: #ff9999;}")
    # example data frame
    # add the tag inside the cells

    sig <- anova_tab[,3] < .05

    anova_tab <- apply(anova_tab, 2, function(x) paste(x, ifelse(sig, "#sigcol", "#inscol")))

    # generate html table with pander package and markdown package
    htmltab <- markdownToHTML(
      text = pandoc.table.return(
        anova_tab,
        style="rmarkdown", split.tables=Inf
      ),
      fragment.only=TRUE
    )
    colortable(htmltab, css)
  })

    # renderTable({
    # # as.data.frame(summary(anova_tab)[[1]])
    # anova_tab
    # }, rownames = FALSE, digits = 2, align = 'r')



}

