
##################### Loading packages ##########################

library(shiny)
library(shinydashboard)
library(highcharter)
library(DT)
library(pander)
library(markdown)
library(stringr)
library(shiny)



uu_color <- " #ffcd00"



##################################################################



ui <- dashboardPage(
  skin = "black",

  dashboardHeader(title = "ANOVA and ANCOVA demonstrations", titleWidth = 350),
  dashboardSidebar(width = 350,
                   sidebarMenu(
                     # menuItem("", tabName = "home", icon = icon("home")),
                     menuItem("ANOVA", tabName = "anova"),
                     menuItem("ANCOVA", tabName = "ancova", selected = TRUE),
                     # menuItem("add 3rd tab name", tabName = "tab3"),
                     menuItem("Disclaimer", tabName = "Disclaimer"),

                     HTML("<br><br><br><br>"),
                     div(" -  Shiny app by",
                         a(href="https://www.uu.nl/staff/KTMulder/0",
                           target = "_blank",
                           "Kees Mulder"),align="left", style = "font-size: 10pt"),

                     div(" -  Base R code by",
                         a(href="https://www.uu.nl/staff/KTMulder/0",target="_blank",
                           "Kees Mulder"),align="left", style = "font-size: 10pt"),

                     div(" -  Base Layout by",
                         a(href="https://www.uu.nl/medewerkers/KMLek/0",target="_blank",
                           "Kimberley Lek"),align="left", style = "font-size: 10pt"),

                     div(" -  Shiny source files:",
                         a(href="https://github.com/EducationalShinyUU/ANCOVA_Shiny",
                           target="_blank","GitHub"),align="left", style = "font-size: 10pt"),

                     HTML("<br><br><br><br><br><br><br><br><br><br><br><br>"),
                     img(src = 'logo.png', align = "left")

                     ### you can easily add extra tabs by including an extra "menuItem("...",
                     ### tabName = "")," before the disclaimer ### you can remove or add <br>
                     ### statements in the HTML function (menuItem("Disclaimer")) to adjust the
                     ### position of the UU logo (make sure it is approximately at the bottom of the
                     ### screen when opened)
                   )
                   # href="http://www.github.com/keesmulder",

  ),

  dashboardBody(

    # CSS styles
    tags$style(HTML(paste0(".irs-bar {background:",  uu_color, "}"))),
    tags$style(HTML(paste0(".irs-bar {border-top: 1px solid black}"))),
    tags$style(HTML(paste0(".irs-bar-edge {background:",  uu_color, "}"))),
    tags$style(HTML(paste0(".irs-bar-edge {border: 1px solid black}"))),
    tags$style(HTML(paste0(".irs-single {background:",  uu_color, "}"))),
    tags$style(HTML(paste0(".selectize-input {border-color:",  uu_color, "}"))),
    tags$style(HTML(paste0(".selectize-dropdown {border-color:",  uu_color, "}"))),

    ### note that uu_color is the mustard yellow color used in the sidebar. ###
    ### If possible, you can use this color + different shades of grey (+ black & white) in your figures. ###

    tags$head(tags$style(HTML(
      paste0('.skin-black .main-header .logo {
                               background-color:',  uu_color, ';
                               }
                               .skin-black .main-header .logo:hover {
                               background-color:',  uu_color, ';
                               }

                               /* active selected tab in the sidebarmenu */
                               .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                               background-color:',  uu_color, ';
                               }

                               /* navbar (rest of the header) */
                               .skin-black .main-header .navbar {
                               background-color:',  uu_color, ';
                               }

                               /* toggle button when hovered  */
                               .skin-black .main-header .navbar .sidebar-toggle:hover{
                               background-color:',  uu_color, ';
                               }

                               /* other links in the sidebarmenu when hovered */
                               .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
                               background-color:',  uu_color, ';
                               }
                               /* other links in the sidebarmenu */
                               .skin-black .main-sidebar .sidebar .sidebar-menu a{
                               background-color:',  uu_color, ';
                               color: #000000;
                               }

                               /* active selected tab in the sidebarmenu */
                               .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                               background-color: #000000;
                               color: #FFFFFF;
                               }

                               .skin-black .main-sidebar {color: #000000; background-color:',  uu_color, ';}

                               ')
    ))),
    tabItems(
      tabItem(tabName = "Disclaimer", box(width = 12,h5("Terms of Usage Utrecht Unversity Shiny Server", br(), br(), tags$ul(
        tags$li("Purpose of the service “utrecht-university.shinyapps.io” is to provide a digital place for trying out, evaluating and/or comparing methods developed by researchers of Utrecht University for the scientific community worldwide. The app and its contents may not be preserved in such a way that it can be cited or can be referenced to. "), tags$li("The web application is provided ‘as is’ and ‘as available’ and is without any warranty. Your use of this web application is solely at your own risk."), tags$li("	You must ensure that you are lawfully entitled and have full authority to upload  data in the web application. The file data must not contain any  data which can raise issues relating to abuse, confidentiality, privacy,  data protection, licensing, and/or intellectual property. You shall not upload data with any confidential or proprietary information that you desire or are required to keep secret. "),tags$li("By using this app you agree to be bound by the above terms."))))),

      # tabItem(tabName = "home",
      #         box(width = 12, align = "center",h4("Welcome"),
      #             column(12, align = "left",
      #                     h5("add app background info")))),

      tabItem(tabName = "anova",
              box(width = 12, align = "center",
                  column(12, align = "left",
                         highchartOutput("anova_plot"))
              ),
              box(width = 12, align = "center",
                  column(width = 6,
                         h4("Data"),
                         textOutput("text", container = h5),
                         tableOutput("dattab")
                  ),
                  column(width = 6,
                         # box(width = 12, align = "center",
                         h4("Anova"),
                         uiOutput("anova_results")
                  )
              ),
              box(width = 12, align = "center",
                  sliderInput("n_anova", "Sample size (n)", 10, 200, 60, 10)

              )

      ),

      tabItem(tabName = "ancova",
              box(width = 12, align = "center",
                  column(12, align = "left",
                         highchartOutput("ancova_plot"))
              )
              # ,
              # box(width = 12, align = "center",
              #     column(width = 6,
              #            h4("Data"),
              #            textOutput("text_ancova", container = h5),
              #            tableOutput("dattab_ancova")
              #     ),
              #     column(width = 6,
              #            # box(width = 12, align = "center",
              #            h4("Ancova"),
              #            uiOutput("ancova_results")
              #     )
              # ),
              # box(width = 12, align = "center",
              #     sliderInput("n_ancova", "Sample size (n)", 10, 200, 60, 10)
              #
              # )

              # box(width = 12, align = "center",
              #     column(width = 6,
              #            h4("Data"),
              #            textOutput("text", container = h5),
              #            tableOutput("dattab")
              #     ),

      )
    )
  )
)



