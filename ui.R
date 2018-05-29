
##################### Loading packages ##########################

library(shiny)
library(shinydashboard)
library(highcharter)
library(rCharts)
library(knitr)


uu_color <- " #ffcd00"



##################################################################



ui <- dashboardPage(
  skin = "black",

  dashboardHeader(title = "ANOVA and ANCOVA demonstrations", titleWidth = 350),
  dashboardSidebar(width = 350,
                   sidebarMenu(
                     # menuItem("", tabName = "home", icon = icon("home")),
                               menuItem("ANOVA", tabName = "tab1"),
                               menuItem("ANCOVA", tabName = "tab2"),
                               # menuItem("add 3rd tab name", tabName = "tab3"),
                               menuItem("Disclaimer", tabName = "Disclaimer"),
                               HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br>"),
                               img(src = 'logo.png', align = "left")

                               ### you can easily add extra tabs by including an extra "menuItem("...",
                               ### tabName = "")," before the disclaimer ### you can remove or add <br>
                               ### statements in the HTML function (menuItem("Disclaimer")) to adjust the
                               ### position of the UU logo (make sure it is approximately at the bottom of the
                               ### screen when opened)
                   )
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

      tabItem(tabName = "tab1",
              box(width = 12, align = "center",
                  column(12, align = "left",
                         highchartOutput("anova_plot"))
              ),
              box(width = 12, align = "center",
                  h4("Data"),
                  textOutput("text", container = h5),
                  tableOutput("dattab")
              ),
              box(width = 12, align = "center",
                  h4("Anova"),
                  tableOutput("anova_results")
              # ),
              # box(width = 12, align = "center",
              #     h4("Anova (text)"),
              #     verbatimTextOutput("anova_text")
              )),


      tabItem(tabName = "tab2", box(width = 12, align = "center", h4("add title"),column(12,align = "left", h5("add content"))))


    )
  )
)



