
##################### Loading packages ##########################

# install.packages("shiny")
require("shiny")

# install.packages("shinydashboard")
library("shinydashboard")


##################################################################


ui <- dashboardPage(skin = "black",
                    
                    dashboardHeader(title = "add app title",titleWidth = 350), 
                    dashboardSidebar(width = 350,
                                     sidebarMenu(menuItem("", tabName = "home", icon = icon("home")),
                                                 menuItem("add 1st tab name", tabName = "tab1"),
                                                 menuItem("add 2nd tab name", tabName = "tab2"),
                                                 menuItem("add 3rd tab name", tabName = "tab3"),
                                                 menuItem("Disclaimer", tabName = "Disclaimer"), HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br>"), img(src='cm_hs_uu-logoengels_diapositief_rgb.png', align = "left")
                    
                                                  ### you can easily add extra tabs by including an extra "menuItem("...", tabName = "")," before the disclaimer ### 
                                                  ### you can remove or add <br> statements in the HTML function (menuItem("Disclaimer")) to adjust the position of the UU logo (make sure it is approximately at the bottom of the screen when opened)
                                                 
                                     )
                    ),
                    
                    
                    dashboardBody(
                      
                      # CSS styles
                      tags$style(HTML(".irs-bar {background: #EAC626}")),
                      tags$style(HTML(".irs-bar {border-top: 1px solid black}")),
                      tags$style(HTML(".irs-bar-edge {background: #EAC626}")),
                      tags$style(HTML(".irs-bar-edge {border: 1px solid black}")),
                      tags$style(HTML(".irs-single {background: #EAC626}")),
                      tags$style(HTML(".selectize-input {border-color: #EAC626}")),
                      tags$style(HTML(".selectize-dropdown {border-color: #EAC626}")),
                      
                      ### note that #EAC626 is the mustard yellow color used in the sidebar. ###
                      ### If possible, you can use this color + different shades of grey (+ black & white) in your figures. ###
                      
                      tags$head(tags$style(HTML('.skin-black .main-header .logo {
                                                background-color: #EAC626;
                                                }
                                                .skin-black .main-header .logo:hover {
                                                background-color: #EAC626;
                                                }
                                                
                                                /* active selected tab in the sidebarmenu */
                                                .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                                                background-color: #EAC626;
                                                }
                                                
                                                /* navbar (rest of the header) */
                                                .skin-black .main-header .navbar {
                                                background-color: #EAC626;
                                                }
                                                
                                                /* toggle button when hovered  */                    
                                                .skin-black .main-header .navbar .sidebar-toggle:hover{
                                                background-color: #EAC626;
                                                }
                                                
                                                /* other links in the sidebarmenu when hovered */
                                                .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
                                                background-color: #EAC626;
                                                }
                                                /* other links in the sidebarmenu */
                                                .skin-black .main-sidebar .sidebar .sidebar-menu a{
                                                background-color: #EAC626;
                                                color: #000000;
                                                }
                                                
                                                /* active selected tab in the sidebarmenu */
                                                .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                                                background-color: #000000;
                                                color: #FFFFFF;
                                                }
                                                
                                                .skin-black .main-sidebar {color: #000000; background-color: #EAC626;}
                                                
                                                '))),
                      tabItems(
                        
                          tabItem(tabName = "Disclaimer", box(width = 12,h5("Terms of Usage Utrecht Unversity Shiny Server", br(), br(), tags$ul(
                                  tags$li("Purpose of the service “utrecht-university.shinyapps.io” is to provide a digital place for trying out, evaluating and/or comparing methods developed by researchers of Utrecht University for the scientific community worldwide. The app and its contents may not be preserved in such a way that it can be cited or can be referenced to. "), tags$li("The web application is provided ‘as is’ and ‘as available’ and is without any warranty. Your use of this web application is solely at your own risk."), tags$li("	You must ensure that you are lawfully entitled and have full authority to upload  data in the web application. The file data must not contain any  data which can raise issues relating to abuse, confidentiality, privacy,  data protection, licensing, and/or intellectual property. You shall not upload data with any confidential or proprietary information that you desire or are required to keep secret. "),tags$li("By using this app you agree to be bound by the above terms."))))),
                          tabItem(tabName = "home", box(width = 12, align = "center",h4("Welcome"),column(12,align = "left", h5("add app background info")))),
                          tabItem(tabName = "tab1", box(width = 12, align = "center", h4("add title"),column(12,align = "left", h5("add content")))),
                          tabItem(tabName = "tab2", box(width = 12, align = "center", h4("add title"),column(12,align = "left", h5("add content")))),
                          tabItem(tabName = "tab3", box(width = 12, align = "center", h4("add title"),column(12,align = "left", h5("add content"))))
                          
                        ### If needed, you can add extra tabItems above by simply adding an extra "tabItem(tabName = ...., box(...))" statement (don't forget to add a comma at the end of "tab3")
                        
                      )))



