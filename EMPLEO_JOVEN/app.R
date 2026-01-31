#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(rvest, dplyr, tidytext, tm, tidyverse,
               ggplot2, wordcloud, wordcloud2, plotly,
               igraph, ggraph, tidygraph, forcats, httr,
               KoboconnectR, devtools, haven, readxl,
               stringi, readxl, labelled, units, sf,
               sp, geoR, spdep, leaflet, tidyverse,
               scatterplot3d, ggrepel, cowplot, ggmap,
               shiny, fresh, DT, readr, bs4Dash)


# Cargar datos
setwd("C:/Users/jose2/OneDrive/Desktop/R_dat/FAO/1052/DB_DASH/EMPLEO_JOVEN")
load("geo_prov.Rdata")  # Aseg??rate que el objeto se llama geo_prov y tiene num_emp

# Tema FAO personalizado
lot_colour <- "#009EDB"  # Azul ONU

theme <- create_theme(
  bs4dash_color(
    lime = "#005B82",
    olive = "#D9D9D9",
    purple = lot_colour
  ),
  bs4dash_status(
    primary = "#009EDB",
    info = "#A7D3F4"
  )
)

# UI
ui <- dashboardPage(
  title = "EMPLEO AGROJOVEN",
  freshTheme = theme,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  
  header = dashboardHeader(
    status = "lime",
    title = dashboardBrand(
      title = "EMPLEO AGROJOVEN",
      color = "olive",
      image = "FAO_logo.png"
    ),
    controlbarIcon = icon("circle-info"),
    fixed = TRUE,
    rightUi = dropdownMenu(
      badgeStatus = "info",
      type = "notifications",
      notificationItem(text = "Success", status = "success", icon = icon("circle-check")),
      notificationItem(text = "Warning", status = "warning", icon = icon("circle-exclamation")),
      notificationItem(text = "Error", status = "danger", icon = icon("circle-xmark"))
    )
  ),
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem("MENU PRINCIPAL", tabName = "home", icon = icon("home")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("bar-chart"))
    )
  ),
  
  controlbar = dashboardControlbar(),
  footer = dashboardFooter(left = "FAO ECUADOR", right = "2025"),
  
  body = dashboardBody(
    tabItems(
      
      # HOME
      tabItem(
        tabName = "home",
        jumbotron(
          title = "??BIENVENIDOS!",
          status = "info",
          lead = "Emprendimientos apoyados por parte de FAO ECUADOR con su programa EMPLEO AGROJOVEN",
          href = "https://www.fao.org/ecuador/noticias/detail-events/en/c/1709351/",
          btnName = "M??S INFORMACI??N"
        ),
        fluidRow(
          userBox(
            collapsible = FALSE,
            title = userDescription(
              title = "TITULO",
              subtitle = "SUBTITULO",
              image = "IMAGEN",
              type = 1
            ),
            status = "purple",
            "TEXTO."
          ),
          box(
            title = "TITULO",
            width = 6,
            collapsible = FALSE,
            blockQuote("TEXTO", color = "purple")
          ),
          box(
            title = "TITULO",
            width = 6,
            collapsible = FALSE,
            blockQuote("TEXTO", color = "purple")
          ),
          userBox(
            collapsible = FALSE,
            title = userDescription(
              title = "TITULO",
              subtitle = "SUBTITULO",
              image = "",
              type = 1
            ),
            status = "purple",
            "TEXTO"
          )
        )
      ),
      
      # DASHBOARD
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(
            title = "Mapa de Emprendimientos",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            leafletOutput("mapa_emprendimientos", height = "600px")
          )
        )
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  output$mapa_emprendimientos <- renderLeaflet({
    
    # Paleta de colores seg??n n??mero de emprendimientos
    pal <- colorNumeric(palette = "YlGnBu", domain = df_unido$num_emp)
    
    leaflet(df_unido) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(num_emp),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        label = ~paste0("Freq: ", num_emp)
      ) %>%
      addLegend(
        pal = pal,
        values = df_unido$num_emp,
        title = "Frecuencia",
        position = "bottomright"
      )
  })
  
  # Mostrar pesta??a HOME al iniciar
  observe({
    updateTabItems(session, "sidebarMenuid", "home")
  })
}

# Ejecutar app
shinyApp(ui, server)

# Cargar datos
setwd("C:/Users/jose2/OneDrive/Desktop/R_dat/FAO/1052/DB_DASH/EMPLEO_JOVEN")
load("geo_prov.Rdata")  # Aseg??rate que el objeto se llama geo_prov y tiene num_emp

# Tema FAO personalizado
lot_colour <- "#009EDB"  # Azul ONU

theme <- create_theme(
  bs4dash_color(
    lime = "#005B82",
    olive = "#D9D9D9",
    purple = lot_colour
  ),
  bs4dash_status(
    primary = "#009EDB",
    info = "#A7D3F4"
  )
)

# UI
ui <- dashboardPage(
  title = "EMPLEO AGROJOVEN",
  freshTheme = theme,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  
  header = dashboardHeader(
    status = "lime",
    title = dashboardBrand(
      title = "EMPLEO AGROJOVEN",
      color = "olive",
      image = "FAO_logo.png"
    ),
    controlbarIcon = icon("circle-info"),
    fixed = TRUE,
    rightUi = dropdownMenu(
      badgeStatus = "info",
      type = "notifications",
      notificationItem(text = "Success", status = "success", icon = icon("circle-check")),
      notificationItem(text = "Warning", status = "warning", icon = icon("circle-exclamation")),
      notificationItem(text = "Error", status = "danger", icon = icon("circle-xmark"))
    )
  ),
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem("MENU PRINCIPAL", tabName = "home", icon = icon("home")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("bar-chart"))
    )
  ),
  
  controlbar = dashboardControlbar(),
  footer = dashboardFooter(left = "FAO ECUADOR", right = "2025"),
  
  body = dashboardBody(
    tabItems(
      
      # HOME
      tabItem(
        tabName = "home",
        jumbotron(
          title = "BIENVENIDOS!",
          status = "info",
          lead = "Emprendimientos apoyados por parte de FAO ECUADOR con su programa EMPLEO AGROJOVEN",
          href = "https://www.fao.org/ecuador/noticias/detail-events/en/c/1709351/",
          btnName = "MAS INFORMACION"
        ),
        fluidRow(
          userBox(
            collapsible = FALSE,
            title = userDescription(
              title = "TITULO",
              subtitle = "SUBTITULO",
              image = "IMAGEN",
              type = 1
            ),
            status = "purple",
            "TEXTO."
          ),
          box(
            title = "TITULO",
            width = 6,
            collapsible = FALSE,
            blockQuote("TEXTO", color = "purple")
          ),
          box(
            title = "TITULO",
            width = 6,
            collapsible = FALSE,
            blockQuote("TEXTO", color = "purple")
          ),
          userBox(
            collapsible = FALSE,
            title = userDescription(
              title = "TITULO",
              subtitle = "SUBTITULO",
              image = "",
              type = 1
            ),
            status = "purple",
            "TEXTO"
          )
        )
      ),
      
      # DASHBOARD
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(
            title = "Mapa de Emprendimientos",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            leafletOutput("mapa_emprendimientos", height = "600px")
          )
        )
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  output$mapa_emprendimientos <- renderLeaflet({
    
    # Paleta de colores seg??n n??mero de emprendimientos
    pal <- colorNumeric(palette = "YlGnBu", domain = df_unido$num_emp)
    
    leaflet(df_unido) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(num_emp),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        label = ~paste0("Freq: ", num_emp)
      ) %>%
      addLegend(
        pal = pal,
        values = df_unido$num_emp,
        title = "Frecuencia",
        position = "bottomright"
      )
  })
  
  # Mostrar pesta??a HOME al iniciar
  observe({
    updateTabItems(session, "sidebarMenuid", "home")
  })
}

# Ejecutar app
shinyApp(ui, server)


