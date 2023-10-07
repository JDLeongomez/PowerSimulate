#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

Sys.setlocale("LC_ALL", "en_US.UTF-8")
library(shiny)
library(thematic)
library(shinythemes)
library(shinycssloaders)
library(dplyr)
library(knitr)
library(kableExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("slate"),
  
  # Application title
  titlePanel(title = tags$link(rel = "icon",
                               type = "image",
                               href = "https://image.pngaaa.com/393/402393-middle.png"),
             "PowerSimulate"),
  HTML("<center><a href='https://shiny.jdl-svr.lat/PowerSimulate'><img src='powersimulate.svg'' width='600'></a></center>"),
  p(HTML("<center><a style=color:#4075de;  href='https://jdleongomez.info/'>Juan David Leongómez</a>, Universidad El Bosque
      · 2023</a></center>")),
  hr(),
  
  fluidRow(
    column(6, 
           fluidRow( 
             column(1),
             column(10,
                    style='margin-bottom:30px;border:1px solid; padding: 20px;',
                    tags$h3(HTML("<b>English</b>")),
                    p(HTML("<b><i>PowerSimulate</b></i> is a a collection of compact R Shiny applications designed for conducting 
             simulation-based power analyses.<br><br>
             While more straightforward tools for power analysis are available for simple tests like 
             correlations, <em>t</em>-tests, and ANOVA, this application exclusively employs simulations. 
             It serves as both an educational tool for demonstrating the concept of statistical 
             power and as a platform to introduce the importance of simulation-based power 
             analysis for more complex designs.<br><br>
             At this point, there are only 3 <b><i>PowerSimulate</b></i> apps,
             but I plan to add more in the future.
             <br><center>______________________________________</center><br>")),
             p(HTML("<center>Click on the appropriate link to be redirected to the app</center>")),
             tableOutput("eng_kable") %>% 
               withSpinner(color = "#ff5555"),
             p(HTML("<center>______________________________________</center><br>")),
             tags$h6(HTML("These applications are available on my (rather slow) personal Shiny server. 
                        However, if they run too slowly or my server is not working, you can 
                        always run them locally on your computer with R installed. All the code is available on 
                        <a style=color:#ff5555;  href='https://github.com/JDLeongomez/PowerSimulate'>GitHub</a>.<br><br>
                        For more information about statistical power and more advanced techniques, please check 
                        this <a style=color:#ff5555;  href='https://www.youtube.com/playlist?list=PLHk7UNt35ccVdyHqnQ6oXVYA6JBNFrE1x'>video playlis</a> 
                        from my YouTube channel (in Spanish)
                        <a style=color:#ff5555;  href='https://www.youtube.com/@InvestigacionAbierta'>Investigación Abierta</a>, or this 
                        <a style=color:#ff5555;  href='https://doi.org/10.5281/zenodo.3988776'>detailed PDF guide</a> (also in Spanish)."))
             ),
             column(1))),
    column(6, 
           fluidRow(
             column(1),
             column(10,
                  style='margin-bottom:30px;border:1px solid; padding: 20px;',
                  tags$h3(HTML("<b>Español</b>")),
                  p(HTML("<b><i>PowerSimulate</b></i> es una colección de aplicaciones R Shiny compactas diseñadas para llevar a cabo 
             análisis de poder estadístico basados en simulación.<br><br>
             Aunque existen herramientas más directas para el análisis de poder en pruebas simples como 
             correlaciones, pruebas <em>t</em> y ANOVA, esta aplicación se basa exclusivamente en simulaciones. 
             Sirve tanto como una herramienta educativa para demostrar el concepto de poder estadístico, 
             como una plataforma para mostrar la importancia del análisis de poder basado en simulación para 
             diseños más complejos.<br><br>
             En este momento, solo existen 3 aplicaciones de <b><i>PowerSimulate</b></i>, pero planeo crear más en el futuro.
             <br><center>______________________________________</center><br>")),
             p(HTML("<center>Haz clic en el enlace adecuado para abrir a la aplicación correspondiente</center>")),
             tableOutput("esp_kable") %>% 
               withSpinner(color = "#ff5555"),
             p(HTML("<center>______________________________________</center><br>")),
             tags$h6(HTML("Estas aplicaciones están disponible en mi (bastante lento) servidor personal Shiny. 
                        Sin embargo, si corren demasiado lento o mi servidor no funciona, siempre puedes 
                        ejecutarlas localmente en tu ordenador con R instalado. Todo el código está disponible 
                        en <a style=color:#ff5555;  href='https://github.com/JDLeongomez/PowerSimulate'>GitHub</a>.<br><br>
                        Para más información sobre poder estadístico y técnicas más avanzadas tanto en R como en <i>jamovi</i>, consulta esta 
                        <a style=color:#ff5555;  href='https://www.youtube.com/playlist?list=PLHk7UNt35ccVdyHqnQ6oXVYA6JBNFrE1x'>serie de videos</a> 
                        en mi canal de YouTube <a style=color:#ff5555;  href='https://www.youtube.com/@InvestigacionAbierta'>Investigación Abierta</a>, 
                        o esta <a style=color:#ff5555;  href='https://doi.org/10.5281/zenodo.3988776'>guía detallada en PDF</a>.")),
             column(1)))
           )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$eng_kable <- function() {
    tibble(
      Design = c("Correlation", 
                 "Independent t-test",
                 "Paired t-test"),
      `App  (click to open)` = c("PowerSimulate: Correlation", 
                                 "PowerSimulate: Independent t-test",
                                 "PowerSimulate: Paired t-test"), 
      Code = c(rep("GitHub", 3))) |> 
      knitr::kable("html") |> 
      kable_styling("striped", full_width = F)  |> 
      row_spec(0, bold = T, color = "#272b30ff", background = "#ff5555", align = "center") |> 
      row_spec(1:3, align = "center") |>
      column_spec(1, monospace = TRUE) |> 
      column_spec(2, color = "#ff5555", bold = TRUE, italic = TRUE,
                  link = c("https://shiny.jdl-svr.lat/PowerSimulate_corr_EN/",
                           "https://shiny.jdl-svr.lat/PowerSimulate_ind_t_EN/",
                           "https://shiny.jdl-svr.lat/PowerSimulate_pair_t_EN/")) |> 
      column_spec(3, color = "#4075de",
                  link = c("https://github.com/JDLeongomez/PowerSimulate_corr_EN/",
                           "https://github.com/JDLeongomez/PowerSimulate_ind_t_EN/",
                           "https://github.com/JDLeongomez/PowerSimulate_pair_t_EN/"))
  }
  output$esp_kable <- function() {
    tibble(
      Diseño = c("Correlación", 
                 "Prueba t independiente",
                 "Prueba t pareada"),
      `Aplicación (clic para abrir)` = c("PowerSimulate: Correlación", 
                                         "PowerSimulate: Prueba t independiente",
                                         "PowerSimulate: Prueba t pareada"), 
      Código = c(rep("GitHub", 3))) |>
      knitr::kable("html") |> 
      kable_styling("striped", full_width = F)  |> 
      row_spec(0, bold = T, color = "#272b30ff", background = "#ff5555", align = "center") |> 
      row_spec(1:3, align = "center") |>
      column_spec(1, monospace = TRUE) |> 
      column_spec(2, color = "#ff5555", bold = TRUE, italic = TRUE,
                  link = c("https://shiny.jdl-svr.lat/PowerSimulate_corr_ES/",
                           "https://shiny.jdl-svr.lat/PowerSimulate_ind_t_ES/",
                           "https://shiny.jdl-svr.lat/PowerSimulate_pair_t_ES/")) |> 
      column_spec(3, color = "#4075de",
                  link = c("https://github.com/JDLeongomez/PowerSimulate_corr_ES/",
                           "https://github.com/JDLeongomez/PowerSimulate_ind_t_ES/",
                           "https://github.com/JDLeongomez/PowerSimulate_pair_t_ES/"))
  }
}

# Run the application 
shinyApp(ui = ui, server = server)
