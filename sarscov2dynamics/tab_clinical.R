tab_clinical <- tabItem("clinical",
                       fluidRow(
                           column(
                               width = 5, 
                               box(width = 6, status = "primary",  solidHeader = TRUE, #background = "red", 
                                   div(
                                       class = "gobutton",
                                       actionBttn("go3", HTML("&nbsp <b>Simulate model</b>"), icon = icon("share"), 
                                                  style = "jelly", color = "danger", size = "sm", block = TRUE
                                       )
                                   )
                               ),
                               box(
                                   width = 12, title = "Transmission parameters", 
                                   collapsible = TRUE, status = "primary", solidHeader = TRUE, collapsed = TRUE,
                                   selectInput(inputId = "climate", label = "Climate", selected = "NY", choices = c("New York" = "NY", "Delhi", "Jakarta")),
                                   sliderInput(inputId = "npi", label = "Period of NPI adoption (weeks)", 
                                               value = c(16, 55), min = 5, max = 2 * 52, step = 1),
                                   sliderInput(inputId = "efficacy", label = HTML("Efficacy of NPI (% reduction in R<sub>0</sub>)"), 
                                               value = 40, min = 0, max = 90, step = 5)
                               ),
                               box(width = 12, title = "Immunological parameters", 
                                   collapsible = TRUE, status = "primary", solidHeader = TRUE, 
                                   sliderInput(inputId = "alpha", label = HTML("Relative transmissibility, &alpha;"), 
                                               value = 1, min = 0, max = 1, step = 0.05),
                                   sliderInput(inputId = "epsilon1", label = HTML("Relative susceptibility following a robust primary immune response, &epsilon;<sub>1</sub>"), 
                                               value = 0.7, min = 0, max = 1, step = 0.05),
                                   sliderInput(inputId = "deltaT", label = HTML("Duration of robust immunity (years), 1/&delta;<sub>1</sub>"), 
                                               value = 1, min = 0.25, max = 10, step = 0.25),
                                   sliderInput(inputId = "rel_severe", label = HTML("Relative severity of secondary disease, x<sub>sev, s</sub>/x<sub>sev, p</sub>"), 
                                               value = 0.5, min = 0, max = 2, step = 0.05)
                               ),
                               box(width = 12, title = "Vaccination parameters", 
                                   collapsible = TRUE, status = "primary", solidHeader = TRUE, collapsed = TRUE,
                                   sliderInput(inputId = "nu", label = HTML("Vaccination rate (% of population per week), &nu;"), 
                                               value = 5e-3 * 100, min = 0, max = 1e-2 * 100, step = 0.05),
                                   sliderInput(inputId = "deltaV", label = HTML("Duration of vaccinal immunity (years), 1/&delta;<sub>vax</sub>"), 
                                               value = 0.5, min = 0.25, max = 10, step = 0.25),
                                   sliderInput(inputId = "tV", label = HTML("Time after which vaccine introduced (years), t<sub>vax</sub>"), 
                                               value = 78/52, min = 0.75, max = 2, step = 0.05)
                               ),
                               box(width = 12, title = "Additional heterogeneity parameters", 
                                   collapsible = TRUE, status = "primary", solidHeader = TRUE, 
                                   sliderInput(inputId = "sigmaP", label = HTML("Transmissibility of severe primary infections relative to mild, &sigma;<sub>P</sub>"), 
                                               value = 1.2, min = 0.5, max = 1.5, step = 0.05),
                                   sliderInput(inputId = "sigmaS", label = HTML("Transmissibility of severe secondary infections relative to mild, &sigma;<sub>S</sub>"), 
                                               value = 1.2, min = 0.5, max = 1.5, step = 0.05),
                                   sliderInput(inputId = "epsilon2", label = HTML("Relative susceptibility following a weak secondary immune response, &epsilon;<sub>2</sub>"), 
                                               value = 1, min = 0.5, max = 1, step = 0.01),
                                   sliderInput(inputId = "delta_rel", label = HTML("Waning of weak immunity relative to robust, &delta;<sub>2</sub>/&delta;<sub>1</sub>"), 
                                               value = 2, min = 1, max = 2.5, step = 0.1)
                               )
                           ),
                           column(
                               width = 7,
                               tabBox(
                                   width = 12, 
                                   tabPanel("Infection classes", plotOutput("plot31", height = 445)),
                                   tabPanel("Immune classes", plotOutput("plot32", height = 445))
                               ),
                               box(
                                   width = 12, title = "Immune landscape", 
                                   status = "primary", #solidHeader = TRUE, 
                                   plotOutput("plot33", height = 740)
                               )
                           )
                       )
)