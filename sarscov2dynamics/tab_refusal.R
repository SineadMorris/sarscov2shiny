tab_refusal <- tabItem("refusal",
                        fluidRow(
                            column(
                                width = 5, 
                                box(width = 6, status = "primary",  solidHeader = TRUE, #background = "red", 
                                    div(
                                        class = "gobutton",
                                        actionBttn("go2", HTML("&nbsp <b>Simulate model</b>"), icon = icon("share"), 
                                                   style = "jelly", color = "danger", size = "sm", block = TRUE
                                        )
                                    )
                                ),
                                box(
                                    width = 12, title = "Transmission parameters", collapsed = TRUE,
                                    collapsible = TRUE, status = "primary", solidHeader = TRUE, 
                                    selectInput(inputId = "climate", label = "Climate", selected = "NY", choices = c("New York" = "NY", "Delhi", "Jakarta")),
                                    sliderInput(inputId = "npi", label = "Period of NPI adoption (weeks)", 
                                                value = c(16, 55), min = 5, max = 2 * 52, step = 1),
                                    sliderInput(inputId = "efficacy", label = HTML("Efficacy of NPI (% reduction in R<sub>0</sub>)"), 
                                                value = 40, min = 0, max = 90, step = 5)
                                    # sliderInput(inputId = "a22", label = HTML("Relative contact rate of refusal group, c<sub>22</sub>"), 
                                    #             value = 1, min = 0.75, max = 1.5, step = 0.05)
                                ),
                                box(width = 12, title = "Immunological parameters", collapsed = TRUE,
                                    collapsible = TRUE, status = "primary", solidHeader = TRUE, 
                                    sliderInput(inputId = "alpha", label = HTML("Relative transmissibility, &alpha;"), 
                                                value = 1, min = 0, max = 1, step = 0.05),
                                    sliderInput(inputId = "epsilon", label = HTML("Relative susceptibility, &epsilon;"), 
                                                value = 0.7, min = 0, max = 1, step = 0.05),
                                    sliderInput(inputId = "deltaT", label = HTML("Duration of immunity (years), 1/&delta;"), 
                                                value = 1, min = 0.25, max = 10, step = 0.25),
                                    sliderInput(inputId = "rel_severe", label = HTML("Relative severity of secondary disease, x<sub>sev, s</sub>/x<sub>sev, p</sub>"), 
                                                value = 0.5, min = 0, max = 2, step = 0.05)
                                ),
                                box(width = 12, title = "Vaccination parameters", 
                                    collapsible = TRUE, status = "primary", solidHeader = TRUE, 
                                    # sliderInput(inputId = "N2", label = HTML("Fraction who refuse vaccine, N<sub>2</sub>"), 
                                    #             value = 0.3, min = 0, max = 1, step = 0.05),
                                    sliderInput(inputId = "nu", label = HTML("Vaccination rate (% of population per week), &nu;"), 
                                                value = 5e-3 * 100, min = 0, max = 1e-2 * 100, step = 0.05),
                                    sliderInput(inputId = "deltaV", label = HTML("Duration of vaccinal immunity (years), 1/&delta;<sub>vax</sub>"), 
                                                value = 0.5, min = 0.25, max = 10, step = 0.25),
                                    sliderInput(inputId = "tV", label = HTML("Time after which vaccine introduced (years), t<sub>vax</sub>"), 
                                                value = 78/52, min = 0.75, max = 2, step = 0.05)
                                ),
                                box(width = 12, title = "Additional heterogeneity parameters", 
                                    collapsible = TRUE, status = "primary", solidHeader = TRUE, 
                                    sliderInput(inputId = "N2", label = HTML("Fraction who refuse vaccine, N<sub>2</sub>"), 
                                                value = 0.3, min = 0, max = 1, step = 0.05),
                                    sliderInput(inputId = "a22", label = HTML("Relative contact rate of refusal group, c<sub>22</sub>"), 
                                                value = 1, min = 0.75, max = 1.5, step = 0.05)
                                )
                            ),
                            column(
                                width = 7,
                                tabBox(
                                    width = 12, 
                                    tabPanel("Infection classes", plotOutput("plot21", height = 445)),
                                    tabPanel("Immune classes", plotOutput("plot22", height = 445))
                                ),
                                box(
                                    width = 12, title = "Immune landscape", 
                                    status = "primary", #solidHeader = TRUE, 
                                    plotOutput("plot23", height = 740)
                                )
                            )
                        )
)