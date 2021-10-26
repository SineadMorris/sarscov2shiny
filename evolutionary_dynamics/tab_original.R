tab_original <- tabItem("original",
                        fluidRow(
                            column(
                                width = 5, 
                                box(width = 6, status = "primary",  solidHeader = TRUE, #background = "red", 
                                    div(
                                        class = "gobutton",
                                        actionBttn("go1", HTML("&nbsp <b>Simulate model</b>"), icon = icon("share"), 
                                                   style = "jelly", color = "danger", size = "sm", block = TRUE
                                        )
                                    )
                                ),
                                box(width = 12, title = "Vaccination parameters", 
                                    collapsible = TRUE, status = "primary", solidHeader = TRUE, 
                                    # Choose 1 or 2 doses
                                    selectInput(inputId = "doses", label = "Strategy", selected = "two", choices = c("One dose" = "one", "Two dose" = "two")),
                                    conditionalPanel(
                                        "input.doses == 'two'",
                                        sliderInput(inputId = "L", label = HTML("Inter-dose period (weeks), L (1/&omega;)"), 
                                                    value = 12, min = 4, max = 52, step = 1)
                                    ),
                                    sliderInput(inputId = "nu0", label = HTML("Maximum rate of first dose administration (% of population per week), &nu;<sub>0</sub>"), 
                                                value = 0.01 * 100, min = 0, max = 5e-2 * 100, step = 0.05),
                                    sliderInput(inputId = "tV", label = HTML("Week of vaccine introduction, t<sub>vax</sub>"), 
                                                value = 48, min = 40, max = 60, step = 1),
                                    selectInput(inputId = "switch", label = "Include change in strategy?", selected = "two", 
                                                choices = c("No" = "no", 
                                                            "Yes: increase maximum rate of first dose administration" = "yes1", 
                                                            "Yes: switch to recommended 2-dose strategy" = "yes2")),
                                    conditionalPanel(
                                        "input.switch != 'no'",
                                        sliderInput(inputId = "t.switch", label = HTML("Week of change in strategy"), 
                                                    value = 60, min = 60, max = 104, step = 1)
                                    ),
                                    conditionalPanel(
                                        "input.switch == 'yes1'",
                                        sliderInput(inputId = "nu0.switch", label = HTML("Relative increase in maximum rate of first dose administration"), 
                                                    value = 3, min = 1, max = 5, step = 0.5)
                                    ),
                                ),
                                box(
                                    width = 12, title = "Transmission parameters", 
                                    collapsible = TRUE, status = "primary", solidHeader = TRUE, collapsed = TRUE,
                                    selectInput(inputId = "climate", label = "Climate", selected = "NY", choices = c("New York" = "NY", "Delhi", "Jakarta")),
                                    sliderInput(inputId = "tred1", label = "Week of NPI introduction", 
                                                value = 8, min = 5, max = 45, step = 1),
                                    sliderInput(inputId = "npi1", label = "Duration of first NPI period (weeks)", 
                                                value = 40, min = 5, max = 1 * 52, step = 1),
                                    sliderInput(inputId = "npi2", label = "Duration of second NPI period (weeks)", 
                                                 value = 32, min = 5, max = 1 * 52, step = 1),
                                    sliderInput(inputId = "eff1", label = HTML("Efficacy of first NPI period (% reduction in R<sub>0</sub>)"), 
                                                value = 55, min = 0, max = 90, step = 5),
                                    sliderInput(inputId = "eff2", label = HTML("Efficacy of second NPI period (% reduction in R<sub>0</sub>)"), 
                                                value = 41.5, min = 0, max = 90, step = 5)
                                    
                                ),
                                box(width = 12, title = "Relative susceptibility parameters", 
                                    collapsible = TRUE, status = "primary", solidHeader = TRUE, 
                                    #  sliderInput(inputId = "alpha", label = HTML("Relative transmissibility, &alpha;"), 
                                    #              value = 1, min = 0, max = 1, step = 0.05),
                                    sliderInput(inputId = "epsilon", label = HTML("After primary infection, &epsilon;"), 
                                                value = 0.7, min = 0, max = 1, step = 0.05),
                                    sliderInput(inputId = "epsilonV1", label = HTML("After dose 1, &epsilon;<sub>V<sub>1</sub></sub>"), 
                                                value = 0.1, min = 0, max = 1, step = 0.05),
                                    sliderInput(inputId = "epsilonV2", label = HTML("After dose 2, &epsilon;<sub>V<sub>2</sub></sub>"), 
                                                value = 0.05, min = 0, max = 1, step = 0.05),
                                    sliderInput(inputId = "epsilon1", label = HTML("After waning of dose 1, &epsilon;<sub>1</sub>"), 
                                                value = 0.7, min = 0, max = 1, step = 0.05),
                                    sliderInput(inputId = "epsilon2", label = HTML("After waning of dose 2, &epsilon;<sub>2</sub>"), 
                                                value = 0.7, min = 0, max = 1, step = 0.05)
                                ),
                                box(width = 12, title = "Duration of immunity parameters", 
                                    collapsible = TRUE, status = "primary", solidHeader = TRUE, 
                                    sliderInput(inputId = "deltaT", label = HTML("Natural immunity (years), 1/&delta;"), 
                                                value = 1, min = 0.25, max = 10, step = 0.25),
                                    sliderInput(inputId = "Rho1", label = HTML("Immunity from dose 1 (years), 1/&rho;<sub>1</sub>"), 
                                                value = 1, min = 0.25, max = 10, step = 0.25),
                                    sliderInput(inputId = "Rho2", label = HTML("Immunity from dose 2 (years), 1/&rho;<sub>2</sub>"), 
                                                value = 1, min = 0.25, max = 10, step = 0.25)
                                ),
                                box(width = 12, title = "Fractions of severe cases", 
                                    collapsible = TRUE, status = "primary", solidHeader = TRUE, collapsed = TRUE,
                                    sliderInput(inputId = "primary.burden", label = HTML("Primary infection, x<sub>sev,P</sub>"), 
                                                value = 0.14, min = 0, max = 0.14, step = 0.01),
                                    sliderInput(inputId = "secondary.burden", label = HTML("Secondary infection, x<sub>sev,S</sub>"), 
                                                value = 0.07, min = 0, max = 0.14, step = 0.01),
                                    sliderInput(inputId = "vaccine.burden", label = HTML("Infection after vaccination, x<sub>sev,V</sub>"), 
                                                value = 0.14, min = 0, max = 0.14, step = 0.01),
                                    sliderInput(inputId = "one.dose.burden", label = HTML("Infection after waning of dose 1, x<sub>sev,1</sub>"), 
                                                value = 0.07, min = 0, max = 0.14, step = 0.01),
                                    sliderInput(inputId = "two.doses.burden", label = HTML("Infection after waning of dose 2, x<sub>sev,2</sub>"), 
                                                value = 0, min = 0, max = 0.14, step = 0.01)
                                )
                            ),
                            column(
                                width = 7,
                                #conditionalPanel( "input.switch == 'no' && (input.go1 | input.go1 == '0')",
                              #  conditionalPanel( "input.plotVaccine == 'TRUE'",
                               #     box(
                               #     width = 12, title = "Vaccination progress (no change in strategy)", 
                               #     status = "primary", #solidHeader = TRUE, 
                               #     plotOutput("plot11", height = 250)
                               #     )
                              #  ),
                                box(
                                    width = 12, title = "Case burden", 
                                    status = "primary", #solidHeader = TRUE, 
                                    plotOutput("plot12", height = 400)
                                ),
                                box(
                                    width = 12, title = "Evolutionary scenarios", 
                                    status = "primary", #solidHeader = TRUE, 
                                    plotOutput("plot12b", height = 250)
                                ),
                                box(
                                    width = 12, title = "Immune landscape", 
                                    status = "primary", #solidHeader = TRUE, 
                                    plotOutput("plot13", height = 650),
                                    tags$img(style="height:100%; width:100%", src="legend.png")
                                )
                            )
                        )
)