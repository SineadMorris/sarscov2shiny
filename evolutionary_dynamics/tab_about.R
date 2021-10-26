tab_about <- tabItem("about",
                     box(title = "Summary", width = 12,
                         #tags$div(
                         HTML(paste(
                             "This interactive dashboard is a companion to the paper <br>
                <b>Epidemiological and evolutionary considerations of SARS-CoV-2 vaccine dosing regimes</b>,<br>
                CM Saad-Roy, SE Morris, CJE Metcalf, MJ Mina, RE Baker, J Farrar, EC Holmes, OG Pybus, 
                AL Graham, SA Levin, BT Grenfell, CE Wagner. Science (2021).
                <br> <br><b> Abstract </b> <br>
                In the face of vaccine dose shortages and logistical challenges, various deployment strategies 
                are being proposed to increase population immunity levels to SARS-CoV-2. How timing of 
                delivery of the second dose affects infection burden but also prospects for the evolution 
                of viral immune escape via a build-up of partially immune individuals are critical questions. 
                Both hinge on the robustness of the immune response elicited by a single dose, compared to 
                natural and two-dose immunity. Building on an existing immuno-epidemiological model, we find 
                that in the short-term, focusing on one dose generally decreases infections, but longer-term 
                outcomes depend on this relative immune robustness. We then explore three scenarios of 
                selection and find that a one-dose policy may increase the potential for antigenic evolution 
                under certain conditions. We highlight the critical need to test viral loads and quantify 
                immune responses after one vaccine dose, and to ramp up vaccination efforts throughout the 
                world.
                <br><br><b>"
                             ,
                             tags$span("To use this dashboard, go to 'Interactive plots' on the sidebar, choose desired parameters 
                                       from the input options, then click 'Simulate model'.",
                                       style="color:#EF4A4A"), sep = "") , "</b>",
                             "<br><br>Details for all models and parameters can be found in the <a href='https://science.sciencemag.org/lookup/doi/10.1126/science.abg8663'>paper</a>."
                         )
                         #)
                     ),
                     box(title = "Framework for the SIR(S) immuno-epidemiological model with one- and two-dose vaccination regimes", width = 12,
                         tags$img(style="height:80%; width:80%", src="framework_both.png"),
                         HTML("<br>(A) Model flow chart depicting transitions between immune classes. (B) Diagram of the inter-dose period (1/&omega;) 
                         considered between the first and second vaccine doses and its relationship to the rate of administration of the first vaccine dose (&nu;). 
                         (C) Representative schematic of societal composition of various immune classes for the SIR(S) model with no vaccination (left), 
                         the extended model with a shorter inter-dose period (middle), and the extended model with a long inter-dose period (right). 
                         (D) Potential viral evolution scenarios under different vaccine regimes. Schematic shows the potential net viral adaptation rate associated with the I<sub>S</sub>, 
                         I<sub>S<sub>1</sub></sub>, and I<sub>S<sub>2</sub></sub> infection classes (filled dots; colours as in (A)) under three different scenarios. 
                         Further details can be found in the paper.")
                     )
)
