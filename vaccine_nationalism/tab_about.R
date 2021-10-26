tab_about <- tabItem("about",
                     box(title = "Summary", width = 12,
                         #tags$div(
                         HTML(paste(
                             "This interactive dashboard is a companion to the paper <br>
                <b>Vaccine nationalism and the dynamics and control of SARS-CoV-2</b>,<br>
                CE Wagner*, CM Saad-Roy*, SE Morris, RE Baker, MJ Mina, J Farrar, EC Holmes,
                OG Pybus, AL Graham, EJ Emanuel, SA Levin, CJE Metcalf, BT Grenfell (*equal contribution). 
                Science (2021).<br> <br>
               
                <b> Abstract </b> <br>
                Vaccines provide powerful tools to mitigate the enormous public health and economic costs 
                that the ongoing SARS-CoV-2 pandemic continues to exert globally, yet vaccine distribution 
                remains unequal among countries. To examine the potential epidemiological and evolutionary 
                impacts of 'vaccine nationalism', we extend previous models to include simple scenarios of 
                stockpiling between two regions. In general, when vaccines are widely available and the 
                immunity they confer is robust, sharing doses minimizes total cases across regions. A number 
                of subtleties arise when the populations and transmission rates in each region differ, depending 
                on evolutionary assumptions and vaccine availability. When the waning of natural immunity 
                contributes most to evolutionary potential, sustained transmission in low access regions 
                results in an increased potential for antigenic evolution, which may result in the emergence 
                of novel variants that affect epidemiological characteristics globally. Overall, our results 
                stress the importance of rapid equitable vaccine distribution for global control of the pandemic.
                <br><br><b>"
                             ,
                             tags$span("To use this dashboard, go to 'Interactive plots' on the 
                             sidebar, choose desired parameters from the input options, then 
                                       click 'Simulate model'.",
                                       style="color:#EF4A4A"), sep = "") , "</b>",
                             "<br><br>Here we focus on the decoupled model. Details for all models and parameters can be 
                             found in the <a href='https://science.sciencemag.org/content/early/2021/08/16/science.abj7364'>paper</a>."
                         )
                         #)
                     ),
                     box(title = "Framework for the SIR(S) immuno-epidemiological model 
                         incorporating vaccine distribution between two countries", width = 12,
                         tags$img(style="height:60%; width:75%", src="framework2.png"),
                         HTML("<br>Schematic of the decoupled model framework. One country has high vaccine 
                         availability (high access region, 'HAR'), and the other has low vaccine 
                         availability (low access region, 'LAR'). 
                         The HAR shares a fraction, f, of its total vaccine supply with the LAR. 
                         In all other respects, the epidemiological dynamics of both countries are independent (i.e. decoupled), 
                         with no immigration between them (&eta;=0).
                         Further details can be found in the paper.")
                     )
)
