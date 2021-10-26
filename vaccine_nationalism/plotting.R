## Plotting dose regime,  case burden,  and landscape function -----------------------------------

plots.doses.switch <- function(time, R0.list1, R0.list2, input, alpha = 1, d = 0.5,
                             #start.value = 45, 
                             weight.IS = 0.05, weight.IS1 = 0.3, weight.IS2 = 0.05,
                             weight2.IS = 0.05, weight2.IS1 = 1, weight2.IS2 = 0.05,
                             weight3.IS = 0.8, weight3.IS1 = 1, weight3.IS2 = 0.8,
                             weight4.IS = 1, weight4.IS1 = 0.05, weight4.IS2 = 0.05
){
    ## Plot settings -------------
    sp.col=rgb(0.75,0.75,0.75)
    ip.col=rgb(0.8,0.28,0.23)
    r.col=rgb(0.36,0.33,0.58)
    ss.col=rgb(0.71,0.64,0.83)
    ss1.col=rgb(1,0.5,0)
    ss2.col=rgb(1,0.9,0)
    is.col=rgb(1,0.81,0.85)
    v2.col=rgb(0.33,0.51,0.21)
    v1.col=rgb(0.33,0.81,0.21)
    iv.col=rgb(0.6,0.4,0.4)
    is2.col=rgb(1,0.6,1)
    is1.col=rgb(1,0.1,0.5)
    
    fspace <- 3
    
    mytheme <- theme_classic((size = 18)) + 
        theme(legend.position = "none", strip.text.x = element_text(size = 17), 
              strip.background = element_blank())
    
    countrylabs <- c(A = "High access region (HAR)", B = "Low access region (LAR)")
    
    ## ICs and parameters -------------
    xstart = c(SP = 1 - 1e-9,  IP = 1e-9,  R = 0,  SS = 0,  IS = 0,  V1 = 0, 
               V2 = 0,  IV = 0,  SS1 = 0,  SS2 = 0,  IS1 = 0,  IS2 = 0) 
    
    tvax <- as.numeric(input$tV)
    
    if (input$doses == "one"){
        omega.init <- 0
    } else{
        omega.init <- 1/as.numeric(input$L)
    }
    
    nu.init <- as.numeric(input$nu0/100) * 16 ^ (- omega.init) 
    
    nu.switch <- nu.init
    omega.switch <- omega.init
    
    if (input$switch == "yes") {
        omega.switch <- 1/4
    }
    nu.switch <- as.numeric(input$nu0.switch * input$nu0/100) * 16 ^ (- omega.switch) 
    
    nu.switchA <- (1 - input$f) * nu.switch
    nu.switchB <- input$f * nu.switch
    
    tred1a <- input$tred1a - 1
    tred2a <- input$tred1a + input$npi1a
    tred3a <- tred2a + input$npi2a
    
    tred1b <- input$tred1b - 1
    tred2b <- input$tred1b + input$npi1b
    tred3b <- tred2b + input$npi2b
    
    start.value <- min(tred2a, tred2b, 45)
                           
    parameters <- list(N  =  1,  mu  =  1/(50*52), 
                       tvax = tvax, t.switch = input$t.switch, 
                        alpha = alpha, epsilon = input$epsilon, delta = 1/(52 * input$deltaT),  
                        d = d, rho1 = 1/(52 * input$Rho1), rho2 = 1/(52 * input$Rho2), 
                        epsilonV1  =  input$epsilonV1, epsilonV2 = input$epsilonV2, 
                        epsilon1 = input$epsilon1, epsilon2 = input$epsilon2, 
                        alphaV = alpha, alpha1 = alpha, alpha2 = alpha) 
       
    ## Simulate -------------  
   parametersA <- append(
                parameters, 
                list(nu.init = nu.init, nu.switch = nu.switchA, Rpop = 1,
                     omega.init = omega.init, omega.switch = omega.switch, 
                     R0.list =  R0.list1, gamma = (7/5) * (as.numeric(input$Rtrans)/100),
                     R0red  =  1 - input$eff1a/100, R0red.increase = 1 - input$eff2a/100, 
                     tred1 = tred1a, tred2 = tred2a, tred3 = tred3a) 
                )
    
    parametersB <- append(
        parameters,
        list(nu.init = 0, nu.switch = nu.switchB, Rpop = input$Rpop,
             omega.init = 0, omega.switch = omega.switch, 
             gamma = 7/5, R0.list =  R0.list2,
             R0red  =  1 - input$eff1b/100, R0red.increase = 1 - input$eff2b/100, 
             tred1 = tred1b, tred2 = tred2b, tred3 = tred3b) 
        )
   
    outA = as.data.frame(rk(func = sir_s_.model.doses.switch, y = xstart, times = time, parms = parametersA)) %>%
        mutate(country = "A")
    outB = as.data.frame(rk(func = sir_s_.model.doses.switch, y = xstart, times = time, parms = parametersB)) %>%
        mutate(country = "B")
    
    out <- rbind(outA, outB) %>% filter(time %in% c(start.value:(5*52))) %>% 
        mutate(all.cases = IP + IS + IV + IS1 + IS2, 
               burden = input$primary.burden*IP + input$secondary.burden*IS +
                   input$vaccine.burden*IV + input$one.dose.burden*IS1 + input$two.doses.burden*IS2)

    ## Plot case burden -------------
    scalar = 5
    max.y.cases = max(out$all.cases) + 1e-3

    plot.top = ggplot(data = out, aes(x = time/52)) + 
        geom_line(aes(y = all.cases), color = "red", size = 1)+
        geom_line(aes(y = burden*scalar), linetype = "dashed", size =1) + 
        facet_wrap(~ country, ncol = 2, labeller = labeller(country = countrylabs)) +
        scale_y_continuous(name = "Total cases",  
                           sec.axis = sec_axis(~./scalar, name = "Severe cases"), limits = c(0, max.y.cases),
                           expand = c(0, 0)) +
        scale_x_continuous("Time (years)", expand = c(0, 0)) +
        mytheme +
        theme(panel.grid.major.x = element_line(colour = "grey66", linetype = "dashed"), 
              axis.line.y.left = element_line(color = "red"), axis.ticks.y.left = element_line(color = "red"), 
              axis.title.y.left = element_text(color = "red"), axis.text.y.left = element_text(color = "red"), 
              plot.margin  =  unit(c(0, 0, 0.1, 0), "cm"), panel.spacing = unit(fspace, "lines"))
    
    ## Plot NPI timing -------------
    NPIlength1a <- outA[(outA$time > parametersA["tred1"] & outA$time <= parametersA["tred2"]),]
    NPIlength2a <- outA[(outA$time >= parametersA["tred2"] & outA$time < parametersA["tred3"]),]

    NPIlength1b <- outB[(outB$time > parametersB["tred1"] & outB$time <= parametersB["tred2"]),]
    NPIlength2b <- outB[(outB$time >= parametersB["tred2"] & outB$time < parametersB["tred3"]),]
    
    NPIlength1 <- rbind(NPIlength1a, NPIlength1b)
    NPIlength2 <- rbind(NPIlength2a, NPIlength2b)
    
    plotNPI <- ggplot() + 
        geom_line(data = out, aes(x = time/52, y = 1, colour = "noNPI"), size = 10) +
        geom_line(data = NPIlength1, aes(x = time/52, y = 1, colour = "NPI1"), size = 10) + 
        geom_line(data = NPIlength2, aes(x = time/52, y = 1, colour = "NPI2"), size = 10) + 
        facet_wrap(~ country, ncol = 2) +
        scale_colour_manual("", labels = c("No NPI", "First NPI", "Second NPI"), values = c("grey80", "grey45", "black")) +
        scale_y_continuous("", expand = c(0,0)) + scale_x_continuous("", expand = c(0,0), limits = c(start.value/52, 5)) + 
        theme(axis.title = element_blank(), 
              axis.text = element_blank(), axis.ticks = element_blank(),
              panel.grid = element_blank(), 
              panel.background = element_blank(), plot.background = element_blank(),
              panel.spacing = unit(fspace, "lines"),
              strip.text.x = element_blank(),
              plot.margin = unit(c(0,0,0.1,0), "lines"),
              title = element_blank(), legend.title = element_blank(),
              legend.position = "bottom", legend.direction = "horizontal", 
              legend.text = element_text(size = c(16), margin = margin(r = 0.5, unit = 'cm')))
    
    plotCombined <- plot_grid(plot.top, plotNPI, nrow = 2, rel_heights = c(1, 0.3), align = "v", axis = "rl")
    
    ## Plot immune landscape -------------
    areas <- out %>% gather(var, value, SP:IS2)

    areas$var = factor(areas$var, levels = c("V1", "V2", "SP", "R", "SS", "SS1", 
                                             "SS2", "IV", "IS1", "IS2", "IS", "IP"))
                                                                                         
    plot.below = ggplot(areas, aes(x = time/52, y = value, fill = var)) +
        geom_area() +
        facet_wrap(~ country, ncol = 2, labeller = labeller(country = countrylabs)) +
        scale_y_continuous(name = "Fraction of population") +
        scale_x_continuous(name = "Time (years)", expand = c(0, 0)) +
        scale_fill_manual(values = c(v1.col, v2.col, sp.col, r.col, ss.col, ss1.col, 
                                     ss2.col, iv.col, is1.col, is2.col, is.col, ip.col),
                          guide = FALSE) +
    mytheme + theme(plot.margin = unit(c(0, 2.1, 0.5, 0), "cm"), panel.spacing = unit(fspace, "lines")) 

    ## Plot evolutionary potential -------------
    evodata <- out %>% mutate(S1 = weight.IS*IS + weight.IS1*IS1 + weight.IS2*IS2,
                              S2 = weight2.IS*IS + weight2.IS1*IS1 + weight2.IS2*IS2,
                              S3 = weight3.IS*IS + weight3.IS1*IS1 + weight3.IS2*IS2,
                              S4 = weight4.IS*IS + weight4.IS1*IS1 + weight4.IS2*IS2) %>%
        group_by(time) %>% summarize(S1 = sum(S1), S2 = sum(S2), S3 = sum(S3), S4 = sum(S4)) %>% ungroup() %>% 
        gather(var, value, S1:S4) 

    evodata$var=factor(evodata$var, levels=c("S1", "S2", "S3", "S4"))
    
    plot.evo = ggplot(evodata,aes(x=time/52, y= value, colour= var)) + geom_line(size=1.4) +
        scale_y_continuous(name="Relative net adaption rate") +
        scale_x_continuous(name="Time (years)",expand=c(0,0)) +
        scale_color_manual(name="", values=c("S1" = "black", "S2" = "blue", "S3" = "purple", "S4" = "green4"), 
                           labels = c("Scenario I   ","Scenario II   ","Scenario III   ", "Scenario IV")) +
        mytheme + theme(plot.margin = unit(c(0, 2.5, 0, 0), "cm"), panel.spacing = unit(fspace, "lines"), 
                        legend.position = "top", legend.text = element_text(size = c(16)) )
    
    ## Gather all plots -------------
    plots <- list(cases = plotCombined, landscape = plot.below, evo = plot.evo)      

    return(plots)
}


