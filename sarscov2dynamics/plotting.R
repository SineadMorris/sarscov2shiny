plots.soc.dis.seasonality.vax.hesitancy = function(time, R0.list, input){

    # Plot settings
    sp.col = rgb(0.75,0.75,0.75)
    ip.col = rgb(0.8,0.28,0.23)
    r.col = rgb(0.36,0.33,0.58)
    ss.col = rgb(0.71,0.64,0.83)
    is.col = rgb(1,0.81,0.85)
    v.col = rgb(0.33,0.51,0.21)
    
    basetext = 17
    baseline = 2
    basealpha = 0.8
    
    buffer <- 0.03
    
    mytheme <- theme_bw() + theme(axis.title = element_text(size = basetext), 
                                  axis.text = element_text(size = basetext - 1),
                                  legend.title = element_text(size = basetext), 
                                  legend.text = element_text(size = basetext - 1),
                                  title = element_text(size = basetext - 3))
    
    # Get parameters & simulate model
    I0 <- 1e-9
    
    severeP <- 0.14
    
    if(input$tabs == "refusal"){
        
        parameters <- list(N =  1, mu = 1/(50*52), gamma = 7/5, 
                            alpha = input$alpha, epsilon = input$epsilon, delta = 1/(52 * input$deltaT), 
                            nu = input$nu / 100, delta_vax = 1/(52 * input$deltaV), tvax = input$tV * 52,
                            sevP = severeP, sevS = input$rel_severe * severeP,
                            R0.list = R0.list, R0red = 1 - input$efficacy/100, 
                            tred1 = as.numeric(input$npi[1]) - 1, tred2 = as.numeric(input$npi[2]) + 1, tred3 = 0, tred4 = 0,
                            N1 = 1 - input$N2, N2 = input$N2, 
                            a11 = 1, a22 = input$a22, a12 = (input$a22 + 1) / 2, a21 = (input$a22 + 1) / 2)
        
        N2 <- input$N2
        N1 <- 1 - N2
        
        xstart <- c(SP1 = N1 - N1*I0, IP1 = N1*I0, R1 = 0, SS1 = 0, IS1 = 0, V1 = 0, 
                    SP2 = N2 - N2*I0, IP2 = N2*I0, R2 = 0, SS2 = 0, IS2 = 0) 
        
        out <- as.data.frame(rk(func = hesitancy, y = xstart, times = time, parms = parameters)) %>% 
            mutate(IP = IP1 + IP2, IS = IS1 + IS2, R = R1 + R2, SS = SS1 + SS2, SP = SP1 + SP2, V = V1)
        
        timecourses <- out %>% gather(immunity, value, IP:V)
    } else if (input$tabs == "clinical") {
        
        parameters <- list(N =  1, mu = 1/(50*52), gamma = 7/5, 
                            alpha = input$alpha, 
                            nu = input$nu / 100, delta_vax = 1/(52 * input$deltaV), tvax = input$tV * 52,
                            sevP = severeP, sevS = input$rel_severe * severeP,
                            R0.list = R0.list, R0red = 1 - input$efficacy/100, 
                            tred1 = as.numeric(input$npi[1]) - 1, tred2 = as.numeric(input$npi[2]) + 1, tred3 = 0, tred4 = 0,
                            sigmaP = input$sigmaP, sigmaS = input$sigmaS,
                            delta1 =  1/(52 * input$deltaT), delta2 =  input$delta_rel * ( 1/(52 * input$deltaT) ), 
                            epsilon1 = input$epsilon, epsilon2 = 1)
 
        xstart = c(SP = (1 - I0), IP1 = I0*(1 - severeP), IP2 = I0*severeP, R1 = 0, R2 = 0, SS1 = 0, SS2 = 0, IS1 = 0, IS2 = 0, V = 0) 
        
        out <- as.data.frame(rk(func = heterogeneity, y = xstart, times = time, parms = parameters))%>% 
            mutate(IP = IP1 + IP2, IS = IS1 + IS2, R = R1 + R2, SS = SS1 + SS2)
        
        timecourses <- out %>% gather(immunity, value, SP, V, IP:SS)
    } else{
        
        parameters <- list(N =  1, mu = 1/(50*52), gamma = 7/5, 
                            alpha = input$alpha, epsilon = input$epsilon, delta = 1/(52 * input$deltaT), 
                            nu = input$nu / 100, delta_vax = 1/(52 * input$deltaV), tvax = input$tV * 52,
                            sevP = severeP, sevS = input$rel_severe * severeP,
                            R0.list = R0.list, R0red = 1 - input$efficacy/100, 
                            tred1 = as.numeric(input$npi[1]) - 1, tred2 = as.numeric(input$npi[2]) + 1, tred3 = 0, tred4 = 0)
        
        xstart <- c(SP = 1-I0, IP = I0, R = 0, SS = 0, IS = 0, V = 0) 
        
        out <- as.data.frame(rk(func = original, y = xstart, times = time, parms = parameters))
    
        timecourses <- out %>% gather(immunity, value, SP:V)
    }
    
    plotCombined <- timecourses %>% filter(immunity %in% c("IP", "IS")) %>% 
        ggplot(data = ., aes(x = time/52,y = value, colour = immunity)) + 
        geom_line(size = baseline, alpha = basealpha) +
        scale_colour_manual(name = "", values = c(ip.col, is.col), labels = c("primary infection", "secondary infection")) +
        scale_y_continuous("Fraction of population", expand = c(buffer, 0)) + scale_x_continuous("Time (years)", expand = c(0,0)) + 
        mytheme + theme(plot.margin = unit(c(1,1,2,1), "lines"))
    
    
    NPIlength <- out[out$time > parameters["tred1"] & out$time < parameters["tred2"],]
    
    plotNPI <- ggplot() + geom_line(data = out, aes(x = time/52, y = 1, colour = "noNPI"), size = 10) +
        geom_line(data = NPIlength, aes(x = time/52, y = 1, colour = "NPI"), size = 10) + 
        scale_colour_manual("", labels = c("No NPI", "NPI"), values = c("grey", "black")) +
        scale_y_continuous("", expand = c(0,0)) + scale_x_continuous("", expand = c(0,0)) + 
        theme_bw() + theme(axis.title = element_blank(), 
                           axis.text = element_blank(), axis.ticks = element_blank(),
                           panel.grid = element_blank(), panel.border = element_rect(colour = "white"),
                           title = element_blank(),
                           legend.position = "right", legend.direction = "horizontal", 
                           legend.text = element_text(size = c(basetext - 2), margin = margin(r = 0.5, unit = 'cm')))
    
    plotCombined <- plot_grid(plotCombined, plotNPI, nrow = 2, rel_heights = c(1, 0.1), align = "v", axis = "rl")
    
    
    timecourses$immunity = factor(timecourses$immunity, levels = c("V","SP","R", "SS","IS","IP"))
    
    plotSusc  <- timecourses %>% filter(immunity %in% c("V", "SP", "R", "SS")) %>% 
        ggplot(data = ., aes(x = time/52, y = value, colour = immunity)) + 
        geom_line(size = baseline, alpha = basealpha) +
        scale_colour_manual(name = "", values = c(v.col, sp.col, r.col, ss.col), 
                            labels = c("vaccine immunity", "full susceptibility","natural immunity", "partial immunity")) +
        scale_y_continuous("Fraction of population", expand = c(buffer, 0)) + scale_x_continuous("Time (years)", expand = c(0,0)) + 
        mytheme + theme(plot.margin = unit(c(1,1,2,1), "lines"))
    
    plotSusc <- plot_grid(plotSusc, plotNPI, nrow = 2, rel_heights = c(1, 0.1), align = "v", axis = "rl")
    
    plotSevere <- out %>% mutate(severe = as.numeric(parameters["sevP"]) * IP + as.numeric(parameters["sevS"]) * IS) %>% 
        ggplot(data = ., aes(x = time/52, y = severe)) + 
        geom_line(size = baseline - 0.5, alpha = basealpha) +
        scale_y_continuous("Severe cases", expand = c(buffer, 0)) + scale_x_continuous("", expand = c(0,0)) + 
        mytheme + theme(axis.text.x = element_blank(), plot.margin = unit(c(1,1,1.75,1), "lines"))
    
    plot.area.middle <- ggplot(data = timecourses, aes(x = time/52,y = value, fill = immunity)) + geom_area() +
        scale_y_continuous(name = "Fraction of population", expand = c(0,0)) +
        scale_x_continuous(name = "Time (years)", expand = c(0,0)) +
        scale_fill_manual(name = "", values = c(v.col,sp.col,r.col,ss.col,is.col,ip.col), 
                          labels = c("vaccine immunity","full susceptibility","natural immunity",
                                     "partial immunity","secondary infection","primary infection")) + mytheme
    
    plotArea <- plot_grid(plotSevere, plot.area.middle, nrow = 2, rel_heights = c(0.5, 1), align = "v", axis = "rl")
    
    plots <- list(plotCombined, plotSusc, plotArea)

    return(plots)
}

