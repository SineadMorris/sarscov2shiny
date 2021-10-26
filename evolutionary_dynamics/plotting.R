## Plotting dose regime,  case burden,  and landscape function -----------------------------------

plots.doses.both <- function(time, R0.list, input, alpha = 1, 
                             start.value = 45, d = 0.5,
                             weight.IS = 0.05, weight.IS1 = 0.3, weight.IS2 = 0.05,
                             weight2.IS = 0.05, weight2.IS1 = 1, weight2.IS2 = 0.05,
                             weight3.IS = 0.8, weight3.IS1 = 1, weight3.IS2 = 0.8
){
    valuesIP = list()
    valuesIS = list()
    valuesR = list()
    valuesSP = list()
    valuesSS = list()
    valuesV1 = list()
    valuesV2 = list()
    valuesIV = list()
    valuesSS1 = list()
    valuesSS2 = list()
    valuesIS1 = list()
    valuesIS2 = list()
    
    I0 = 1e-9
    
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
    
    mytheme <- theme_classic((size = 18)) + theme(legend.position = "none")
    
    # ICs
    if (input$tabs  ==  "gamma") {
        xstart = c(SP = 1-I0,  IP = I0,  R = 0,  SS = 0,  IS = 0,  V1 = 0,  V2 = 0,  IV = 0,  SS1 = 0,  SS2 = 0,  IS1 = 0,  IS2 = 0,  V1a = 0)
    } else {
        xstart = c(SP = 1-I0,  IP = I0,  R = 0,  SS = 0,  IS = 0,  V1 = 0,  V2 = 0,  IV = 0,  SS1 = 0,  SS2 = 0,  IS1 = 0,  IS2 = 0) 
    }
    
    tvax <- as.numeric(input$tV)
    
    if (input$doses == "one"){
        omega <- omega.init <- 0
    } else{
        omega <- omega.init <- 1/as.numeric(input$L)
    }
    
    nu <- as.numeric(input$nu0/100) * 16 ^ (- omega) 
    nu.init <- as.numeric(input$nu0/100) * 16 ^ (- omega.init) 
    
    nu.switch <- nu.init
    omega.switch <- omega.init
    
    if (input$switch == "yes2"){
        omega.switch <- 1/4
        #nu.switch <- as.numeric(input$nu0/100) * 16 ^ (- omega.switch)
        nu.switch <- as.numeric(input$nu0/100) * 16 ^ (- 0)
    } else if (input$switch == "yes1") {
        omega.switch <- omega.init
        nu.switch <- as.numeric(input$nu0.switch * input$nu0/100) * 16 ^ (- omega.switch)
    }
    
    tred1 <- input$tred1 - 1
    tred2 <- input$tred1 + input$npi1
    tred3 <- tred2 + input$npi2
    
    # Parameters
    parameters <- list(N  =  1,  mu  =  1/(50*52),  gamma  =  7/5, 
                       alpha  =  alpha,  epsilon  =  input$epsilon,  delta  =  1/(52 * input$deltaT),  
                       R0.list  =  R0.list,  
                       R0red  =  1 - input$eff1/100,  # efficacy in previous model
                       R0red.increase  =  1 - input$eff2/100, 
                       tred1 = tred1, tred2 = tred2, tred3 = tred3, 
                       tvax  =  tvax,  nu  =  nu,  
                       rho1  =  1/(52 * input$Rho1),  rho2  =  1/(50 * input$Rho2), 
                       epsilonV1  =  input$epsilonV1,  epsilonV2  =  input$epsilonV2, 
                       epsilon1  =  input$epsilon1,  epsilon2  =  input$epsilon2, 
                       alphaV  =  alpha,  alpha1  =  alpha,  alpha2  =  alpha, 
                       omega  =  omega,  d  =  d)
    
    if (input$tabs  ==  "gamma") {
        out = as.data.frame(rk(func = sir_s_.model.doses.gamma, y = xstart, times = time, parms = parameters))
        
        valuesIP = out$IP[start.value:(5*52)]
        valuesIS = out$IS[start.value:(5*52)]
        valuesR = out$R[start.value:(5*52)]
        valuesSS = out$SS[start.value:(5*52)]
        valuesSP = out$SP[start.value:(5*52)]
        valuesV1 = out$V1[start.value:(5*52)] + out$V1a[start.value:(5*52)]
        valuesV2 = out$V2[start.value:(5*52)]
        valuesSS1 = out$SS1[start.value:(5*52)]
        valuesSS2 = out$SS2[start.value:(5*52)]
        valuesIS1 = out$IS1[start.value:(5*52)]
        valuesIS2 = out$IS2[start.value:(5*52)]
        valuesIV = out$IV[start.value:(5*52)]
        
    } else {
        if (input$switch != "no") {
            
            parameters <- list(N  =  1,  mu  =  1/(50*52),  gamma  =  7/5, 
                               alpha  =  alpha,  epsilon  =  input$epsilon,  delta  =  1/(52 * input$deltaT),  
                               R0.list  =  R0.list,  
                               R0red  =  1 - input$eff1/100,  # efficacy in previous model
                               R0red.increase  =  1 - input$eff2/100, 
                               tred1 = tred1, tred2 = tred2, tred3 = tred3, 
                               tvax  =  tvax,  nu.init  =  nu.init,  nu.switch = nu.switch,
                               rho1  =  1/(52 * input$Rho1),  rho2  =  1/(50 * input$Rho2), 
                               epsilonV1  =  input$epsilonV1,  epsilonV2  =  input$epsilonV2, 
                               epsilon1  =  input$epsilon1,  epsilon2  =  input$epsilon2, 
                               alphaV  =  alpha,  alpha1  =  alpha,  alpha2  =  alpha, 
                               omega.init  =  omega.init, omega.switch = omega.switch, 
                               t.switch = input$t.switch, d  =  d)
            
            out = as.data.frame(rk(func = sir_s_.model.doses.switch, y = xstart, times = time, parms = parameters))
        } else {
            out = as.data.frame(rk(func = sir_s_.model.doses, y = xstart, times = time, parms = parameters))
        }
        valuesIP = out$IP[start.value:(5*52)]
        valuesIS = out$IS[start.value:(5*52)]
        valuesR = out$R[start.value:(5*52)]
        valuesSS = out$SS[start.value:(5*52)]
        valuesSP = out$SP[start.value:(5*52)]
        valuesV1 = out$V1[start.value:(5*52)]
        valuesV2 = out$V2[start.value:(5*52)]
        valuesSS1 = out$SS1[start.value:(5*52)]
        valuesSS2 = out$SS2[start.value:(5*52)]
        valuesIS1 = out$IS1[start.value:(5*52)]
        valuesIS2 = out$IS2[start.value:(5*52)]
        valuesIV = out$IV[start.value:(5*52)]
    }
    
    forAreaPlotIP = valuesIP
    forAreaPlotIS = valuesIS
    forAreaPlotR = valuesR
    forAreaPlotSS = valuesSS
    forAreaPlotSP = valuesSP
    forAreaPlotV1 = valuesV1
    forAreaPlotV2 = valuesV2
    forAreaPlotIV = valuesIV
    forAreaPlotSS1 = valuesSS1
    forAreaPlotSS2 = valuesSS2
    forAreaPlotIS1 = valuesIS1
    forAreaPlotIS2 = valuesIS2
    scalar = 5
    all.cases = forAreaPlotIP+forAreaPlotIS+forAreaPlotIV+forAreaPlotIS1+forAreaPlotIS2
    
    burden = input$primary.burden*forAreaPlotIP + input$secondary.burden*forAreaPlotIS + 
        input$vaccine.burden*forAreaPlotIV + input$one.dose.burden*forAreaPlotIS1 + input$two.doses.burden*forAreaPlotIS2
    
    data.burden = data.frame(xx = time[start.value:(5*52)], yy = burden, zz = all.cases)
    
    max.y.cases = max(all.cases) + 1e-3
    
    # Plot of case burden
    plot.top = ggplot(data = data.burden, aes(x = xx/52))+geom_line(aes(y = zz), color = "red")+
        geom_line(aes(y = yy*scalar), linetype = "dashed")+
        scale_y_continuous(name = "Total cases",  
                           sec.axis = sec_axis(~./scalar, name = "Severe cases"), limits = c(0, max.y.cases),
                           expand = c(0, 0))+
        scale_x_continuous("Time (years)", expand = c(0, 0))+
        mytheme +
        theme(panel.grid.major.x = element_line(colour = "grey66", linetype = "dashed"), 
              axis.line.y.left = element_line(color = "red"), axis.ticks.y.left = element_line(color = "red"), 
              axis.title.y.left = element_text(color = "red"), axis.text.y.left = element_text(color = "red"), 
              plot.margin  =  unit(c(0, 0, 0.1, 0), "cm"))
    
    #out0 <- out[start.value:(5*52),]
    NPIlength1 <- out[(out$time > parameters["tred1"] & out$time <= parameters["tred2"]),]
    NPIlength2 <- out[(out$time >= parameters["tred2"] & out$time < parameters["tred3"]),]
    
    plotNPI <- ggplot() + 
        geom_line(data = out, aes(x = time/52, y = 1, colour = "noNPI"), size = 10) +
        geom_line(data = NPIlength1, aes(x = time/52, y = 1, colour = "NPI1"), size = 10) + 
        geom_line(data = NPIlength2, aes(x = time/52, y = 1, colour = "NPI2"), size = 10) + 
        scale_colour_manual("", labels = c("No NPI", "First NPI", "Second NPI"), values = c("grey80", "grey45", "black")) +
        scale_y_continuous("", expand = c(0,0)) + scale_x_continuous("", expand = c(0,0), limits = c(start.value/52, 5)) + 
        theme(axis.title = element_blank(), 
              axis.text = element_blank(), axis.ticks = element_blank(),
              panel.grid = element_blank(), 
              panel.background = element_blank(), plot.background = element_blank(),
              panel.spacing = unit(c(0,0,0,0), "cm"),
              plot.margin = unit(c(0,0,0.1,0), "lines"),
              title = element_blank(), legend.title = element_blank(),
              legend.position = "bottom", legend.direction = "horizontal", 
              legend.text = element_text(size = c(16), margin = margin(r = 0.5, unit = 'cm')))
    
    plotCombined <- plot_grid(plot.top, plotNPI, nrow = 2, rel_heights = c(1, 0.3), align = "v", axis = "rl")
    
    # Plot immune landscape
    data.AP.IP = data.frame(xx = time[start.value:(5*52)], yy = forAreaPlotIP)
    data.AP.IS = data.frame(xx = time[start.value:(5*52)], yy = forAreaPlotIS)
    data.AP.R = data.frame(xx = time[start.value:(5*52)], yy = forAreaPlotR)
    data.AP.SS = data.frame(xx = time[start.value:(5*52)], yy = forAreaPlotSS)
    data.AP.SP = data.frame(xx = time[start.value:(5*52)], yy = forAreaPlotSP)
    data.AP.V1 = data.frame(xx = time[start.value:(5*52)], yy = forAreaPlotV1)
    data.AP.V2 = data.frame(xx = time[start.value:(5*52)], yy = forAreaPlotV2)
    data.AP.IV = data.frame(xx = time[start.value:(5*52)], yy = forAreaPlotIV)
    data.AP.SS1 = data.frame(xx = time[start.value:(5*52)], yy = forAreaPlotSS1)
    data.AP.SS2 = data.frame(xx = time[start.value:(5*52)], yy = forAreaPlotSS2)
    data.AP.IS1 = data.frame(xx = time[start.value:(5*52)], yy = forAreaPlotIS1)
    data.AP.IS2 = data.frame(xx = time[start.value:(5*52)], yy = forAreaPlotIS2)
    
    data.AP.list = list("primary infection" = data.AP.IP, "secondary infection" = data.AP.IS, 
                        "fully immune" = data.AP.R, "partially immune" = data.AP.SS, 
                        "fully susceptible" = data.AP.SP, 
                        "one dose" = data.AP.V1,  "two doses" = data.AP.V2, 
                        "one dose,  waned" = data.AP.SS1,  "two doses,  waned" = data.AP.SS2, 
                        "infection after vaccine" = data.AP.IV, "infection after one dose waning" = data.AP.IS1, 
                        "infection after two doses waning" = data.AP.IS2)
    
    area.plots.timecourses = bind_rows(data.AP.list, .id = c("immunity"))
    area.plots.timecourses$immunity = factor(area.plots.timecourses$immunity, levels = c("one dose", "two doses", "fully susceptible", "fully immune", 
                                                                                         "partially immune", "one dose,  waned", "two doses,  waned", 
                                                                                         "infection after vaccine", "infection after one dose waning", 
                                                                                         "infection after two doses waning", "secondary infection", 
                                                                                         "primary infection"))
    plot.below = ggplot(area.plots.timecourses, aes(x = xx/52, y = yy, fill = immunity)) +
        geom_area()+
        scale_y_continuous(name = "Fraction of population")+
        scale_x_continuous(name = "Time (years)", expand = c(0, 0))+
        scale_fill_manual(#name = "", 
                          values = c(v1.col, v2.col, sp.col, r.col, ss.col, ss1.col, ss2.col, iv.col, is1.col, is2.col, is.col, ip.col),
                          #labels = c("1-dose vaccinal immunity", "2-dose vaccinal immunity", "Full susceptibility",
                          #           "Natural immunity", "Waned natural immunity", "Waned 1-dose immunity",
                          #          "Waned 2-dose immunity", "Infection after vaccination", "Infection after waned 1-dose immunity",
                          #           "Infection after waned 2-dose immunity", "Secondary infection", "Primary infection")
                          guide = FALSE)+
        mytheme + theme(plot.margin = unit(c(0, 2.1, 0.5, 0), "cm")) 
    
    # 
    # 
    # if (input$tabs != "gamma" & input$tabs !=  "no vaccine") {
    #     vaccination.times = time[(start.value:(5*52))] - tvax
    #     vaccination.times[vaccination.times<0] = 0
    #     vaccine.one.dose.rate = 1-exp(-nu*vaccination.times)
    #     vaccine.two.doses.rate = 1-((omega/(omega-nu))*exp(-nu*vaccination.times))+((nu/(omega-nu))*exp(-omega*vaccination.times))
    #     vaccine.two.doses.rate[((start.value:tvax)-start.value+1)] = 0
        
        # if (input$switch != "no"){
        #     vaccination.times1 = time[start.value:input$t.switch] - tvax
        #     vaccination.times2 = time[(input$t.switch + 1):(5*52)] - tvax
        #     vaccination.times1[vaccination.times1<0] = 0
        #     vaccination.times2[vaccination.times2<0] = 0
        #     
        #     vaccination.times <- c(vaccination.times1, vaccination.times2)
        #     
        #     vaccine.one.dose.rate1 = 1-exp(-nu.init*vaccination.times1)
        #     vaccine.one.dose.rate2 = 1-exp(-nu.switch*vaccination.times2)
        #     
        #     vaccine.one.dose.rate <- c(vaccine.one.dose.rate1, vaccine.one.dose.rate2)
        #     
        #     vaccine.two.doses.rate1 = 1-((omega.init/(omega.init-nu.init))*exp(-nu.init*vaccination.times1))+((nu.init/(omega.init-nu.init))*exp(-omega.init*vaccination.times1))
        #     vaccine.two.doses.rate1[((start.value:tvax)-start.value+1)] = 0
        #     
        #     vaccine.two.doses.rate2 = 1-((omega.switch/(omega.switch-nu.switch))*exp(-nu.switch*vaccination.times2))+((nu.switch/(omega.switch-nu.switch))*exp(-omega.switch*vaccination.times2))
        #     #vaccine.two.doses.rate2[((start.value:tvax)-start.value+1)] = 0
        #     vaccine.two.doses.rate <- c(vaccine.two.doses.rate1, vaccine.two.doses.rate2)
        # }
        
        
        # data.vaccinated = data.frame(xx = c(time[start.value:(5*52)], time[start.value:(5*52)]), 
        #                              yy = c(vaccine.one.dose.rate, vaccine.two.doses.rate), 
        #                              zz = c(rep("1", length(vaccine.one.dose.rate)), rep("2", length(vaccine.two.doses.rate))))
        # 
        
        ## Plot vaccination progress
        # plot.first = ggplot(data = data.vaccinated)+geom_line(aes(x = xx/52, y = yy, color = zz), size = 1)+
        #     scale_color_manual(name = "", values = c(v1.col, v2.col))+
        #     xlab("Time (years)")+scale_y_continuous(name = "Proportion vaccinated", limits = c(0, 1))+
        #     scale_x_continuous(expand = c(0, 0))+
        #     mytheme + theme(plot.margin = unit(c(0, 2.5, 0, 0), "cm"))
        
        
        ## For evolutionary plots
        data.IS.weighted.sum = data.frame(xx=time[start.value:(5*52)],
                                          yy=(weight.IS*forAreaPlotIS + weight.IS1 * forAreaPlotIS1 + weight.IS2*forAreaPlotIS2))
        
        data.IS.weighted2.sum = data.frame(xx=time[start.value:(5*52)],
                                           yy=(weight2.IS*forAreaPlotIS + weight2.IS1*forAreaPlotIS1 + weight2.IS2*forAreaPlotIS2))
        
        data.IS.weighted3.sum = data.frame(xx=time[start.value:(5*52)],
                                           yy=(weight3.IS*forAreaPlotIS + weight3.IS1*forAreaPlotIS1 + weight3.IS2*forAreaPlotIS2))
        
        data.IS.weights.list=list("Scenario I   "=data.IS.weighted.sum,
                                  "Scenario II   "=data.IS.weighted2.sum,
                                  "Scenario III   "=data.IS.weighted3.sum)
        
        IS.weights.timecourses=bind_rows(data.IS.weights.list,.id=c("immunity"))
        
        IS.weights.timecourses$immunity=factor(IS.weights.timecourses$immunity,
                                               levels=c("Scenario I   ","Scenario II   ","Scenario III   "))
        
        plot.evo = ggplot(IS.weights.timecourses,aes(x=xx/52,y=yy,colour=immunity))+geom_line(size=1.2)+
            scale_y_continuous(name="Relative net adaption rate")+
            scale_x_continuous(name="Time (years)",expand=c(0,0))+
            scale_color_manual(name="",values=c("Scenario I   "="black","Scenario II   "="blue",
                                                "Scenario III   "="purple"))+
            mytheme + theme(plot.margin = unit(c(0, 2.5, 0, 0), "cm"), 
                            legend.position = "top", legend.text = element_text(size = c(16)) ) #, legend.spacing.x = unit(0.75, 'cm'))
        
        plots <- list(cases = plotCombined, landscape = plot.below, evo = plot.evo)      
        
  #  } else {
   #     plots <- list(cases = plotCombined, landscape = plot.below, evo = plot.evo)      
   # }
    
    return(plots)
}


