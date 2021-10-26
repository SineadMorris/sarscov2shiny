## No Vaccine Refusal ODE equations -----------------------------------

sir_s_.model.doses <- function(t,y,parms){
    
    with(as.list(c(y, parms)),{
        
        beta = R0.list[t] * gamma
        
        if((t > tred1) & (t < tred2)){
            beta = beta * R0red
        }
        if((t >= tred2) & (t < tred3)){
            beta = beta * R0red.increase 
        }
        svax=0
        if(t>tvax){
            svax=1
        }
        
        #Equations
        dSPdt=mu-(beta*SP*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2))-mu*SP-svax*nu*SP
        dIPdt=(beta*SP*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2))-(gamma+mu)*IP
        dRdt=gamma*(IP+IS+IV+IS1+IS2)-(delta+mu)*R
        dSSdt=delta*R-epsilon*(beta*SS*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2))-mu*SS-svax*nu*SS
        dISdt=epsilon*(beta*SS*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2))-(gamma+mu)*IS
        dV1dt=svax*nu*(SP+d*SS)-epsilonV1*beta*V1*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-(omega+rho1+mu)*V1
        dV2dt=(1-d)*svax*nu*SS+omega*V1-epsilonV2*beta*V2*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-(rho2+mu)*V2
        dIVdt=(epsilonV1*beta*V1+epsilonV2*beta*V2)*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-(gamma+mu)*IV
        dSS1dt=rho1*V1-epsilon1*beta*SS1*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-mu*SS1
        dSS2dt=rho2*V2-epsilon2*beta*SS2*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-mu*SS2
        dIS1dt=epsilon1*beta*SS1*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-(gamma+mu)*IS1
        dIS2dt=epsilon2*beta*SS2*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-(gamma+mu)*IS2
        dxdt <- c(dSPdt,dIPdt,dRdt,dSSdt,dISdt,dV1dt,dV2dt,dIVdt,dSS1dt,dSS2dt,dIS1dt,dIS2dt)
        return(list(dxdt))
    })
}


## Change in dosing strategy ODE equations -----------------------------------

sir_s_.model.doses.switch<-function (t, y, parms) {
    
    with(as.list(c(y, parms)),{
    
        beta = R0.list[t] * gamma
        
        if((t > tred1) & (t < tred2)){
            beta = beta * R0red
        }
        if((t >= tred2) & (t < tred3)){
            beta = beta * R0red.increase 
        }
        
        svax=0
        if(t>tvax){
            svax=1
        }
        if(t>t.switch){
            omega=omega.switch
            nu=nu.switch
        } else {
            omega=omega.init
            nu=nu.init
        }
        #Equations
        dSPdt=mu-(beta*SP*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2))-mu*SP-svax*nu*SP
        dIPdt=(beta*SP*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2))-(gamma+mu)*IP
        dRdt=gamma*(IP+IS+IV+IS1+IS2)-(delta+mu)*R
        dSSdt=delta*R-epsilon*(beta*SS*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2))-mu*SS-svax*nu*SS
        dISdt=epsilon*(beta*SS*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2))-(gamma+mu)*IS
        dV1dt=svax*nu*(SP+d*SS)-epsilonV1*beta*V1*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-(omega+rho1+mu)*V1
        dV2dt=(1-d)*svax*nu*SS+omega*V1-epsilonV2*beta*V2*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-(rho2+mu)*V2
        dIVdt=(epsilonV1*beta*V1+epsilonV2*beta*V2)*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-(gamma+mu)*IV
        dSS1dt=rho1*V1-epsilon1*beta*SS1*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-mu*SS1
        dSS2dt=rho2*V2-epsilon2*beta*SS2*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-mu*SS2
        dIS1dt=epsilon1*beta*SS1*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-(gamma+mu)*IS1
        dIS2dt=epsilon2*beta*SS2*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-(gamma+mu)*IS2
        dxdt <- c(dSPdt,dIPdt,dRdt,dSSdt,dISdt,dV1dt,dV2dt,dIVdt,dSS1dt,dSS2dt,dIS1dt,dIS2dt)
    return(list(dxdt))
    })
}


## Vaccine Refusal ODE equations -----------------------------------

sir_s_.model.doses.gamma<-function (t,y, parms) {
    
    with(as.list(c(y, parms)),{
        
        beta = R0.list[t] * gamma
        
        if((t > tred1) & (t < tred2)){
            beta = beta * R0red
        }
        if((t >= tred2) & (t < tred3)){
            beta = beta * R0red
        }
        if((t >= tred3) & (t < tred4)){
            beta = beta * R0red * R0red.increase
        } 
        svax=0
        if(t>tvax){
            svax=1
        }
        
        #Equations
        dSPdt=mu-(beta*SP*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2))-mu*SP-svax*nu*SP
        dIPdt=(beta*SP*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2))-(gamma+mu)*IP
        dRdt=gamma*(IP+IS+IV+IS1+IS2)-(delta+mu)*R
        dSSdt=delta*R-epsilon*(beta*SS*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2))-mu*SS-svax*nu*SS
        dISdt=epsilon*(beta*SS*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2))-(gamma+mu)*IS
        dV1dt=svax*nu*(SP+d*SS)-epsilonV1*beta*V1*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-(2*omega+rho1+mu)*V1
        dV1adt=2*omega*V1-epsilonV1*beta*V1a*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-(2*omega+rho1+mu)*V1a
        dV2dt=(1-d)*svax*nu*SS+2*omega*V1a-epsilonV2*beta*V2*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-(rho2+mu)*V2
        dIVdt=(epsilonV1*beta*(V1+V1a)+epsilonV2*beta*V2)*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-(gamma+mu)*IV
        dSS1dt=rho1*(V1+V1a)-epsilon1*beta*SS1*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-mu*SS1
        dSS2dt=rho2*V2-epsilon2*beta*SS2*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-mu*SS2
        dIS1dt=epsilon1*beta*SS1*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-(gamma+mu)*IS1
        dIS2dt=epsilon2*beta*SS2*(IP+alpha*IS+alphaV*IV+alpha1*IS1+alpha2*IS2)-(gamma+mu)*IS2
        dxdt <- c(dSPdt,dIPdt,dRdt,dSSdt,dISdt,dV1dt,dV2dt,dIVdt,dSS1dt,dSS2dt,dIS1dt,dIS2dt,dV1adt)
        return(list(dxdt))
    })
}

