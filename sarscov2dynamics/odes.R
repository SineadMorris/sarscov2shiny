## Model with seasonality, social distancing, and vaccination  ----------------------------------

original <- function (t, y, parms) {

    with(as.list(c(y, parms)),{
    
        beta = R0.list[t] * gamma
        
        if ((t > tred1) & (t < tred2)) {
            beta = beta * R0red
        }
        
        svax = 0
        if (t > tvax) {
            svax = 1
        }
        
        dSP = mu*N - (beta * SP * (IP + alpha * IS)/N) - mu * SP - svax * nu * SP
        dIP = (beta * SP * (IP + alpha * IS)/N) - (gamma + mu) * IP
        dR = gamma * (IP + IS) - (delta + mu) * R
        dSS = delta * R - epsilon * (beta * SS * (IP + alpha * IS)/N) - mu * SS + delta_vax * V - svax * nu * SS
        dIS = epsilon * (beta * SS * (IP + alpha * IS)/N) - (gamma + mu) * IS
        dV = svax * nu * (SP + SS) - delta_vax*V - mu * V
        
        return(list(c(dSP, dIP, dR, dSS, dIS, dV)) )
    })
}


## 2nd Sep addition: adding vaccine refusal ----------------------------------

hesitancy <- function(t, y, parms){
    
    with(as.list(c(y, parms)),{
    
    beta = R0.list[t] * gamma
        
    if ((t > tred1) & (t < tred2)) {
        beta = beta * R0red
    }
    
    svax = 0
    if (t > tvax) {
        svax = 1
    }
    
    dSP1 = mu*N1 - (beta*SP1 * (a11 * (IP1 + alpha*IS1) + a12*(IP2 + alpha*IS2))) - mu*SP1 - svax*nu*SP1
    dIP1 = (beta*SP1 * (a11 * (IP1 + alpha*IS1) + a12 * (IP2 + alpha*IS2))) - (gamma + mu) * IP1
    dR1 = gamma * (IP1 + IS1) - (delta + mu) * R1
    dSS1 = delta * R1 - epsilon * (beta*SS1 * (a11*(IP1 + alpha*IS1) + a12*(IP2 + alpha*IS2))) - mu*SS1 + delta_vax*V1 - svax*nu*SS1
    dIS1 = epsilon * (beta*SS1 * (a11*(IP1 + alpha*IS1) + a12*(IP2 + alpha*IS2))) - (gamma + mu)*IS1
    dV1 = svax*nu * (SP1 + SS1) - delta_vax * V1 - mu * V1
    
    dSP2 = mu*N2 - (beta*SP2 * (a22*(IP2 + alpha*IS2) + a21*(IP1 + alpha*IS1))) - mu*SP2
    dIP2 = (beta*SP2 * (a22 * (IP2 + alpha*IS2) + a21 * (IP1 + alpha*IS1))) - (gamma + mu)*IP2
    dR2 = gamma * (IP2 + IS2) - (delta + mu) * R2
    dSS2 = delta*R2 - epsilon * (beta*SS2 * (a22*(IP2 + alpha*IS2) + a21*(IP1 + alpha*IS1))) - mu*SS2
    dIS2 = epsilon * (beta*SS2 * (a22*(IP2 + alpha*IS2) + a21*(IP1 + alpha*IS1))) - (gamma + mu)*IS2
    
    return(list(c(dSP1,dIP1,dR1,dSS1,dIS1,dV1,dSP2,dIP2,dR2,dSS2,dIS2)) )
    })
}

## 9th Sep addition: Model with seasonality, social distancing, vaccination, and heterogeneity  ----------------------------------

heterogeneity <- function(t, y, parms){
    
    with(as.list(c(y, parms)),{
        
    beta = R0.list[t] * gamma
    
    if ((t > tred1) & (t < tred2)) {
        beta = beta * R0red
    }
    
    svax = 0
    if (t > tvax) {
        svax = 1
    }
    
    #Equations
    dSP = mu*N - (beta*SP*(IP1 + alpha*IS1 + sigmaP*IP2 + sigmaS*alpha*IS2)/N) - mu*SP - svax*nu*SP
    dIP1 = (beta*SP*(1 - sevP)*(IP1 + alpha*IS1 + sigmaP*IP2 + sigmaS*alpha*IS2)/N) - (gamma + mu)*IP1
    dIP2 = (beta*SP*sevP*(IP1 + alpha*IS1 + sigmaP*IP2 + sigmaS*alpha*IS2)/N) - (gamma + mu)*IP2
    dR1 = (1 - sevS)*gamma*IP1 + gamma*IS1 + gamma*(IP2 + IS2) - (delta1 + mu)*R1
    dR2 = sevS*gamma*IP1 - (delta2 + mu)*R2
    dSS1 = delta1*R1 + delta_vax*V - mu*SS1 - svax*nu*SS1 - epsilon1*(beta*SS1*(IP1 + alpha*IS1 + sigmaP*IP2 + sigmaS*alpha*IS2)/N)
    dSS2 = delta2*R2 - mu*SS2 - svax*nu*SS2 - epsilon2*(beta*SS2*(IP1 + alpha*IS1 + sigmaP*IP2 + sigmaS*alpha*IS2)/N)
    dIS1 = epsilon1*(beta*SS1*(IP1 + alpha*IS1 + sigmaP*IP2 + sigmaS*alpha*IS2)/N) - (gamma + mu)*IS1
    dIS2 = epsilon2*(beta*SS2*(IP1 + alpha*IS1 + sigmaP*IP2 + sigmaS*alpha*IS2)/N) - (gamma + mu)*IS2
    dV = svax*nu*(SP + SS1 + SS2) - delta_vax*V - mu*V
    
    return(list(c(dSP, dIP1, dIP2, dR1, dR2, dSS1, dSS2, dIS1, dIS2, dV) ) )
    })
}