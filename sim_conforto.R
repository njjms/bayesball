# simulate hits in a season with beta prior and a column of At-Bats
sim.conforto <- function(data, ini.alpha, ini.beta) {
    data %>% 
    dplyr::select(Date, AB) %>% 
    rowid_to_column("Game") %>% 
    mutate(
        H = rbinom(n = nrow(data), size = AB, prob = p),
        cumAB = cumsum(AB),
        cumH = cumsum(H),
        cumAverage = cumH/cumAB,
        ebAverage = (ini.alpha + cumH)/(ini.alpha + ini.beta + cumAB),
        alpha = ini.alpha + cumH,
        beta = ini.beta + cumAB - cumH
    ) -> output
    
    return(output)
}

# simulate hits in a season with beta prior and a column of At-Bats but this time p is allowed to vary
sim.conforto.p <- function(data, ini.alpha, ini.beta, p) {
    data$p <- rbeta(nrow(data), shape1 = conforto.2017.prior[1], shape2 = conforto.2017.prior[2])
    data$H <- mapply(function(x, y){rbinom(1, x, y)}, data$AB, data$p)
    
    data %>% 
        dplyr::select(Date, p, AB, H) %>% 
        rowid_to_column("Game") %>% 
        mutate(
            cumAB = cumsum(AB),
            cumH = cumsum(H),
            cumAverage = cumH/cumAB,
            ebAverage = (ini.alpha + cumH)/(ini.alpha + ini.beta + cumAB),
            alpha = ini.alpha + cumH,
            beta = ini.beta + cumAB - cumH
        ) -> output
    
    return(output)
}

# heuristic function that can adjust our overall mets prior to better fit a specific player
adjust_prior <- function(p, shape1 = 10.6, shape2=36.76, epsilon = .001) {
    if(p > (shape1/(shape1 + shape2))) {
        while (abs((p - (shape1/(shape1 + shape2)))) >= epsilon) {
           shape1 <- shape1 + .01
        }
        output <- c(shape1 = shape1, shape2 = shape2)
        return(output)
    } else {
        while (abs((p - (shape1/(shape1 + shape2)))) >= epsilon) {
           shape1 <- shape1 - .01
        }
        output <- c(shape1 = shape1, shape2 = shape2)
        return(output)
    }
}

# calculate the MLE beta distribution parameters for the New York Mets from particular season
mets_prior <- function(year.id) {
    require(Lahman)
    
    mets <- Batting %>% 
        filter(AB > 0 & teamID == "NYN" & yearID == year.id) %>% 
        anti_join(Pitching, by = "playerID") %>%
        group_by(playerID) %>% 
        summarise(
            H = sum(H),
            AB = sum(AB)
        ) %>% 
        mutate(
            average = H/AB
        )
    
    m <- MASS::fitdistr(mets$average[!(mets$average %in% c(0,1))],
                        dbeta,
                        start = list(shape1 = 15, shape2 = 40))
    return(m)    
}
