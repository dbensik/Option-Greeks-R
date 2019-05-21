################ HELPER FUNCTIONS ###################
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

############### END HELPER FUNCTIONS ################

# Calculates call option prices using Black-Scholes method.
# S is the stock current price.
# X is the strike price at expiration date
# t is the time from now until expiration, expressed in years
# r is the risk-free interest rate
# v is the standard diviation (or volatility)
call.value <- function(S, X, t, r, v)
{
    d1 <- (log(S/X)+(r+ 0.5 * v^2) * t) / ( v * sqrt(t))
    d2 <- d1 - v * sqrt(t)
    result <- S * pnorm(d1) - X * exp(-r * t) * pnorm(d2)
    result
}


# Calculates put option prices using Black-Scholes method.
# S is the stock current price.
# X is the strike price at expiration date
# t is the time from now until expiration, expressed in years
# r is the risk-free interest rate
# v is the standard diviation (or volatility)
put.value  <- function(S, X, t, r, v)
{
    d1 <- (log(S/X) + (r + 0.5 * v^2) * t)/(v * sqrt(t))
    d2 <- d1 - v * sqrt(t)
    X * exp(-r * t) * pnorm(-d2)- S * pnorm(-d1)
}



# Calculate delta function
# S is the stock current price. 
# X is the strike price at expiration date 
# t is the time from now until expiration, expressed in years 
# r is the risk-free interest rate 
# v is the standard diviation (or volatility) 
# type: either 0 means call option and 1 means put option
delta <- function(type, S, X, t, r, v)
{
    d1 <- (log(S/X) + (r + 0.5 * v^2) * t)/(v * sqrt(t));
    if(type > 0)	# put
        pnorm(d1) -1
    else			# call
        pnorm(d1);
    
}


# Calculate gamma function
# S is the stock current price. 
# X is the strike price at expiration date 
# t is the time from now until expiration, expressed in years 
# r is the risk-free interest rate 
# v is the standard diviation (or volatility) 
# type: either 0 means call option and 1 means put option
gamma <- function(type, S, X, t, r, v)
{
    d <- (log(S/X) + (r + 0.5 * v^2) * t)/(v * sqrt(t));
    top <- exp(-d^2/2)
    bottom <- S * v * sqrt(2*pi*t)
    top/bottom
}


# Calculate rho function
# S is the stock current price. 
# X is the strike price at expiration date 
# t is the time from now until expiration, expressed in years 
# r is the risk-free interest rate 
# v is the standard diviation (or volatility) 
# type: either 0 means call option and 1 means put option
rho <- function(type, S, X, t, r, v)
{
    d1 <- (log(S/X)+(r+ 0.5 * v^2) * t) / ( v * sqrt(t))
    d2 <- d1 - v * sqrt(t)
    if(type >0)	# put
        -X * t * exp(-r * t) * pnorm(-d2)
    else	# call	
        X * t * exp(-r * t) * pnorm(d2);
}



# Calculate vega function
# S is the stock current price. 
# X is the strike price at expiration date 
# t is the time from now until expiration, expressed in years 
# r is the risk-free interest rate 
# v is the standard diviation (or volatility) 
# type: either 0 means call option and 1 means put option
vega <- function(type, S, X, t, r, v)
{
    d1 <- (log(S/X)+(r+ 0.5 * v^2) * t) / ( v * sqrt(t))
    S * sqrt(t) * ( exp(-d1^2/2)/ sqrt(2 * pi) )
}


# Calculate theta function
# S is the stock current price. 
# X is the strike price at expiration date 
# t is the time from now until expiration, expressed in years 
# r is the risk-free interest rate 
# v is the standard diviation (or volatility) 
# type: either 0 means call option and 1 means put option
theta <- function(type, S, X, t, r, v)
{
    d1 <- (log(S/X)+(r+ 0.5 * v^2) * t) / ( v * sqrt(t))
    d2 <- d1 - v * sqrt(t)
    if(type >0)	# put
    {
        p1 <- -S * exp(-d1^2/2) * v / (2 * sqrt(2 * pi * t) )
        p2 <- r * X * exp(-r * t) * pnorm(1 - pnorm(d2))	        
        p1 + p2
    }
    else	# call	
    {
        p1 <- -S * exp(-d1^2/2) * v / (2 * sqrt(2 * pi * t) ) 
        p2 <- r * X * exp(-r * t) * pnorm(d2);	        
        p1 - p2
    }
}

# common call for greeks
# S is the stock current price. 
# X is the strike price at expiration date 
# t is the time from now until expiration, expressed in years 
# r is the risk-free interest rate 
# v is the standard diviation (or volatility) 
# type: either 0 means call option and 1 means put option
# function is is either "delta", 
#  "theta",  "vega",  "rho",  "gamma"
greeks <- function(type, fnc, S, X, t, r, v)
{
    if (fnc=="delta")
    {
        delta(type, S, X, t, r, v)
    }
    else if(fnc=="vega")
    {
        vega(type, S, X, t, r, v)
    }
    else if(fnc=="gamma")
    {
        gamma(type, S, X, t, r, v)
    }
    else if(fnc=="rho")
    {
        rho(type, S, X, t, r, v)
    }
    else
    {
        theta(type, S, X, t, r, v);
    }
}


# The following code produces a 3D perspective 
# plot for the Black-Schole Call option.
CallOption3DPlot = function ( S, X, t, r, v, 
                              theta = 30, phi = 30, expand = 0.75, 
                              col = "lightblue", ltheta = 120, shade = 0.75, 
                              ticktype = "detailed", cex = 0.6, 
                              main = "Black-Scholes Call Option Price", ...)
{   
    premium3D = function(S, t,  X, r, v) 
    {
        call.value(S, X, t, r, v)
    }
    # Prices:
    Price = outer(S, t, FUN = premium3D,  X, r,  v)
    
    # Perspective Plot:
    persp(x = S, y = t, z = Price, xlab = "Spot", ylab = "Time", zlab = "Option Value",
          theta = theta, phi = phi, expand = expand, col = col, 
          ltheta = ltheta, shade = shade, ticktype = ticktype, 
          cex = cex, main = main, ...) 
    
    # Return Value:
    invisible(list(S = S, t= t, Price = Price))  
}

# The following code produces a 3D perspective 
# plot for the Black-Scholes Put option.
PutOption3DPlot = function ( S, X, t, r, v, 
                              theta = 30, phi = 30, expand = 0.75, 
                              col = "lightblue", ltheta = 120, shade = 0.75, 
                              ticktype = "detailed", cex = 0.6, 
                              main = "Black-Scholes Put Option Price", ...)
{   
    premium3D = function(S, t, X, r, v) 
    {
        put.value(S, X, t, r, v)
    }
    # Prices:
    Price = outer(S, t, FUN = premium3D,  X, r,  v)
    
    # Perspective Plot:
    persp(x = S, y = t, z = Price, xlab = "Spot", ylab = "Time", zlab = "Option Value",
          theta = theta, phi = phi, expand = expand, col = col, 
          ltheta = ltheta, shade = shade, ticktype = ticktype, 
          cex = cex, main = main, ...) 
    
    # Return Value:
    invisible(list(S = S, t= t, Price = Price))  
}




#----------------------------------------------------------------
# Applications
#----------------------------------------------------------------
# Black-Scholes Call - 3D Plot for Premium: 
# par(mfrow = c(2, 1), cex = 1/3)
par(mfrow = c(1,1), cex = 1)
CallOption3DPlot( S = seq(from = 75, to = 125, length = 40), 
                  X = 100, 
                  t = seq(from = 1/52, to = 1, length = 40), 
                  r = 0.1, 
                  v = 0.4)


# Black-Scholes Put - 3D Plot for Premium: 
# par(mfrow = c(2, 1), cex = 1/3)
par(mfrow = c(1,1), cex = 1)
PutOption3DPlot( S = seq(from = 75, to = 125, length = 40), 
                  X = 100, 
                  t = seq(from = 1/52, to = 1, length = 40), 
                  r = 0.1, 
                  v = 0.4)

#----------------------------------------------------------------
# Applications
#----------------------------------------------------------------
# Calculate the sensitivities for some selected options

# Delta Call
delta(0, S = 105, X = 100, t = 0.5, r = 0.1, v = 0.36)

# Delta Put
delta(1, S = 105, X = 100, t = 0.5, r = 0.1, v = 0.36)  

# Theta Call
theta(0, S = 430, X = 405, t = 1/12, r = 0.07, v = 0.20)

# Theta Put    
theta(1, S = 430, X = 405, t = 1/365, r = 0.07, v = 0.20)

# Vega Call
vega(0, S = 55, X = 60, t = 0.75, r = 0.10, v = 0.30)

# Vega Put
vega(1, S = 55, X = 60, t = 0.75, r = 0.10, v = 0.30)

# Rho Call
rho(0, S = 72, X = 75, t = 1, r = 0.09, v = 0.19)

# Rho Put
rho(1, S = 72, X = 75, t = 1, r = 0.09, v = 0.19)

# Gamma Call
gamma(0, S = 55, X = 60, t = 0.75, r = 0.10, v = 0.30)

# Gamma Put
gamma(1, S = 55, X = 60, t = 0.75, r = 0.10, v = 0.30)

for (fnc in c("delta", "theta",  "vega",  "rho",  "gamma"))
{
    for (type in c(0,1))
    {
        cat(fnc);
        if (type==0)
        {
            cat(" Call");
        }
        else
        {
            cat(" Put");
        }
        cat("\n");
        cat (greeks(type, fnc, 430, 405, 0.5, 0.10, 0.19) )
        cat("\n");
    }
}

plotOptionVsUnderlying <- function(type, func, S, X, t, r, v){
    if (type == 0){option = "Call"}
    else{option = "Put"}      
    
    if (func == "spot") {
        S = seq(from = 50, to = 150, length = 100)
        variable = "Spot"
        if(type == 0){
            optionValue = call.value(S, X, t, r, v)
        }
        else{
            optionValue = put.value(S, X, t, r, v)
        }
        yMin = min(optionValue)
        yMax = max(optionValue)
        xMin = min(S)
        xMax = max(S)
        x = S
    }
    else if (func == "strike"){
        X = seq(from = 50, to = 150, length = 100)
        variable = "strike"
        if(type == 0){
            optionValue = call.value(S, X, t, r, v)
        }
        else{
            optionValue = put.value(S, X, t, r, v)
        }
        yMin = min(optionValue)
        yMax = max(optionValue)
        xMin = min(X)
        xMax = max(X)
        x = X
    }
    else if (func == "volatility"){
        v = seq(from = 0.1, to = 0.5, length = 100)
        variable = "Volatility"
        if(type == 0){
            optionValue = call.value(S, X, t, r, v)
        }
        else{
            optionValue = put.value(S, X, t, r, v)
        }
        yMin = min(optionValue)
        yMax = max(optionValue)
        xMin = min(v)
        xMax = max(v)
        x = v
    }
    else if (func == "time"){
        t = seq(from = 0, to = 1, length = 100)
        variable = "time"
        if(type == 0){
            optionValue = call.value(S, X, t, r, v)
        }
        else{
            optionValue = put.value(S, X, t, r, v)
        }
        yMin = min(optionValue)
        yMax = max(optionValue)
        xMin = min(t)
        xMax = max(t)
        x = t
    }
    else if (func == "rate"){
        r = seq(from = 0.0, to = .25, length = 100)
        variable = "Interest Rate"
        if(type == 0){
            optionValue = call.value(S, X, t, r, v)
        }
        else{
            optionValue = put.value(S, X, t, r, v)
        }
        yMin = min(optionValue)
        yMax = max(optionValue)
        xMin = min(r)
        xMax = max(r)
        x = r
    }
    
    plot(x, x, type = "n", xlim = c(xMin, xMax), xlab = simpleCap(variable), 
         ylab = "Option Value", ylim = c(yMin, yMax),#ylim = c(-1,1),
         main = paste(option, " Value vs. Change in ", simpleCap(variable), sep="")) 
    lines(x, optionValue)
}


# plot a 2D graph for the greeks
# type is either 1 for put or 0 for call
# function is one of "delta", "gamma", "vega", "theta", or "rho"
# S means Stock price
# X indicates the strike price
# t is the time till expiration
# r is the interest rate
# v is the volatility of the stock

plot2DGreeks <- function(type, func, S, X, t, r, v) # plot2DGreeks(1, greekType, S = S, X = 1.7, t = i/365, r = 0.06, v = 0.1)
{
    #par(mfrow = c(2, 2), cex = 0.7)
    ##S = seq(from = 1.4, to = 2.0, length = 250)
    #S = seq(from = 0, to = X * 2, length = 250)
    
    if (type == 0){
        option = "Call"
    }
    else{
        option = "Put"
    }      
    
    if (func == "delta") {
        greek = delta(type, S = S, X, t, r, v)
        yMin = min(greek)
        yMax = max(greek)
        #lines(S, greek) 
    }
    else if (func == "gamma")
    {
        greek = gamma(type, S=S, X , t, r, v)
        yMin = min(greek)
        yMax = max(greek)
        #lines (S, greek, ylim=c(-2,2)) 
    }
    else if (func == "vega")
    {
        greek = vega(type, S=S, X , t, r, v)
        yMin = min(greek)
        yMax = max(greek)
        #lines (S, greek) 
    }
    else if (func == "theta")
    {
        greek = theta(type, S=S, X , t, r, v)
        yMin = min(greek)
        yMax = max(greek)
        #lines (S, greek) 
    }
    else if (func == "rho")
    {
        greek = rho(type, S, X , t, r, v)
        yMin = min(greek)
        yMax = max(greek)
        #lines (S, greek) 
    }
    plot(S, S, type = "n", xlab = "S", ylab = simpleCap(func), ylim = c(yMin, yMax),#ylim = c(-1,1),
         main = paste(option, " ", simpleCap(func), " No. Days = ", t*365, sep="")) 
    
    lines(S, greek) 
}



# x-axis represents Stock price, 
# y-axis represents time
# z-axis represents the greek function
# type is either 0 for call or 1 for put
# func is any of the five string "delta", "gamma", "vega", "theta", and "rho".
# S is the stock price
# X is the strike price
# t is the time to expiration
# r is the interest rate
# v is the volatility

plot3DGreeks <- function (type, func, S, X, t, r, v, theta = 30, 
                          phi = 30, expand = 0.75, col = "cyan", ltheta = 120, shade = 0.75, 
                          ticktype = "detailed", cex = 0.6, 
                          main = paste("Black-Scholes ", type , " Option Sensitivity for ", simpleCap(func), sep="") , ...)
{  
    greeks3D = function(S, t, X, r, v) 
    { 
        if (func == "delta")
        {
            delta(type, S, X, t, r, v)
        }
        else if (func == "gamma")
        {
            gamma(type, S, X, t, r, v)
        }
        else if (func == "vega")
        {
            vega(type, S, X, t, r, v)
        }
        else if (func == "theta")
        {
            theta(type, S, X, t, r, v)
        }
        else if (func == "rho")
        {
            rho(type, S, X, t, r, v)
        }
    }
    
    # Sensitivities:
    Greeks = outer(S, t, FUN = greeks3D, X, r, v)
    
    # Perspective Plot:
    persp(x=S, y=t, z=Greeks, xlab="S", ylab="Time", zlab = simpleCap(func), # replaced zlab = "Delta" with zlab = func
          theta=theta, phi=phi, expand=expand, col=col, ltheta=ltheta,
          shade=shade, ticktype=ticktype, cex=cex, main=main, ...) 
    
    # Return Value:
    invisible(list(S = S, t = t, Sensitivity = Greeks))
}

# Example usage
# Black-Scholes - Plot3D Greeks:
#par(mfrow = c(3, 2), cex = 1/3)
##par(mfrow = c(2,1), cex = 1/2)
types <- c(0, 1)
S <- seq(from = 75, to = 125, length = 25) 
t <- seq(from = 1/52, to = 1, length = 25)
for (fnc in c("delta", "theta",  "vega",  "rho",  "gamma"))
{
    for (i in types)
    {
        plot3DGreeks(i, fnc, S = S, X = 100, t = t, r = 0.1, v = 0.40)
    }
}

#----------------------------------------------------------------
# Applications
#----------------------------------------------------------------
# A 2D plot to show relationship between call delta 
# and strike price together with expiration date
# 2D Plot for Delta for different expiration amounts
# par(mfrow = c(2, 2), cex = 0.7)
# S = seq(from = 1.0, to = 2.0, length = 250)
# days = c(1, 120, 240, 365)
# greekType = "delta"
# for (i in days)
# {
#     plot2DGreeks(1, greekType, S = S, X = 1.5, t = i/365, r = 0.06, v = 0.1)
# }
# 
# par(mfrow = c(2, 2), cex = 0.7)
# S = seq(from = 1.0, to = 2.0, length = 250)
# days = c(1, 120, 240, 365)
# greekType = "gamma"
# for (i in days)
# {
#     plot2DGreeks(0, greekType, S = S, X = 1.5, t = i/365, r = 0.06, v = 0.1)
# }
# 
# par(mfrow = c(2, 2), cex = 0.7)
# S = seq(from = 1, to = 2, length = 250)
# days = c(1, 120, 240, 365)
# greekType = "gamma"
# for (i in days)
# {
#     plot2DGreeks(1, greekType, S = S, X = 1.5, t = i/365, r = 0.06, v = 0.1)
# }

for (fnc in c("delta", "gamma", "theta",  "vega",  "rho")) {
    for (type in c(0,1)) {
        par(mfrow = c(2, 2), cex = 0.7)
        S = seq(from = 1, to = 2, length = 250)
        days = c(1, 120, 240, 365)
        for (i in days) {
            plot2DGreeks(type, fnc, S = S, X = 1.5, t = i/365, r = 0.06, v = 0.1)
        }
    }
}

for (fnc in c("spot", "strike",  "time",  "rate", "volatility")) 
    {
    par(mfrow = c(1, 2), cex = 0.5)
    for (type in c(0,1)) {
        plotOptionVsUnderlying(type, fnc, S = 100, X = 105, t = 1, r = 0.05, v = 0.1)
    }
}

par(mfrow = c(3, 1), cex = 0.5)
S = seq(from = 60, to = 140, length = 100)
optionValue = call.value(S, X = 100, t = 1/12, r = 0.05, v = 0.1)
plot(S, optionValue, type = "l", main = "Standard Call (X = $100)", bty = "n", lty = 1, xlab = "", ylab = "Call Value")
optionValue = call.value(S, X = 100, t = 1/2, r = 0.05, v = 0.1)
lines(S, optionValue, lty = 2, col = "red")
optionValue = call.value(S, X = 100, t = 1, r = 0.05, v = 0.1)
lines(S, optionValue, lty = 3, col = "blue")
legend('left',c("12 mo", "6 mo", "1 mo"), lty=c(1,2,3), lwd=c(1,1,1),col=c("black","red", "blue"),bty="n")

del = delta(0, S, X = 100, t = 1/12, r = 0.05, v = 0.1)
plot(S, del, type = "l", bty = "n", lty = 1, xlab = "", ylab = "Call Delta")
del = delta(0, S, X = 100, t = 1/2, r = 0.05, v = 0.1)
lines(S, del, lty = 2, col = "red")
del = delta(0, S, X = 100, t = 1, r = 0.05, v = 0.1)
lines(S, del, lty = 3, col = "blue")
legend('left',c("12 mo", "6 mo", "1 mo"), lty=c(1,2,3), lwd=c(1,1,1),col=c("black","red", "blue"),bty="n")

gam = gamma(0, S, X = 100, t = 1/12, r = 0.05, v = 0.1)
plot(S, gam, type = "l", bty = "n", lty = 1, xlab = "Spot Price", ylab = "Call Gamma", ylim = c(0,max(gam)))
gam = gamma(0, S, X = 100, t = 1/2, r = 0.05, v = 0.1)
lines(S, gam, lty = 2, col = "red")
gam = gamma(0, S, X = 100, t = 1, r = 0.05, v = 0.1)
lines(S, gam, lty = 3, col = "blue")
legend('left',c("12 mo", "6 mo", "1 mo"), lty=c(1,2,3), lwd=c(1,1,1),col=c("black","red", "blue"),bty="n")

par(mfrow = c(3, 1), cex = 0.5)
S = seq(from = 60, to = 140, length = 100)
thet = theta(0, S, X = 100, t = 1/12, r = 0.05, v = 0.1)
plot(S, thet, type = "l", main = "Standard Call (X = $100)", bty = "n", lty = 1, xlab = "", ylab = "Call Theta")
the = theta(0, S, X = 100, t = 1/2, r = 0.05, v = 0.1)
lines(S, the, lty = 2, col = "red")
the = theta(0, S, X = 100, t = 1, r = 0.05, v = 0.1)
lines(S, the, lty = 3, col = "blue")
legend('left',c("12 mo", "6 mo", "1 mo"), lty=c(1,2,3), lwd=c(1,1,1),col=c("black","red", "blue"),bty="n")

veg = vega(0, S, X = 100, t = 1, r = 0.05, v = 0.1)
plot(S, veg, type = "l", bty = "n", lty = 1, xlab = "", ylab = "Call Vega")
veg = vega(0, S, X = 100, t = 1/2, r = 0.05, v = 0.1)
lines(S, veg, lty = 2, col = "red")
veg = vega(0, S, X = 100, t = 1/12, r = 0.05, v = 0.1)
lines(S, veg, lty = 3, col = "blue")
legend('left',c("12 mo", "6 mo", "1 mo"), lty=c(1,2,3), lwd=c(1,1,1),col=c("black","red", "blue"),bty="n")

S = seq(from = 60, to = 140, length = 100)
rh = rho(0, S, X = 100, t = 1, r = 0.05, v = 0.1)
plot(S, rh, type = "l", bty = "n", lty = 1, xlab = "Spot Price", ylab = "Call Rho", ylim = c(0,max(rh)))
rh = rho(0, S, X = 100, t = 1/2, r = 0.05, v = 0.1)
lines(S, rh, lty = 2, col = "red")
rh = rho(0, S, X = 100, t = 1/12, r = 0.05, v = 0.1)
lines(S, rh, lty = 3, col = "blue")
legend('left',c("12 mo", "6 mo", "1 mo"), lty=c(1,2,3), lwd=c(1,1,1),col=c("black","red", "blue"),bty="n")

par(mfrow = c(1, 1), cex = 0.5, cex = 0.75)
S = seq(from = 60, to = 140, length = 100)
optionValue = call.value(S, X = 100, t = 1, r = 0.0, v = 0.1)
plot(S, optionValue, type = "l", main = "European Call vs Put (X = $100)", bty = "n", lty = 1,
     xlab = "Spot", ylab = "Option Value")
optionValue = put.value(S, X = 100, t = 1, r = 0.0, v = 0.1)
lines(S, optionValue, lty = 2, col = "red")
legend('left',c("Call", "Put"), lty=c(1,2), lwd=c(1,1),col=c("black","red"),bty="n")




