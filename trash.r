create_opt_obj<-function(){
  bs_Spot<-as.numeric(readline())
  bs_Strike<-as.numeric(readline())
  bs_Sigma<-as.numeric(readline())
  bs_Rate<-as.numeric(readline())
  bs_Time<-as.numeric(readline())
  option_obj<-c(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time)
}

bs_call <- function(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time) {
   D1<-bs_D1(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time)
   D2<-(D1 - (bs_Sigma * sqrt(bs_Time)))
   bs_Spot*pnorm(D1)-(pnorm(D2)*bs_Strike*exp(-bs_Rate*bs_Time))
}

bs_D1 <- function(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time) {
     (log(bs_Spot/bs_Strike)+(bs_Rate*bs_Time))/(sqrt(bs_Time)*bs_Sigma)+(0.5*sqrt(bs_Time)*bs_Sigma)
   }

bs_call_outer <- function(bs_Spot, bs_Sigma, bs_Strike, bs_Rate, bs_Time) {
  D1<-bs_D1(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time)
  D2<-(D1 - (bs_Sigma * sqrt(bs_Time)))
  bs_Spot*pnorm(D1)-(pnorm(D2)*bs_Strike*exp(-bs_Rate*bs_Time))
}


bs_put <- function(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time) {
  D1<-bs_D1(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time)
  D2<-(D1 - (bs_Sigma * sqrt(bs_Time)))
  pnorm(-D2)*bs_Strike*exp(-bs_Rate*bs_Time)-pnorm(-D1)*bs_Spot
}

bs_call_delta<-function(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time) {
  pnorm(bs_D1(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time))
}

bs_put_delta<-function(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time) {
  pnorm(bs_D1(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time))-1
}

bs_gamma<-function(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time) {
  dnorm(bs_D1(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time))/(bs_Spot*bs_Sigma*sqrt(bs_Time))
}

bs_vega<-function(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time) {
  bs_Spot*dnorm(bs_D1(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time))*sqrt(bs_Time)
}

bs_call_theta<-function(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time) {
  D1<-bs_D1(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time)
  D2<-(D1 - (bs_Sigma * sqrt(bs_Time)))
  (-(bs_Sigma*bs_Spot*dnorm(D1))/(2*bs_Time))-bs_Rate*bs_Strike*exp(-bs_Rate*bs_Time)*pnorm(D2)
}

bs_put_theta<-function(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time) {
  D1<-bs_D1(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time)
  D2<-(D1 - (bs_Sigma * sqrt(bs_Time)))
  (-(bs_Sigma*bs_Spot*dnorm(D1))/(2*bs_Time))+bs_Rate*bs_Strike*exp(-bs_Rate*bs_Time)*pnorm(-D2)
}

bs_call_rho<-function(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time) {
  D1<-bs_D1(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time)
  D2<-(D1 - (bs_Sigma * sqrt(bs_Time)))
  bs_Strike*bs_Time*exp(-bs_Rate*bs_Time)*pnorm(D2)
}

bs_put_rho<-function(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time) {
  D1<-bs_D1(bs_Spot, bs_Strike, bs_Sigma, bs_Rate, bs_Time)
  D2<-(D1 - (bs_Sigma * sqrt(bs_Time)))
  -bs_Strike*bs_Time*exp(-bs_Rate*bs_Time)*pnorm(-D2)
}

bs_call_impvol<-function(bs_Spot,bs_Strike,bs_Price,bs_Rate,bs_Time) {
  
  accuracy<-1e-5
  max_iterations<-1000
  high_value<-1e9
  error<- -1e40
  
  min_Sigma<-1e-4
  min_price<-bs_call(bs_Spot,bs_Strike,min_Sigma,bs_Rate,bs_Time)
  if (bs_Price<min_price) impvol <- 0 else
    
  Sigma_high<-0.25
  price<-bs_call(bs_Spot,bs_Strike,Sigma_high,bs_Rate,bs_Time)
  while (price<bs_Price) {
    Sigma_high<-2*Sigma_high
    price<-bs_call(bs_Spot,bs_Strike,Sigma_high,bs_Rate,bs_Time)
    if (Sigma_high > high_value) return("Error")
  }
  for(i in 1:max_iterations) {
    bs_Sigma <- (min_Sigma+Sigma_high)*0.5
   price<-bs_call(bs_Spot,bs_Strike,bs_Sigma,bs_Rate,bs_Time)
    price_test <- (price-bs_Price)
    if (abs(price_test)<accuracy) return(bs_Sigma)
    else if (price_test<0) {
      min_Sigma<-bs_Sigma
    }
    else {
      Sigma_high <- bs_Sigma
    }
  }
}

bs_put_impvol<-function(bs_Spot,bs_Strike,bs_Price,bs_Rate,bs_Time) {
  
  accuracy<-1e-5
  max_iterations<-1000
  high_value<-1e9
  error<- -1e10
  i<-1
  
  min_Sigma<-1e-4
  min_price<-bs_put(bs_Spot,bs_Strike,min_Sigma,bs_Rate,bs_Time)
  if (bs_Price<min_price) impvol <- 0 else
    
    Sigma_high<-0.25
  price<-bs_put(bs_Spot,bs_Strike,Sigma_high,bs_Rate,bs_Time)
  while (price<bs_Price) {
    Sigma_high<-2*Sigma_high
    price<-bs_put(bs_Spot,bs_Strike,Sigma_high,bs_Rate,bs_Time)
    if (Sigma_high > high_value) return("Error")
  }
  for(i in 1:max_iterations) {
    bs_Sigma <- (min_Sigma+Sigma_high)*0.5
    price<-bs_put(bs_Spot,bs_Strike,bs_Sigma,bs_Rate,bs_Time)
    price_test <- (price-bs_Price)
    if (abs(price_test)<accuracy) return(bs_Sigma)
    else if (price_test<0) {
      min_Sigma<-bs_Sigma
    }
    else {
      Sigma_high <- bs_Sigma
    }
  }
}



bs_spot_bump_hi<-function(bs_Spot, distance = 0.05, steps = 16) {
  stopifnot(distance <= 1, steps > 1, steps%%1==0)
  spot_step<-(bs_Spot*distance)/(steps/2)
  bs_spot_axis <- integer(steps+1)
  for (i in 1:steps) {
    bs_spot_axis[1]<-(bs_Spot*(1-distance))
    bs_spot_axis[i+1]<-bs_spot_axis[i]+spot_step
  }
  bs_spot_axis
}

bs_vol_bump_hi<-function(bs_vol, distance = 0.8, steps = 16) {
  stopifnot(distance <= 1, steps > 1, steps%%1==0)
  vol_step<-(bs_vol*distance)/(steps/2)
  bs_vol_axis <- integer(steps+1)
  for (i in 1:steps) {
    bs_vol_axis[1]<-(bs_vol*(1-distance))
    bs_vol_axis[i+1]<-bs_vol_axis[i]+vol_step
  }
  bs_vol_axis
}


bs_call_matrix<-function(bs_Spot,bs_Sigma,bs_Strike,bs_Rate,bs_Time) {
  output_spot_axis <- bs_spot_bump_hi(bs_Spot)
  output_vol_axis <- bs_vol_bump_hi(bs_Sigma)
  #output_spot_axis <- c(80,90,100,110,120)
  #output_vol_axis <- c(0.05,0.15,0.25,0.35,0.45)
  output_matrix<-matrix(nrow=length(output_spot_axis),ncol=length(output_vol_axis))
  output_matrix<-outer(output_spot_axis,output_vol_axis,bs_call_outer,bs_Strike,bs_Rate,bs_Time)
  return(output_matrix)  
}


bs_putcall_impliedrate <-function(bs_Spot, bs_Call, bs_Put, bs_Strike, bs_Time, bs_Rate) {
# Formula: Call - Put = Spot * Stock_DF - Strike * RiskFree_DF where 
# Stock_DF -> Exp(-qT) and RiskFree_DF -> Exp(-rT) where q -> dividend - Cost of borrow
# rearranged, q = -(1/T)*Ln((Call-Put+Strike*RiskFree_DF)/Spot)
  RiskFree_DF<-exp(-bs_Rate*bs_Time)
  (1/bs_Time)*log((bs_Call-bs_Put+bs_Strike*RiskFree_DF)/bs_Spot)
}


