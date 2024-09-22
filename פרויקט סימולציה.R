library(rlang)
library(MASS)
library(fitdistrplus)
library(magrittr)
library(dplyr)
library(lazyeval)
library(parallel)
library(e1071)
library(plotly)
library(ggplot2)
library(triangle)
library(sqldf)
#library(readxl)
# library(knitr)
# library(rmarkdown)
library(simmer)
library(simmer.plot)

################################### function ##########################

####### addService 
addService<- function  (trajectory,sname,timeDist){
  updatedPath <- seize(trajectory, resource = sname, amount = 1)%>%
    timeout(timeDist) %>%
    release(resource = sname, amount = 1)
  return(updatedPath)
}

miss<- function(Airport,in_duty_free){
  #print()
  if(get_attribute(Airport,"flight time") <= now(Airport) && in_duty_free == 0){
    return(TRUE)
  }
  return(FALSE)
}

####### select_AddService + leave
selectAddService<- function  (trajectory,snames,timeDist,id,policy = "round-robin",Airport,in_duty_free = 0 ){
    updatedPath <-trajectory%>%
    leave(function(){if(get_attribute(Airport,"flight time") <= now(Airport) && in_duty_free == 0){return(TRUE)}  ### Leaving if the flight is late
      return(FALSE)
    })%>%
    select(snames,policy = policy,id = id )%>%
    seize_selected(id = id,amount = 1)%>%
    timeout(timeDist) %>%
    release_selected(id = id)
  return(updatedPath)
}


#trimmed norm
absrnorm <- function(mu,sd){
  while(TRUE){
    sample<-rnorm(1,mu,sd)
    if (sample>0)
      return (sample)
  }
}

#from the airlines           
airline <- function(){
  #1-"Wix"
  #2-Beta
  #3-Simojet
  
  return(rdiscrete(1,c(0.4,0.25,0.35),c(1,2,3)))
}

#from the passport
npassport<- function(){
  #1-ישראלי
  #2-זר
  return(rdiscrete(1,c(0.68,0.32),c(1,2)))
}

#from the taxi passport
npassport_taxi<-function(Number_in_group){
 # print(Number_in_group)
  for (i in 1:Number_in_group) {
    if( npassport() == 2){return(1)}
  }
  return(2)
}

#from passport check post
get_passport_check<-function(passport,Airline){
  p<-c("passport check Wix Hebrew",
    "passport check Wix English",
    "passport check Beta Hebrew",
    "passport check Beta English",
    "passport check Simojet Hebrew",
    "passport check Simojet English")

  return(p[passport+(Airline-1)*2])
 
}

#from passport check time
get_passport_check_time<-function(size){
  return(sum(rtriangle(size,a=5,b=7)))
}

#from the check in post by Airline
check_in<-function(Airline){
  p<-c("check in Wix",
       "check in Beta",
       "check in Simojet")
  return(p[Airline])
}


#Luggage weight distribution
luggage_weight<-function(){
  f <- runif(1,0,1)
  if(f<=0.4){return(15+(62.5*f)^0.5) }
  if(f>0.8){return(30-(125*(1-f))^0.5)}
  return(runif(1,20,25))
}

#Calculates the check in time
check_in_time<-function(Airline,size = 1){
  #print(Airline)
  time<-0.0
  for(i in 1:size){
    
   if(Airline == 1){ # Wix"
      time<-time+absrnorm(7,3)
     if(luggage_weight()>23){time<-time+runif(1,2,5) }}
    
    else if(Airline == 2){#  Beta
      time<-time+absrnorm(6.5,8)
      if(luggage_weight()>20){time<-time+runif(1,4,7) }}
    
    else if(Airline == 3){# Simojet
      time<-time+absrnorm(7.5,20)
      if(luggage_weight()>25){time<-time+runif(1,1,3) }}
  }
  return(time)
}

#resource by number
get_compound_name<-function(i){
  z<-c("alcohol compound","clothing compound","perfume compound","Food compound","ATM 1","ATM 2","ATM 3")
  return(z[i])
}

compound_order<-function (Airport){
  if(get_attribute(Airport,"Arrival by") == 2)
    {return(c(sample(c(3,4,as.integer(runif(1,5,8))),3),1,2))}
  else
    {return(c(sample(c(1:4,as.integer(runif(1,5,8))),5)))}
}
# duty free passing order 
compound_time<-function (Airport,n){
  if((get_attribute(Airport ,"Arrival by") == 3 && n == 1) || get_attribute(Airport,"flight time") <= now(Airport) ){return(0)}
  return(rexp(1,1/15))
}

#flight time from the beginning of the simulation
flight_time <- function(Airport){return(as.integer(now(Airport)+runif(1,3*60,5*60)))}

#if in business class 1-true , 0-false
business_class<- function(){if(runif(1,0,1)<0.23){ return(1)} else return(0)}


###################################################   simulation #######################################
set.seed(456)
simulation_time = 60*(22-4)
private_car_distribution = function() rexp(1,0.564)
taxi_distribution = function() rexp(1,0.2644598)
  
Airport<- simmer("Airport")%>%
###################################################  resources  #########################################
  add_resource("Swimmingpool",capacity=1,queue_size=Inf)%>%
  add_resource("passport check Wix Hebrew",capacity=5,queue_size=Inf)%>%
  add_resource("passport check Wix English",capacity=3,queue_size=Inf)%>%
  add_resource("passport check Beta Hebrew",capacity=5,queue_size=Inf)%>%
  add_resource("passport check Beta English",capacity=3,queue_size=Inf)%>%
  add_resource("passport check Simojet Hebrew",capacity=5,queue_size=Inf)%>%
  add_resource("passport check Simojet English",capacity=3,queue_size=Inf)%>%

  add_resource("check in Wix",capacity=4,queue_size=Inf)%>%
  add_resource("check in Beta",capacity=3,queue_size=Inf)%>%
  add_resource("check in Simojet",capacity=3,queue_size=Inf)%>%
  
  add_resource("alcohol compound",capacity=10,queue_size=Inf)%>%
  add_resource("perfume compound",capacity=10,queue_size=Inf)%>%
  add_resource("clothing compound",capacity=5,queue_size=Inf)%>%
  add_resource("Food compound",capacity=30,queue_size=Inf)

  for(i in 1:5){
  add_resource(Airport,paste("check in Simojet",i),capacity=1,queue_size=Inf)
  }
  for(i in 1:4){
  add_resource(Airport,paste("passport control",i),capacity=1,queue_size=Inf)
  }

  for(i in 1:3){
    add_resource(Airport,paste("ATM",i),capacity=1,queue_size=Inf)
  }

  
##############################################  trajectories #############################################

#############################   duty free
duty_free<-trajectory("duty_free")%>%
  set_attribute(keys = c("compound 1","compound 2","compound 3","compound 4","compound 5"),values = function()compound_order(Airport))%>%
  selectAddService(function()get_compound_name(get_attribute(Airport,"compound 1")),function() compound_time(Airport,0),1,Airport=Airport,in_duty_free =1)%>%
  selectAddService(function()get_compound_name(get_attribute(Airport,"compound 2")),function() compound_time(Airport,0),2,Airport=Airport,in_duty_free =1)%>%
  selectAddService(function()get_compound_name(get_attribute(Airport,"compound 3")),function() compound_time(Airport,0),3,Airport=Airport,in_duty_free =1)%>%
  selectAddService(function()get_compound_name(get_attribute(Airport,"compound 4")),function() compound_time(Airport,1),4,Airport=Airport,in_duty_free =1)%>%
  selectAddService(function()get_compound_name(get_attribute(Airport,"compound 5")),function() compound_time(Airport,1),5,Airport=Airport,in_duty_free =1)%>%
  timeout(function() rexp(1,1/15))  # Organize the shopping in the bag


Security_check<-trajectory("Security_check")%>%
  ###########################  check in Simojet
  set_prioritization(function(){return(c(get_attribute(Airport,"business class"), get_attribute(Airport,"business class"), F) )})%>%
  selectAddService(c("check in Simojet 1","check in Simojet 2","check in Simojet 3","check in Simojet 4","check in Simojet 5"),
                   function() runif(1,3,5), id = 1,"random",Airport=Airport,in_duty_free =0)%>%
  ###########################   passport control
  set_prioritization (c(0, T, F) )%>%
  selectAddService(c("passport control 1","passport control 2","passport control 3","passport control 4"),
                   function() rexp(1,1/1.5), id = 2,"random",Airport=Airport,in_duty_free =0)%>%
  synchronize(wait = TRUE,mon_all = TRUE)%>%
  clone(function(){if (get_attribute(Airport ,"Arrival by") == 2 ){ return(get_attribute(Airport,"Number in group"))}
    return(1)
  },duty_free,duty_free,duty_free,duty_free)
  
  
 


#arrival_attribute 1-private_car , 2-taxi ,3-train


arrival_private_car<-trajectory("arrival_private_car")%>%
  set_attribute("flight time",function()flight_time(Airport))%>%
  set_attribute("Arrival by",1)%>%
  set_attribute("Airline",function() airline())%>%
  set_attribute("business class",function() business_class())%>%
  set_attribute("passport", function() npassport())%>%
  ##### passport check
  selectAddService(function() get_passport_check(get_attribute(Airport,"passport"),get_attribute(Airport,"Airline")),function() rtriangle(1,a=5,b=7),id=1,Airport = Airport)%>%
  ##### check in
  selectAddService(function() check_in(get_attribute(Airport,"Airline")),function()check_in_time(get_attribute(Airport,"Airline"),1),2,Airport = Airport)%>%
  leave( function() runif(1,0,1)<0.06)%>%
  join(Security_check)
  

  
arrival_taxi<-trajectory("arrival_taxi")%>%
  set_attribute("flight time",function()flight_time(Airport))%>%
  set_attribute("Arrival by",2)%>%
  set_attribute("Number in group",function() rdiscrete(1,c(0.42,0.47,0.11),c(2,3,4)))%>%
  set_attribute("Airline",function() airline())%>%
  set_attribute("passport", function() npassport_taxi(get_attribute(Airport,"Number in group")))%>%
  set_attribute("business class",function() business_class())%>%
  ##### passport check
  selectAddService(function() get_passport_check(get_attribute(Airport,"passport"),get_attribute(Airport,"Airline")),function() rtriangle(1,a=5,b=7),id=1,Airport = Airport)%>%
  ##### check in
  selectAddService(function() check_in(get_attribute(Airport,"Airline")),function()check_in_time(get_attribute(Airport,"Airline"),get_attribute(Airport,"Number in group")),2,Airport = Airport)%>%
  leave(function() min(runif(get_attribute(Airport,"Number in group"),0,1)>=0.06))%>%
  clone(function() get_attribute(Airport,"Number in group"),Security_check,Security_check,Security_check,Security_check)

arrival_train<-trajectory("arrival_train")%>%
  set_attribute("flight time",function()flight_time(Airport))%>%
  set_attribute("Arrival by",3)%>%
  set_attribute("Airline",function() airline())%>%
  set_attribute("passport", function() npassport())%>%
  set_attribute("business class",function() business_class())%>%
  renege_in(function()get_attribute(Airport,"flight time") , out = NULL, keep_seized = FALSE)%>%
  leave(prob = function() at (get_attribute(Airport,"flight time")), keep_seized = FALSE)%>%
  ##### passport check
  selectAddService(function() get_passport_check(get_attribute(Airport,"passport"),get_attribute(Airport,"Airline")),function() rtriangle(1,a=5,b=7),id=1,Airport = Airport)%>%
  ##### check in
  selectAddService(function() check_in(get_attribute(Airport,"Airline")),function()check_in_time(get_attribute(Airport,"Airline"),1),2,Airport = Airport)%>%
  leave(function() runif(1,0,1)<0.06)%>%
  join(Security_check)

  train_arrive<-trajectory()%>%activate("train")

  Airport%>%
add_generator(name="private_car", trajectory=arrival_private_car,mon = 2, distribution=private_car_distribution)%>%
add_generator(name="taxi", trajectory=arrival_taxi,mon = 2,distribution=taxi_distribution)%>%
add_generator(name = "train",trajectory=arrival_train,mon = 2, distribution=when_activated(function() as.integer(runif(1,25,36))))%>%
    
add_generator(name = "train_arrive", trajectory = train_arrive,distribution =  
                at(seq(0,simulation_time,90)),mon = 1)



reset(Airport)
run(Airport, until = simulation_time)#?simulation_time <- 10 ? 


resources<-get_mon_resources(Airport)
attributes<-get_mon_attributes(Airport)