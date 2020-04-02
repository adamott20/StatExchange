library(dplyr)
library(ggplot2)
library(gganimate)
library(plyr)

set.seed(1)
nobs <- 200
start.directions <- runif(nobs,0,360)

start.patients <- data.frame(obs = 1:nobs,
                             x = runif(nobs), y = runif(nobs), 
                             xdir = cos(start.directions * pi / 180), 
                             ydir = sin(start.directions * pi / 180), 
                             speed = runif(nobs, .0025, .005), 
                             Age = sample(c("Young", "Old"), nobs, replace = TRUE, prob = c(.85,.15)), 
                             status = factor(c("Infected", rep("Susceptible",199)), 
                                             levels = c("Infected", "Susceptible", "Recovered", "Dead")), 
                             timeSinceInfect = c(1,rep(0,nobs-1)), time = 1)

movement.noRestrictions <- function(prev.data){
  new.x <- c()
  new.xdir <- c()
  new.y <- c()
  new.ydir <- c()
  new.status <- c()
  new.timeSinceInfect <- c()
  
  for(i in 1:nobs){
    ### Locations and Directions
    
    # If the person is dead, they no longer move
    if(prev.data$status[i] == "Dead") {
      new.x[i] <- prev.data$x[i]
      new.xdir[i] <- 0
      new.y[i] <- prev.data$y[i]
      new.ydir[i] <- 0
      prev.data$speed[i] <- 0
      
      # If it bounces off the right wall,
    } else if(prev.data$x[i] + prev.data$speed[i] * prev.data$xdir[i] > 1) {
      # Then the new X position is 1 - (how far past 1 it would have gone without the wall)
      new.x[i] <- 1 - (prev.data$x[i] + prev.data$speed[i] * prev.data$xdir[i] - 1)
      # and the new X direction is opposite
      new.xdir[i] <- -prev.data$xdir[i]
      
      # Else if it bounces off the left wall
    } else if(prev.data$x[i] + prev.data$speed[i] * prev.data$xdir[i] < 0) {
      new.x[i] <- -(prev.data$x[i] + prev.data$speed[i] * prev.data$xdir[i])
      new.xdir[i] <- -prev.data$xdir[i]
      
      # Otherwise, the new position is within the bounds
    } else{
      # Then the new X position is the previous plus the movement in that direction
      new.x[i] <- prev.data$x[i] + prev.data$speed[i] * prev.data$xdir[i]
      # and the new X direction is unchanged
      new.xdir[i] <- prev.data$xdir[i]
    }
    
    if(prev.data$status[i] == "Dead") {
      new.x[i] <- prev.data$x[i]
      new.xdir[i] <- 0
      new.y[i] <- prev.data$y[i]
      new.ydir[i] <- 0
      prev.data$speed[i] <- 0
      
      # If it bounces off the ceiling,
    } else if(prev.data$y[i] + prev.data$speed[i] * prev.data$ydir[i] > 1) {
      new.y[i] <- 1 - (prev.data$y[i] + prev.data$speed[i] * prev.data$ydir[i] - 1)
      new.ydir[i] <- -prev.data$ydir[i]
      
      # Bounces off the floor
    } else if(prev.data$y[i] + prev.data$speed[i] * prev.data$ydir[i] < 0) {
      new.y[i] <- -(prev.data$y[i] + prev.data$speed[i] * prev.data$ydir[i])
      new.ydir[i] <- -prev.data$ydir[i]
      
      # Does not bounce off ceiling nor floor
    } else{
      new.y[i] <- prev.data$y[i] + prev.data$speed[i] * prev.data$ydir[i]
      new.ydir[i] <- prev.data$ydir[i]
    }
  }
  
  for(i in 1:nobs){
    ### Status Changes
    
    # Continue with previous status
    new.status[i] <- as.character(prev.data$status[i])
    
    # Determine which other observations are the neighbors
    neighbors <- prev.data$obs[-i][(sqrt((new.y[-i]-new.y[i])^2 + (new.x[-i]-new.x[i])^2) < .05)]
    
    # If the subject has been infected for over 160, they recover
    if(prev.data$timeSinceInfect[i] > 160) {
      new.status[i] <- "Recovered"
      
      # If the subject is susceptible, and at least one neighbor is infected, then the subject is as well
    } else if("Infected" %in% prev.data$status[neighbors] & prev.data$status[i] == "Susceptible") {
      new.status[i] <- "Infected"
      
      # If the subject is infected and old, then the subject has a chance of becoming dead
    } else if(new.status[i] == "Infected" & prev.data$Age[i] == "Old") {
      perc.infected <- mean(prev.data$status == "Infected")
      new.status[i] <- sample(c("Infected", "Dead"), 1, prob = c(1-perc.infected*.05,perc.infected*.05))
    }
    
    # If infected, the infection timer increases
    if(new.status[i] == "Infected") {
      new.timeSinceInfect[i] <- prev.data$timeSinceInfect[i] + 1
    } else {
      new.timeSinceInfect[i] <- 0
    }
  }
  
  # Relevel Status
  new.status <- factor(new.status, levels = c("Infected", "Susceptible", "Recovered", "Dead"))
  
  # Output dataframe after one time of movement
  data.frame(obs = 1:nobs, x = new.x, y = new.y, 
             xdir = new.xdir, ydir = new.ydir, 
             speed = prev.data$speed, 
             Age = prev.data$Age, status = new.status, 
             timeSinceInfect = new.timeSinceInfect, 
             time = prev.data$time + 1)
}

time1 <- 400
times <- 1:time1
infect.data <- list()

infect.data[[1]] <- start.patients

for(time in times[-1]){
  infect.data[[time]] <- movement.noRestrictions(infect.data[[time - 1]])
}

full_infect <- bind_rows(infect.data)
full_infect$status <- factor(full_infect$status, levels = c("Infected", "Susceptible", "Recovered", "Dead"))

p <- ggplot(full_infect, aes(x = x, y = y, color = status)) +
  geom_point(aes(shape=Age)) +
  transition_time(time) +
  ggtitle(paste("Time: {round(frame_time,0)}")) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank()) + 
  geom_segment(aes(x = rep(c(0,0,0,1),time1*nobs/4), xend = rep(c(1,1,0,1),time1*nobs/4), 
                   y = rep(c(0,1,0,0),time1*nobs/4), yend = rep(c(0,1,1,1),time1*nobs/4)),
               color = "black")

#anim <- animate(p, fps = 20, nframes = time1, width = 800, height = 450)
#anim
#magick::image_write(anim, "No_Restrictions.gif")

no.move.start.patients <- start.patients
new.speeds <- c(start.patients$speed[1], ifelse(sapply(2:nobs,function(x) rbinom(1,1,.85)==1),0,start.patients$speed[-1]))
no.move.start.patients$speed <- new.speeds

time2 <- 650
times <- 1:time2
infect.data <- list()

infect.data[[1]] <- no.move.start.patients

for(time in times[-1]){
  infect.data[[time]] <- movement.noRestrictions(infect.data[[time - 1]])
}

full_infect <- bind_rows(infect.data)
full_infect$status <- factor(full_infect$status, levels = c("Infected", "Susceptible", "Recovered", "Dead"))

p <- ggplot(full_infect, aes(x = x, y = y, color = status)) +
  geom_point(aes(shape=Age)) +
  transition_time(time) +
  ggtitle(paste("Time: {round(frame_time,0)}")) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank()) + 
  geom_segment(aes(x = rep(c(0,0,0,1),time2*nobs/4), xend = rep(c(1,1,0,1),time2*nobs/4), 
                   y = rep(c(0,1,0,0),time2*nobs/4), yend = rep(c(0,1,1,1),time2*nobs/4)),
               color = "black")

#anim <- animate(p, fps = 20, nframes = time2, width = 800, height = 450)
#anim
#magick::image_write(anim, "Less_Movement.gif")

start.patients$x[1] <- runif(1,0,.25)
start.patients$quadrant <- ifelse(start.patients$x < .25, 1, 2)

movement.withBarrier <- function(prev.data){
  new.x <- c()
  new.xdir <- c()
  new.y <- c()
  new.ydir <- c()
  new.status <- c()
  new.timeSinceInfect <- c()
  
  for(i in 1:nobs){
    ### Locations and Directions
    
    # If the person is dead, they no longer move
    if(prev.data$status[i] == "Dead") {
      new.x[i] <- prev.data$x[i]
      new.xdir[i] <- 0
      new.y[i] <- prev.data$y[i]
      new.ydir[i] <- 0
      prev.data$speed[i] <- 0
      
      # If it bounces off the mid walls,
    } else if((prev.data$x[i] - .25) * ((prev.data$x[i] + prev.data$speed[i] * prev.data$xdir[i]) - .25) < 0) {
      if(prev.data$x[i] < .25){
        y.at.imp <- prev.data$y[i] + 
          prev.data$speed[i] * prev.data$ydir[i] * 
          (.25 - prev.data$x[i]) / (prev.data$speed[i] * prev.data$xdir[i])
      } else{
        y.at.imp <- prev.data$y[i] + 
          prev.data$speed[i] * prev.data$ydir[i] * 
          (prev.data$x[i] - .25) / (prev.data$speed[i] * prev.data$xdir[i])
      }
      
      if(!((y.at.imp > .4375) & (y.at.imp < .5625))){
        if(prev.data$x[i] < .25){
          new.x[i] <- .25 - 
            (prev.data$x[i] + prev.data$speed[i] * prev.data$xdir[i] - .25)
        } else{
          new.x[i] <- .25 + 
            (.25 - prev.data$x[i] - prev.data$speed[i] * prev.data$xdir[i])
        }
        
        new.xdir[i] <- -prev.data$xdir[i]
      } else{
        new.xdir[i] <- prev.data$xdir[i]
        new.x[i] <- prev.data$x[i] + prev.data$speed[i] * prev.data$xdir[i]
      }
      
      # If it bounces off the right wall,
    } else if(prev.data$x[i] + prev.data$speed[i] * prev.data$xdir[i] > 1) {
      # Then the new X position is 1 - (how far past 1 it would have gone without the wall)
      new.x[i] <- 1 - (prev.data$x[i] + prev.data$speed[i] * prev.data$xdir[i] - 1)
      # and the new X direction is opposite
      new.xdir[i] <- -prev.data$xdir[i]
      
      # Else if it bounces off the left wall
    } else if(prev.data$x[i] + prev.data$speed[i] * prev.data$xdir[i] < 0) {
      new.x[i] <- -(prev.data$x[i] + prev.data$speed[i] * prev.data$xdir[i])
      new.xdir[i] <- -prev.data$xdir[i]
      
      # Otherwise, the new position is within the bounds
    } else{
      # Then the new X position is the previous plus the movement in that direction
      new.x[i] <- prev.data$x[i] + prev.data$speed[i] * prev.data$xdir[i]
      # and the new X direction is unchanged
      new.xdir[i] <- prev.data$xdir[i]
    }
    
    if(prev.data$status[i] == "Dead") {
      new.x[i] <- prev.data$x[i]
      new.xdir[i] <- 0
      new.y[i] <- prev.data$y[i]
      new.ydir[i] <- 0
      prev.data$speed[i] <- 0
      
      # If it bounces off the ceiling,
    } else if(prev.data$y[i] + prev.data$speed[i] * prev.data$ydir[i] > 1) {
      new.y[i] <- 1 - (prev.data$y[i] + prev.data$speed[i] * prev.data$ydir[i] - 1)
      new.ydir[i] <- -prev.data$ydir[i]
      
      # Bounces off the floor
    } else if(prev.data$y[i] + prev.data$speed[i] * prev.data$ydir[i] < 0) {
      new.y[i] <- -(prev.data$y[i] + prev.data$speed[i] * prev.data$ydir[i])
      new.ydir[i] <- -prev.data$ydir[i]
      
      # Does not bounce off ceiling nor floor
    } else{
      new.y[i] <- prev.data$y[i] + prev.data$speed[i] * prev.data$ydir[i]
      new.ydir[i] <- prev.data$ydir[i]
    }
  }
  
  new.quadrant <- ifelse(new.x > .25, 2, 1)
  
  for(i in 1:nobs){
    
    ### Status Changes
    
    # Continue with previous status
    new.status[i] <- as.character(prev.data$status[i])
    
    # Determine which other observations are the neighbors
    neighbors <- prev.data$obs[-i][(sqrt((new.y[-i]-new.y[i])^2 + (new.x[-i]-new.x[i])^2) < .05)]
    neighbors <- neighbors[which(new.quadrant[neighbors] == new.quadrant[i])]
    
    # If the subject has been infected for over 160, they recover
    if(prev.data$timeSinceInfect[i] > 160) {
      new.status[i] <- "Recovered"
      
      # If the subject is susceptible, and at least one neighbor is infected, then the subject is as well
    } else if("Infected" %in% prev.data$status[neighbors] & prev.data$status[i] == "Susceptible") {
      new.status[i] <- "Infected"
      
      # If the subject is infected and old, then the subject has a chance of becoming dead
    } else if(new.status[i] == "Infected" & prev.data$Age[i] == "Old") {
      perc.infected <- mean(prev.data$status == "Infected")
      new.status[i] <- sample(c("Infected", "Dead"), 1, prob = c(1-perc.infected*.05,perc.infected*.05))
    }
    
    # If infected, the infection timer increases
    if(new.status[i] == "Infected") {
      new.timeSinceInfect[i] <- prev.data$timeSinceInfect[i] + 1
    } else {
      new.timeSinceInfect[i] <- 0
    }
  }
  
  # Relevel Status
  new.status <- factor(new.status, levels = c("Infected", "Susceptible", "Recovered", "Dead"))
  
  # Output dataframe after one time of movement
  data.frame(obs = 1:nobs, x = new.x, y = new.y, 
             xdir = new.xdir, ydir = new.ydir, 
             speed = prev.data$speed, 
             Age = prev.data$Age, status = new.status, 
             timeSinceInfect = new.timeSinceInfect, 
             time = prev.data$time + 1)
}

time3 <- 500
times <- 1:time3
infect.data <- list()

infect.data[[1]] <- start.patients

for(time in times[-1]){
  infect.data[[time]] <- movement.withBarrier(infect.data[[time - 1]])
}

full_infect <- bind_rows(infect.data)
full_infect$status <- factor(full_infect$status, levels = c("Infected", "Susceptible", "Recovered", "Dead"))

p <- ggplot(full_infect, aes(x = x, y = y, color = status)) +
  geom_point(aes(shape=Age)) +
  transition_time(time) +
  ggtitle(paste("Time: {round(frame_time,0)}")) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank()) + 
  geom_segment(aes(x = rep(c(.25,.25,0,1,0,0,0,0,0,0),nobs*time3 / 10), xend = rep(c(.25,.25,0,1,1,1,1,1,1,1),nobs*time3 / 10), 
                   y = rep(c(0,1,0,0,0,1,1,1,1,1),nobs*time3 / 10), yend = rep(c(.4375,.5625,1,1,0,1,1,1,1,1),nobs*time3 / 10)), 
               color = "black") 

#anim <- animate(p, fps = 20, nframes = time3, width = 800, height = 450)
#anim
#magick::image_write(anim, "With_Barrier.gif")

no.move.start.patients <- start.patients
new.speeds <- c(start.patients$speed[1], ifelse(sapply(2:nobs,function(x) rbinom(1,1,.85)==1),0,start.patients$speed[-1]))
no.move.start.patients$speed <- new.speeds

time4 <- 900
times <- 1:120
infect.data <- list()

infect.data[[1]] <- no.move.start.patients

for(time in times[-1]){
  infect.data[[time]] <- movement.withBarrier(infect.data[[time - 1]])
}

full_infect <- bind_rows(infect.data)
full_infect$status <- factor(full_infect$status, levels = c("Infected", "Susceptible", "Recovered", "Dead"))

p <- ggplot(full_infect, aes(x = x, y = y, color = status, label = obs)) +
  geom_point(aes(shape=Age)) +
  transition_time(time) +
  ggtitle(paste("Time: {round(frame_time,0)}")) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank()) + 
  geom_segment(aes(x = rep(c(.25,.25,0,1,0,0,0,0,0,0),nobs*time4 / 10), xend = rep(c(.25,.25,0,1,1,1,1,1,1,1),nobs*time4 / 10), 
                   y = rep(c(0,1,0,0,0,1,1,1,1,1),nobs*time4 / 10), yend = rep(c(.4375,.5625,1,1,0,1,1,1,1,1),nobs*time4 / 10)), 
               color = "black")

anim <- animate(p, fps = 20, nframes = time4, width = 800, height = 450)
# anim
magick::image_write(anim, "With_Barrier.less.move.gif")