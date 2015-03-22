# Set up a 2D Grid with two different particles randomly moving around. Movment is in 8 directions. one step for each time cycle. 
# particles that land in the same space get a chance (probability) to collide. Times (cycles) are input as a list and a Graph is
# plotted as collisions as a function of time.

particleA_Num <- 200
AssocA <- 0.0
DissA <- 1.0
ParticleB_Num <- 200
AssocB <- 0.0
DissB <- 1.0
collisionProb <- 0.3
Matrix_X <- 200
Matrix_Y <- 200

timeSeriesLst <- c(5,10,15,20,30,40,60,80,100,120,150,200) # each value is the step number for an individual collision experiment. The
# particle collisions versus the times will ne plotted. 

dataSetC <- 1:length(timeSeriesLst)
directionPaths <- 8
directionLst <- 1:8
stepVectorX <- c(0,1,1,1,0,-1,-1,-1)
stepVectorY <- c(1,1,0,-1,-1,-1,0,1) # movment in x and y for movment direction value


# create a matrix of data for the diffusing particles with each row a different particle: 1st & 2nd columns are the X & Y positions, 3rd column is the movment direction, 
# and the 3rd column is a single number representing the location. The 4th & 5th are the association probability and dissociation probability
# the 6th column are the movement directions for each particle
ParticleA_Set <- matrix(0, particleA_Num, 6)
ParticleB_Set <- matrix(0, particleB_Num, 6)

# Misc functions
locationCalc <- function(x,y){(x-1)*Matrix_Y+y}

cntr <- 1 
for (stepNumber in timeSeriesLst) {

  # first create and define a random distribution of particles, A and B, in the grid
  ParticleA_Set[,1] <- sample.int(Matrix_X, size=particleA_Num, replace = TRUE, prob = NULL)
  ParticleA_Set[,2] <- sample.int(Matrix_Y, size=particleA_Num, replace = TRUE, prob = NULL)
  ParticleA_Set[,3] <- locationCalc(ParticleA_Set[,1],ParticleA_Set[,2])
  ParticleA_Set[,4] <- AssocA
  ParticleA_Set[,5] <- DissA
  ParticleA_Set[,6] <- sample.int(directionPaths, size = particleA_Num, replace = TRUE, prob = NULL)

  ParticleB_Set[,1] <- sample.int(Matrix_X, size=particleB_Num, replace = TRUE, prob = NULL)
  ParticleB_Set[,2] <- sample.int(Matrix_Y, size=particleB_Num, replace = TRUE, prob = NULL)
  ParticleB_Set[,3] <- locationCalc(ParticleB_Set[,1],ParticleB_Set[,2])}
  ParticleB_Set[,4] <- AssocB
  ParticleB_Set[,5] <- DissB
  ParticleB_Set[,6] <- sample.int(directionPaths, size = particleB_Num, replace = TRUE, prob = NULL)

# calculate the collisions of the initial setup
  dataSetC[cntr]<-length(intersection(particleA_Set[,3],particleB_Set[,3]))*collisionProb
  
for (j in 1:stepNumber){
    
  ParticleA_Set[,1] <- ParticleA_Set[,1] + stepVectorX[ParticleA_Set[,6]]
  ParticleA_Set[,2] <- ParticleA_Set[,2] + stepVectorY[ParticleA_Set[,6]]
  ParticleB_Set[,1] <- ParticleA_Set[,1] + stepVectorX[ParticleB_Set[,6]]
  ParticleB_Set[,2] <- ParticleA_Set[,2] + stepVectorY[ParticleB_Set[,6]]
  
  # calculate if particles have escaped the defined grid area and bring them back through the opposite walls
  tempVec <- which(ParticleA_Set[,1] > Matrix_X)
  for(pq in tempVec) {ParticleA_Set[pq,1] <- ParticleA_Set[pq,1]-Matrix_X}
  tempVec <- which(ParticleA_Set[,1] < 1)
  for(pq in tempVec) {ParticleA_Set[pq,1] <- ParticleA_Set[pq,1]+Matrix_X}
  tempVec <- which(ParticleA_Set[,2] > Matrix_Y)
  for(pq in tempVec) {ParticleA_Set[pq,2] <- ParticleA_Set[pq,2]-Matrix_Y}
  tempVec <- which(ParticleA_Set[,2] < 1)
  for(pq in tempVec) {ParticleA_Set[pq,2] <- ParticleA_Set[pq,2]+Matrix_Y} 
  
  tempVec <- which(ParticleB_Set[,1] > Matrix_X)
  for(pq in tempVec) {ParticleB_Set[pq,1] <- ParticleB_Set[pq,1]-Matrix_X}
  tempVec <- which(ParticleB_Set[,1] < 1)
  for(pq in tempVec) {ParticleB_Set[pq,1] <- ParticleB_Set[pq,1]+Matrix_X}
  tempVec <- which(ParticleB_Set[,2] > Matrix_Y)
  for(pq in tempVec) {ParticleB_Set[pq,2] <- ParticleB_Set[pq,2]-Matrix_Y}
  tempVec <- which(ParticleB_Set[,2] < 1)
  for(pq in tempVec) {ParticleB_Set[pq,2] <- ParticleB_Set[pq,2]+Matrix_Y} 
  
  # calculate the position number from the new x,y coordinates for each particle
  ParticleA_Set[,3] <- locationCalc(ParticleA_Set[,1],ParticleA_Set[,2])
  ParticleB_Set[,3] <- locationCalc(ParticleB_Set[,1],ParticleB_Set[,2])
  
  ParticleA_Set[,6] <- sample.int(directionPaths, size = particleA_Num, replace = TRUE, prob = NULL)
  ParticleB_Set[,6] <- sample.int(directionPaths, size = particleB_Num, replace = TRUE, prob = NULL)
  
  # calculate the number of collisions for the last step
  dataSetC[cntr]<-dataSetC[cntr]+length(intersection(particleA_Set[,3],particleB_Set[,3]))*collisionProb

  } # end of stepNumber iterations 

cntr <- cntr+1
}