# Set up a 2D Grid with two different particles randomly moving around. Movment is in 8 directions. one step for each time cycle. 
# particles that land in the same space get a chance (probability) to collide. Times (cycles) are input as a list and a Graph is
# plotted as collisions as a function of time.

particleA_Num <- 200
AssocA <- 0.0
DissA <- 1.0
ParticleB_Num <- 200
AssocB <- 0.0
DissB <- 1.0

Matrix_X <- 200
Matrix_Y <- 200

timeSeriesLst <- c(5,10,15,20,30,40,60,80,100,120,150,200) # each value is the step number for an individual diffusion experiment. The
# particle distribution plot will be for the last experiment. The mean distance for the particles for each experiment is saved in "dataSetD"
# saved as DataB with DataA holding the timeSeriesLst
dataSetD <- 1:length(timeSeriesLst)
directionPaths <- 8
directionLst <- 1:8
stepVectorX <- c(0,1,1,1,0,-1,-1,-1)
stepVectorY <- c(1,1,0,-1,-1,-1,0,1) # movment in x and y for movment direction value


# create a matrix of data for the diffusing particles with each row a different particle: 1st & 2nd columns are the X & Y positions, 3rd column is the movment direction, 
# and the 3rd is the single number representing the location. The 4th & 5th are the association probability and dissociation probability
ParticleA_Set <- matrix(0, particleA_Num, 3)
ParticleB_Set <- matrix(0, particleB_Num, 3)

# Misc functions
locationCalc <- function(x,y){(x-1)*Matrix_Y+y}

cntr <- 1 
for (stepNumber in timeSeriesLst) {

  # first create a andom distribution of particles in the grid
  ParticleA_Set[,1] <- sample.int(Matrix_X, size=particleA_Num, replace = TRUE, prob = NULL)
  ParticleA_Set[,2] <- sample.int(Matrix_Y, size=particleA_Num, replace = TRUE, prob = NULL)
  ParticleA_Set[,3] <- locationCalc(ParticleA_Set[,1],ParticleA_Set[,2])}
  ParticleA_Set[,4] <- AssocA
  ParticleA_Set[,4] <- DissA
  ParticleB_Set[,1] <- sample.int(Matrix_X, size=particleA_Num, replace = TRUE, prob = NULL)
  ParticleB_Set[,2] <- sample.int(Matrix_Y, size=particleA_Num, replace = TRUE, prob = NULL)
  ParticleB_Set[,3] <- locationCalc(ParticleB_Set[,1],ParticleB_Set[,2])}
  ParticleA_Set[,4] <- AssocB
  ParticleA_Set[,4] <- DissB
