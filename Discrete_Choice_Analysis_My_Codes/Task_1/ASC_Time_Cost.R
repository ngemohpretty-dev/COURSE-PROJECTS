############################################################
# MODEL 2: MNL with ASCs + generic Travel Time + Travel Cost
# Dataset: DCM_1_dataset.csv
# 7 alternatives (SharedMicromobility = Bike + E-scooter)
############################################################

# install.packages("apollo")    # run once if needed
# install.packages("tidyverse") # run once if needed

library(apollo)
library(tidyverse)

# 1) Initialise Apollo
apollo_initialise()

# 2) Load data
file_path <- "C:/Users/ASUS/OneDrive - TUM/Desktop/Assignment DCA/Assignment 1 DCA/DCM_1_dataset.csv"
database  <- read.csv(file_path)

# 3) Treat -1 as NA (attribute not applicable / not defined)
database[database == -1] <- NA

# 4) Create SHARED MICROMOBILITY attributes
#    Using your actual column names from names(database)
database <- database %>%
  mutate(
    # availability: 1 if either BikeSharing OR EScooterSharing available
    av_SharedMicromobility = if_else(
      av_BikeSharing == 1 | av_EScooterSharing == 1, 1L, 0L
    ),
    
    AccessEgressTime_SharedMicromobility =
      coalesce(AccessEgressTime_BikeSharing, AccessEgressTime_EScooterSharing),
    
    TravelTime_SharedMicromobility =
      coalesce(TravelTime_BikeSharing, TravelTime_EScooterSharing),
    
    SearchParking_SharedMicromobility =
      coalesce(SearchParking_BikeSharing, SearchParking_EScooterSharing),
    
    TravelCost_SharedMicromobility =
      coalesce(TravelCost_BikeSharing, TravelCost_EScooterSharing),
    
    ParkingCost_SharedMicromobility =
      coalesce(ParkingCost_BikeSharing, ParkingCost_EScooterSharing),
    
    Availability_SharedMicromobility =
      coalesce(Availability_BikeSharing, Availability_EScooterSharing)
  )

# 5) Replace remaining NAs in TIME and COST variables with 0
#    (for unavailable alternatives; they won’t be used when av = 0)
database <- database %>%
  mutate(
    TravelTime_Walking         = ifelse(is.na(TravelTime_Walking),         0, TravelTime_Walking),
    TravelTime_PrivateCar      = ifelse(is.na(TravelTime_PrivateCar),      0, TravelTime_PrivateCar),
    TravelTime_PublicTransport = ifelse(is.na(TravelTime_PublicTransport), 0, TravelTime_PublicTransport),
    TravelTime_PrivateBike     = ifelse(is.na(TravelTime_PrivateBike),     0, TravelTime_PrivateBike),
    TravelTime_CarSharing      = ifelse(is.na(TravelTime_CarSharing),      0, TravelTime_CarSharing),
    TravelTime_SharedMicromobility =
      ifelse(is.na(TravelTime_SharedMicromobility), 0, TravelTime_SharedMicromobility),
    
    TravelCost_Walking         = ifelse(is.na(TravelCost_Walking),         0, TravelCost_Walking),
    TravelCost_PrivateCar      = ifelse(is.na(TravelCost_PrivateCar),      0, TravelCost_PrivateCar),
    TravelCost_PublicTransport = ifelse(is.na(TravelCost_PublicTransport), 0, TravelCost_PublicTransport),
    TravelCost_PrivateBike     = ifelse(is.na(TravelCost_PrivateBike),     0, TravelCost_PrivateBike),
    TravelCost_CarSharing      = ifelse(is.na(TravelCost_CarSharing),      0, TravelCost_CarSharing),
    TravelCost_SharedMicromobility =
      ifelse(is.na(TravelCost_SharedMicromobility), 0, TravelCost_SharedMicromobility)
  )

# 6) Apollo control settings
apollo_control <- list(
  modelName  = "MNL_model2_time_cost",
  modelDescr = "MNL with ASCs and generic travel time & travel cost",
  indivID    = "respondent",  # respondent ID column
  mixing     = FALSE,
  nCores     = 1
)

# 7) Parameters:
#    ASCs (walking fixed to 0) + generic beta_time and beta_cost
apollo_beta <- c(
  asc_walk       = 0,   # reference alternative
  asc_car        = 0,
  asc_pt         = 0,
  asc_bike       = 0,
  asc_carsharing = 0,
  asc_sharedmm   = 0,
  asc_none       = 0,
  beta_time      = 0,
  beta_cost      = 0
)

apollo_fixed <- c("asc_walk")   # fix reference ASC

# 8) Validate inputs
apollo_inputs <- apollo_validateInputs()

# 9) Probability function for Model 2
apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_inputs))
  
  P <- list()
  
  # ----- Utilities (V) with generic time & cost -----
  V <- list()
  
  V[["Walking"]] <- asc_walk +
    beta_time * TravelTime_Walking +
    beta_cost * TravelCost_Walking
  
  V[["PrivateCar"]] <- asc_car +
    beta_time * TravelTime_PrivateCar +
    beta_cost * TravelCost_PrivateCar
  
  V[["PublicTransport"]] <- asc_pt +
    beta_time * TravelTime_PublicTransport +
    beta_cost * TravelCost_PublicTransport
  
  V[["PrivateBike"]] <- asc_bike +
    beta_time * TravelTime_PrivateBike +
    beta_cost * TravelCost_PrivateBike
  
  V[["CarSharing"]] <- asc_carsharing +
    beta_time * TravelTime_CarSharing +
    beta_cost * TravelCost_CarSharing
  
  V[["SharedMicromobility"]] <- asc_sharedmm +
    beta_time * TravelTime_SharedMicromobility +
    beta_cost * TravelCost_SharedMicromobility
  
  # "None of the above" has no time/cost
  V[["None"]] <- asc_none
  
  # ----- Availabilities -----
  av <- list()
  av[["Walking"]]             <- av_Walking
  av[["PrivateCar"]]          <- av_PrivateCar
  av[["PublicTransport"]]     <- av_PublicTransport
  av[["PrivateBike"]]         <- av_PrivateBike
  av[["CarSharing"]]          <- av_CarSharing
  av[["SharedMicromobility"]] <- av_SharedMicromobility
  av[["None"]]                <- 1  # always available
  
  # ----- MNL settings -----
  # Mapping alternative names to numeric codes in choiceNumber:
  # 1=Walking, 2=PrivateCar, 3=PublicTransport, 4=PrivateBike,
  # 5=CarSharing, 8=SharedMicromobility, 9=None
  mnl_settings <- list(
    alternatives = c(
      Walking             = 1,
      PrivateCar          = 2,
      PublicTransport     = 3,
      PrivateBike         = 4,
      CarSharing          = 5,
      SharedMicromobility = 8,
      None                = 9
    ),
    avail     = av,
    choiceVar = choiceNumber,
    V         = V
  )
  
  # Observation-level probabilities
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  
  # Panel data: multiple observations per respondent
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  
  # Prepare and return
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# 10) Estimate Model 2
model2 <- apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs
)

# 11) Output results
apollo_modelOutput(model2)
apollo_saveOutput(model2)
