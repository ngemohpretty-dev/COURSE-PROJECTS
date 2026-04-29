############################################################
# MODEL 3: MNL with ASCs +
#          alternative-specific Travel Time & Travel Cost
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
#    Using your actual column names
database <- database %>%
  mutate(
    # availability: 1 if either sharing mode available
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

# 5) Replace remaining NAs in TIME and COST with 0
#    (for unavailable alternatives; they won't influence utility when av = 0)
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

# 6) Apollo control
apollo_control <- list(
  modelName  = "MNL_model3_alt_specific_time_cost",
  modelDescr = "MNL with ASCs + alternative-specific travel time and cost",
  indivID    = "respondent",
  mixing     = FALSE,
  nCores     = 1
)

# 7) Parameters:
#    - ASCs (walking fixed as reference)
#    - one time coefficient per mode
#    - one cost coefficient per paid mode (car, PT, car-sharing, sharedMM)
apollo_beta <- c(
  # ASCs
  asc_walk       = 0,   # reference alternative
  asc_car        = 0,
  asc_pt         = 0,
  asc_bike       = 0,
  asc_carsharing = 0,
  asc_sharedmm   = 0,
  asc_none       = 0,
  # time coefficients
  b_time_walk    = 0,
  b_time_car     = 0,
  b_time_pt      = 0,
  b_time_bike    = 0,
  b_time_carsh   = 0,
  b_time_shmm    = 0,
  # cost coefficients (only modes with non-zero cost)
  b_cost_car     = 0,
  b_cost_pt      = 0,
  b_cost_carsh   = 0,
  b_cost_shmm    = 0
)

apollo_fixed <- c("asc_walk")  # Walking ASC fixed

# 8) Validate inputs
apollo_inputs <- apollo_validateInputs()

# 9) Probability function
apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_inputs))
  
  P <- list()
  
  # ---------- Utilities ----------
  V <- list()
  
  V[["Walking"]] <- asc_walk +
    b_time_walk * TravelTime_Walking
  # no cost term: walking cost always zero
  
  V[["PrivateCar"]] <- asc_car +
    b_time_car * TravelTime_PrivateCar +
    b_cost_car * TravelCost_PrivateCar
  
  V[["PublicTransport"]] <- asc_pt +
    b_time_pt * TravelTime_PublicTransport +
    b_cost_pt * TravelCost_PublicTransport
  
  V[["PrivateBike"]] <- asc_bike +
    b_time_bike * TravelTime_PrivateBike
  # no cost term: private bike cost always zero
  
  V[["CarSharing"]] <- asc_carsharing +
    b_time_carsh * TravelTime_CarSharing +
    b_cost_carsh * TravelCost_CarSharing
  
  V[["SharedMicromobility"]] <- asc_sharedmm +
    b_time_shmm * TravelTime_SharedMicromobility +
    b_cost_shmm * TravelCost_SharedMicromobility
  
  V[["None"]] <- asc_none
  
  # ---------- Availabilities ----------
  av <- list()
  av[["Walking"]]             <- av_Walking
  av[["PrivateCar"]]          <- av_PrivateCar
  av[["PublicTransport"]]     <- av_PublicTransport
  av[["PrivateBike"]]         <- av_PrivateBike
  av[["CarSharing"]]          <- av_CarSharing
  av[["SharedMicromobility"]] <- av_SharedMicromobility
  av[["None"]]                <- 1  # always available
  
  # ---------- MNL settings ----------
  # choiceNumber codes: 1,2,3,4,5,8,9
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
  
  # Panel structure: multiple observations per respondent
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  
  # Prepare & return
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# 10) Estimate Model 3
model3 <- apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs
)

# 11) Output results
apollo_modelOutput(model3)
apollo_saveOutput(model3)

