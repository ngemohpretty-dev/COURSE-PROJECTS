############################################################
# Baseline MNL with only ASCs (7 alternatives)
# Shared micromobility = Bike-sharing + E-scooter-sharing
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

# 3) Treat -1 as NA (attribute not applicable)
database[database == -1] <- NA

# 4) Create SHARED MICROMOBILITY attributes
database <- database %>%
  mutate(
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

# 5) Apollo control
apollo_control <- list(
  modelName  = "MNL_baseline_ASCs_only",
  modelDescr = "Baseline multinomial logit with only alternative-specific constants",
  indivID    = "respondent",  # respondent ID column
  mixing     = FALSE,
  nCores     = 1
)

# 6) Parameters (ASCs). Walking ASC fixed to 0 as reference.
apollo_beta <- c(
  asc_walk       = 0,  # reference
  asc_car        = 0,
  asc_pt         = 0,
  asc_bike       = 0,
  asc_carsharing = 0,
  asc_sharedmm   = 0,
  asc_none       = 0
)

apollo_fixed <- c("asc_walk")

# 7) Validate inputs
apollo_inputs <- apollo_validateInputs()

# 8) Probability function
apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_inputs))
  
  P <- list()
  
  # ---- Utilities (V) ----
  V <- list()
  V[["Walking"]]             <- asc_walk
  V[["PrivateCar"]]          <- asc_car
  V[["PublicTransport"]]     <- asc_pt
  V[["PrivateBike"]]         <- asc_bike
  V[["CarSharing"]]          <- asc_carsharing
  V[["SharedMicromobility"]] <- asc_sharedmm
  V[["None"]]                <- asc_none
  
  # ---- Availabilities ----
  av <- list()
  av[["Walking"]]             <- av_Walking
  av[["PrivateCar"]]          <- av_PrivateCar
  av[["PublicTransport"]]     <- av_PublicTransport
  av[["PrivateBike"]]         <- av_PrivateBike
  av[["CarSharing"]]          <- av_CarSharing
  av[["SharedMicromobility"]] <- av_SharedMicromobility
  av[["None"]]                <- 1  # None of the above always available
  
  # ---- MNL settings ----
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
    choiceVar = choiceNumber,  # numeric code in your data
    V         = V
  )
  
  # Probabilities for each OBSERVATION
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  
  # *** NEW STEP: multiply over observations of same individual ***
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  
  # Prepare and return (averages over individuals etc.)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# 9) Estimate the baseline model
model_baseline <- apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs
)

# 10) Output results
apollo_modelOutput(model_baseline)   # print to console
apollo_saveOutput(model_baseline)    # save to files

