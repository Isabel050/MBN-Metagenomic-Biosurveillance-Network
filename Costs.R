# Generate Cost Data for ShinyApp Rmd

Cost <- function(hospitals = 1, years = 10) {
  # Costs per site:
  Cost_Sequencers <- 450000
  Cost_Staff <- 225000
  Cost_Floor_Space <- 0 # Get value.
  Cost_Compute_Storage <- 24000
  Cost_Sequencing_Reagents_Yearly <- 2300 * 365 * 2 # 2 Machines, both run 1x/day
  Costs_Reagents_Per_Sample <- 60 # Need to multiply by number of patients
  Patients_Per_Year <- 50000 # Current number. This might need to change dynamically? (Maybe todo: put in function definition and call it again.)
  Yearly_cost <- Cost_Staff + Cost_Compute_Storage + Cost_Sequencing_Reagents_Yearly + (Costs_Reagents_Per_Sample * Patients_Per_Year)

  return(hospitals * (Cost_Sequencers + sum(Yearly_cost / (1.03) ^ (1:years))))
}