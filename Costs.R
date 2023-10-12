# Generate Cost Data for ShinyApp Rmd
require("FinancialMath") # For NPV in costs

Cost<-function(number){
  #Costs per site:
  Cost_Sequencers = 450000
  Cost_Staff = 225000
  Cost_Floor_Space = 0 # Get value.
  Cost_Compute_Storage = 24000
  Cost_Sequencing_Reagents_Yearly = 2300*365*2 # 2 Machines, both run 1x/day
  Costs_Reagents_Per_Sample = 60 #Need to multiply by number of patients
  Patients_Per_Year = 50000 # Current number. This might need to change dynamically? (Maybe todo: put in function definition and call it again.)
  Yearly_cost = Cost_Staff+Cost_Compute_Storage+Cost_Sequencing_Reagents_Yearly + (Costs_Reagents_Per_Sample + Patients_Per_Year)
  #Fixed this to include the per-patient costs.
  
  Fixed_Cost = Cost_Sequencers
  return(number*(NPV(cf0=Fixed_Cost,cf=rep(Yearly_cost,10),times=c(1:10),i=0.03)))
}

Overall_10y_Cost_AnyNumber = sapply(c(1:100),Cost) #Get the costs to graph.
