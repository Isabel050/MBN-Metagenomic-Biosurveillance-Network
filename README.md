# Metagenomic Biosurveillance Network

The web user interface for this model is available at https://oscar-delaney.shinyapps.io/MBN_app
The associated paper is currently unpublished, but we will link to it when it is available as a preprint, and once published.

File descriptions:
- stochastic.R is the bones of the model, defining the SEIR model, detection system, and graphing functionality.
- Oscar_shiny.Rmd creates the web interface
- Documentation.Rmd is part of the web interface
- figures.R creates the tables and figures that appear in the manuscript
- Hospital Visitors.csv is data on the health system used in the model, currently Israel.
- Results.RData is the output of many simulation runs used in the web app, to save computation time

Note that customizing the model for other countries requires new values for the hospitals (or other relevant testing locations,) in the indicated csv, as well as updated assumptions about costs contained in stochastic.R

Technical SEIR details:
The meanings of some compartments are nonobvious, so to clarify, we are defining:
- Susceptible (S) = has never been infected, and has no immunity to the pathogen
- Exposed (E) = has been exposed to the pathogen and an infection has begun, but the person cannot yet infect others
- Infectious (I) = able to transmit the pathogen to a susceptible individual, may or may not have symptoms
- Recovered (R) = the disease has progressed to no longer be infectious. The person may or may not still have symptoms, or could even be dead.
- Pre-hospitalization (P) = a tracking compartment of infectious (I) people who are going to have more severe symptoms
- Hospitalized (H) = attending an emergency room because of the pathogen, and possibly being tested through ThreatNet. Cannot infect others due to physical isolation.
- Threat-net tested positive (T) = When people enter H they are tested, and some fraction of them will test positive, these are recorded in T.

Using dashed lines to represent probabilities, and solid lines to represent rates, the transition graph is given by:

![image](https://github.com/Isabel050/Isabel/assets/114768931/bc92f01e-979a-49e7-9579-913a750822c9)

To deploy the Shiny app, use the code:

```{r}
rsconnect::deployApp(
  appDir   = ".",
  account  = "biosurveillance",
  server   = "shinyapps.io",
  appFiles = c(
    "Oscar_shiny.rmd",                      # Your main app file
    "results.RData",                        # Data file
    "Stochastic.R",                         # Helper R script
    "params.csv", "Hospital Visitors.csv",  # Parameters
    "Documentation.RMD"                     # Documentation
  ),
  appName = "MBN_app"
)
```
