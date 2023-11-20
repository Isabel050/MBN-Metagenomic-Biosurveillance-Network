# ThreatNet Israel

The web user interface for this model is available at https://oscar-delaney.shinyapps.io/ThreatNetIsrael/
The associated paper is currently unpublished, but we will link to it when it is up.

File descriptions:
stochastic.R is the bones of the model, defining the SEIR model, detection system, and graphing functionality.
Oscar_shiny.Rmd creates the web interface
Documentation.Rmd is part of the web interface
figures.R creates the tables and figures that appear in the manuscript
Hospital Visitors.csv is data on Israel's health system used in the model
Results.RData is the output of many simulation runs used in the web app, to save computation time

Technical SEIR details:
The meanings of some compartments are nonobvious, so to clarify, we are defining:
Susceptible (S) = has never been infected, and has no immunity to the pathogen
Exposed (E) = has been exposed to the pathogen and an infection has begun, but the person cannot yet infect others
Infectious (I) = able to transmit the pathogen to a susceptible individual, may or may not have symptoms
Infectious with severe disease (I_sev) = same as Infectious, but the person will go on to be hospitalized eventually
Hospitalized (H) = attending an emergency room because of the pathogen, and possibly being tested through ThreatNet. Cannot infect others due to physical isolation.
Recovered (R) = the disease has progressed to no longer be infectious. The person may or may not still have symptoms, or could even be dead.
Recovered with severe disease (R_sev) = same as recovered, but the person will go on to be hospitalized eventually.

The transition graph is given by:
![SEIHR](https://github.com/Isabel050/Isabel/assets/114768931/e118284e-ef69-41f9-925d-8d08df36543a)
[redo this later with a proper diagram and the correct I-->R eqn]

Note in particular that the transition rate from I to R is set at rate x * I such that the average time a person is infectious is the inverse of gamma. We have:
$$\frac{\delta}{\gamma + \theta} + \frac{1-\delta}{x} = \frac{1}{Y}$$
$$\therefore x = \frac{\gamma + \theta}{\frac{1}{Y(1-\delta)} - \frac{\theta}{\gamma(1-\delta)}}$$

