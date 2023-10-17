# Manual rerun if there are changes: ScenariosRun=FALSE

# !! CRITICAL !! - Changes will not be run by the Rmd file if the relevant cached csv files are not deleted.

# Percentage Coverage is based on scenarios for hospitals.
# These three *MUST* have the same order.
Coverage_names <- c("Only Sheba", "Top 2", "Six Largest", "Ten Largest", "Sixteen Largest")
n_Hosp <- c(1, 2, 6, 10, 16)
Coverage_rates <- c(8, 15, 38, 56, 79)
Hospital_visitors <- read.csv("Hospital Visitors.csv")
Hospital_visitors$pct <- Hospital_visitors$Visitors.2021 / sum(Hospital_visitors$Visitors.2021)

# Thresholds are the number of detections (cases) needed for response.
Thresholds <- c(1, 3, 5)


generate_scenario <- function(t, s, e, threshold, scen_name, case = FALSE) {
  Sim <- list()
  for (i in 0:1001) {
    Sim[[i + 1]] <- ifelse(replicate(n = 10, rbinom(1000, i, s * e * t)) >= threshold, 1, 0)
  }
  Case <- list()
  for (i in 0:1001) {
    Case[[i + 1]] <- apply(X = Sim[[i + 1]], MARGIN = 2, FUN = mean)
  }

  Scen_Frame <- data.frame(Case)
  colnames(Scen_Frame) <- c(0:1001)
  Frame_2 <- melt(Scen_Frame, id.vars = NULL)
  colnames(Frame_2) <- c("Case", paste(scen_name, "a", sep = "_"))

  # Find the mean per number of cases
  Frame_3 <- aggregate(Frame_2[, 2], by = list(Frame_2[, 1]), FUN = mean)
  colnames(Frame_3) <- c("Case", paste(scen_name, "b", sep = "_"))

  if (case) {
    return(list(scen_name, Frame_2, Frame_3))
  } else {
    Frame_2$Case <- NULL
    Frame_3$Case <- NULL
    return(list(scen_name, Frame_2, Frame_3))
  }
}



# Check if they are already run:
if (!ScenariosRun) {
  Percentages <- Coverage_rates
  Scenarios <- list(Percentages, Thresholds)

  Run_cases <- function() {
    case <- TRUE
    for (i in Thresholds) {
      for (j in Percentages) {
        scen_name <- paste("Scen", j, i, sep = "_")
        # It turns out that these names are used to order the graph.
        print(scen_name)
        S <- generate_scenario(
          t = j / 100, s = 0.5, e = 0.25, threshold = i,
          scen_name = scen_name, case = case
        )

        if (case) {
          Master_Frame_a <- data.frame(S[[2]])
          Master_Frame_b <- data.frame(S[[3]])
          case <- FALSE
        } else {
          Master_Frame_a <- cbind(Master_Frame_a, S[[2]])
          Master_Frame_b <- cbind(Master_Frame_b, S[[3]])
        }
      }
    }

    Master_Frame_a$Cases_continuous <- as.numeric(as.character(Master_Frame_a$Case))
    Master_Frame_b$Cases_continuous_b <- as.numeric(as.character(Master_Frame_b$Case))

    return(list(Master_Frame_a, Master_Frame_b))
  }

  output <- Run_cases()
  Master_Frame_a <- output[[1]]
  Master_Frame_b <- output[[2]]


  # NEW:Pivot Master Frame 1a longer and subset to only include detection threshold=1
  Master_Frame_c <- Master_Frame_a %>%
    pivot_longer(!c("Case", "Cases_continuous"), names_to = "Coverage", values_to = "Cumulative_probability")


  write.csv(Master_Frame_a, "Master_Frame_a.csv")
}

Master_Frame_a <- read.csv("Master_Frame_a.csv")



# FullDataSet=FALSE
FullDataSet <- TRUE
# Create the full set of possible percentages:
if (FullDataSet & (!ScenariosRun)) {
  Percentages <- c(1:100)
  Scenarios <- list(Percentages, c(1:5))

  Run_cases <- function() {
    case <- TRUE
    for (i in Thresholds) {
      for (j in Percentages) {
        scen_name <- paste("Scen", j, i, sep = "_")
        # These names are later split out for the dataset.
        print(scen_name)
        S <- generate_scenario(
          t = j / 100, s = 0.5, e = 0.25, threshold = i,
          scen_name = scen_name, case = case
        )

        if (case) {
          Master_Frame_a <- data.frame(S[[2]])
          case <- FALSE
        } else {
          Master_Frame_a <- cbind(Master_Frame_a, S[[2]])
        }
      }
    }

    Master_Frame_a$Cases_continuous <- as.numeric(as.character(Master_Frame_a$Case))


    return(Master_Frame_a)
  }

  Master_Frame_Full <- Run_cases()

  # Map from Top N Hospitals to Percentages
  Min_Hospitals_for_Pct <- data.frame(n_Hosp = c(1:length(Hospital_visitors$pct)), HospPCT = cumsum(Hospital_visitors$pct))[1:(length(Hospital_visitors$pct) - 1), ]
  # Remove last row because it's the total, and we can't have 200% coverage...
  Min_Hospitals_for_Pct$HospPCT <- round(100 * Min_Hospitals_for_Pct$HospPCT, 0)
  # This requires that the cost script be run first, which it is when called from the Rmd file.
  Cost_Anynumber_map <- data.frame(n_Hosp = Min_Hospitals_for_Pct$n_Hosp, Overall_10y_Cost = Overall_10y_Cost_AnyNumber[1:length(Min_Hospitals_for_Pct$n_Hosp)])

  Full_Cost_Map <- merge(Min_Hospitals_for_Pct, Cost_Anynumber_map)

  write.csv(Full_Cost_Map, "Full_Cost_Map.csv")
  write.csv(Master_Frame_Full, "Master_Frame_Full.csv")
}
Master_Frame_Full <- read.csv("Master_Frame_Full.csv")

ScenariosRun <- TRUE

# Actually, we don't need the whole thing, just values that can be mapped to.
# (Unless/until we allow selecting arbitrary lists of hospitals.)
Full_Cost_Map <- read.csv("Full_Cost_Map.csv")
