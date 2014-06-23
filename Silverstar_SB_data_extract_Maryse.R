# Data extraction for Maryse, 2014-04-14

# would you mind sending me data for the SB and tillering lines (either the raw or the averages with standard errors) on:
 
# Grain yield, with an estimate of area sampled and plant population (I’ll try to translate this in a per plant basis)
# Dry above-ground biomass at harvest, with number of spikes (again, as long as I know the units) and grain size (1000-grain weight)
# Dry above-ground biomass at anthesis, with leaf area, tiller numbers, spike number and SPAD data if available, and WSC concentration and content (i.e [] x stem weight) if you have it, otherwise I’m happy to write a line of code to calculate it.


setwd("~/AgFace/Plant_Production/Silverstar_tin")
load("Silverstar_tin_environment.RData")

# instead of spikes using heads
paras.Maryse <- c("Yield..g.m2.",           # Yield g/m2
                  "AR.Dry.wt.area..g.m2.",  # Above root biomass (g/m2)
                  "Heads.m2..Quadrat.sample.fresh.plant.no.", # Heads/m2 (Quadrat sample fresh plant no)
                  "Adjusted.SS.1000.Grain.wt..g." # Sub sample 1000 grains weight adjusted for drying
                  "Green.leaf.area.pl..cm2." # Green leaf area/plant (cm2)
                  "SPAD" # Spad is part of the headers of the data set, but no numbers are provided
)
