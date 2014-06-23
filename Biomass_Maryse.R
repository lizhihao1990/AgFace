# AR.Dry.wt.area..g.m2.
# for SB lines, requested by Maryse

# based on script "SB_lines_2011_2012_graphs.R"

cols_to_keep <- c(
"Trmt.Order",
"Plot.Order",
"CO2.Irr.Cultivar.DATABASE",
"Trial.ID",
"Year",
"RingID",
"PlotID",
"HalfRing",
"RingPos.",
"RingTrt",
"CO2",
"Irrigation",
"Crop",
"Qplot",
"QHalfRing",
"Qring",
"Bulk_Trait",
"Cultivar",
"Stage",
"Environment",
"my.HalfringID",
"my.Trait",
"Ord.Environment",
"AR.Dry.wt.area..g.m2.")

AR.Dry.Weight <- df[, names(df) %in% cols_to_keep]

README <- c("The data frame AR.Dry.Weight stores the Above root dry weight biomass in g per m2 in the vector 'AR.Dry.wt.area..g.m2.' Data are from Glenn Fitzgerald, received December 2013. Markus Löw, May 2014")

save(list = c("README", "AR.Dry.Weight"), file = "Above_ground_biomass_SB_lines_2011_2012.RData", compress = TRUE)

p <- ggplot(df,
            aes(x = Cultivar, y = AR.Dry.wt.area..g.m2.))
  p <- p + stat_summary(aes(colour = CO2), 
                        fun.data = "my.stderr", #mult = 1,
                        position = position_dodge(width = 0.66), 
                        geom = "linerange")
  p <- p + stat_summary(aes(fill = CO2, shape = CO2), 
                        fun.data = "my.stderr", #mult = 1, 
                        geom = "point",
                        position = position_dodge(width = 0.66))
  p <- p + scale_colour_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("black", "black"))
  p <- p + scale_shape_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c(21, 21))
  p <- p + scale_fill_manual(name = CO2.label, 
                        labels = CO2.treats,
                        values = c("white", "black"))
  p <- p + labs(y = expression("Above root dry weight"~(g~m^-2)))
  p <- p + facet_grid(Stage ~ Ord.Environment, scale = "free_y")
  p <- p + theme_my
  p <- p + theme(legend.position = c(0.38, 0.59))
p
ggsave(file = "Above_root_biomass_SB_2011_2012_free_y_mean_se.pdf", 
       width = my.width, height = my.height, 
       units = "cm")

