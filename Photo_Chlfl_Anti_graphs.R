# Create graphs for relevant parameters
# based on data analysed in script "Antioxidants_Chlfl_SLA_analysis.R"

p <- ggplot(df.graph[df.graph$Organ == "Leaf", ], 
            aes(x = Date, y = Photo))
        p <- p + stat_summary(aes(colour   = CO2_treatment,
                                  shape    = CO2_treatment,
                                  linetype = is_growth_CO2), 
                                  fun.data = mean_sdl, mult = 1)
        p <- p + stat_summary(aes(colour   = CO2_treatment,
                                  shape    = CO2_treatment,
                                  linetype = is_growth_CO2), 
                                  fun.data = mean_sdl, mult = 1,
                                  geom = "line")

        p <- p + facet_grid(Cultivar ~ Organ)
        p <- p + theme_bw()
p

# boxplots
p <- ggplot(df.graph[df.graph$Organ == "Leaf", ], 
            aes(x = as.factor(Date), y = Photo))
        p <- p + theme_bw()
        p <- p + geom_boxplot(aes(fill     = CO2_treatment,
                                  linetype = is_growth_CO2))
        p <- p + scale_fill_manual(label.CO2, values = c("white", "grey"))
        p <- p + scale_linetype("Measured at")
        p <- p + scale_x_discrete(labels = NULL)
        p <- p + labs(y = expression(Assimilation~rate~"["~mu*mol~CO[2]*m^-2*s^-1~"]"),
                      x = "")
        p <- p + facet_grid(. ~ Cultivar)
        p <- p + theme(legend.position = "none")
p
photo_plot <- p


p <- ggplot(df.graph[df.graph$Organ == "Leaf", ], 
            aes(x = as.factor(Date), y = Cond))
        p <- p + geom_boxplot(aes(fill     = CO2_treatment,
                                  linetype = is_growth_CO2))
        p <- p + scale_fill_manual(label.CO2, values = c("white", "grey"))
        p <- p + scale_linetype("Measured at")
        p <- p + scale_x_discrete(labels = NULL)
        p <- p + labs(y = expression(Stomatal~conductance~g[s]~"["~mol~H[2]*O~m^-2*s^-1~"]"),
                      x = "")
        p <- p + facet_grid(. ~ Cultivar)
        p <- p + theme_bw()
        p <- p + theme(legend.position = c(0.93, 0.66), legend.box.just = "left")
p
cond_plot <- p

p <- ggplot(df.graph[df.graph$Organ == "Leaf", ], 
            aes(x = as.factor(Date), y = Ci))
        p <- p + geom_boxplot(aes(fill     = CO2_treatment,
                                  linetype = is_growth_CO2))
        p <- p + scale_fill_manual(label.CO2, values = c("white", "grey"))
        p <- p + scale_linetype("Measured at")
        p <- p + labs(y = expression(Internal~CO[2]~concentration~c[i]~"["~mu*mol~CO[2]~mol^-1~"]"),
                      x = "Sampling date")
        p <- p + facet_grid(. ~ Cultivar)
        p <- p + theme_bw()
        p <- p + theme(legend.position = "none")
p
ci_plot <- p

# Arrange Fig. 1
grid.arrange(photo_plot, cond_plot, ci_plot, ncol = 1)

pdf(file = "Fig1_gas_exchange_leaves_only.pdf", width = 18, height = 12)
        grid.arrange(photo_plot, cond_plot, ci_plot, ncol = 1)
dev.off()

# for Fig. 2: PhiPS2, 1-qP, and Fv'Fm'
p <- ggplot(df.graph[df.graph$Organ == "Leaf", ], 
            aes(x = as.factor(Date), y = PhiPS2))
        p <- p + geom_boxplot(aes(fill     = CO2_treatment,
                                  linetype = is_growth_CO2))
        p <- p + scale_fill_manual(label.CO2, values = c("white", "grey"))
        p <- p + scale_linetype("Measured at")
        p <- p + scale_y_continuous(limits = c(0, 0.23))
        p <- p + scale_x_discrete(labels = c("Oct 06", "Oct 28", "Nov 08", "Nov 19", "Nov 30", "Dec 9"))
        p <- p + labs(y = expression(phi~PS[2]),
                      x = "Sampling date")
        p <- p + facet_grid(Organ ~ Cultivar)
        p <- p + theme_bw()
        p <- p + theme(legend.position = "none")
p
phips2_leaf_plot <- p

p <- ggplot(df.graph[df.graph$Organ == "Stem", ], 
            aes(x = as.factor(Date), y = PhiPS2))
        p <- p + geom_boxplot(aes(fill     = CO2_treatment,
                                  linetype = is_growth_CO2))
        p <- p + scale_fill_manual(label.CO2, values = c("white", "grey"))
        p <- p + scale_linetype("Measured at")
        p <- p + scale_y_continuous(limits = c(0, 0.23))
        p <- p + scale_x_discrete(labels = c("Oct 06", "Oct 28", "Nov 08", "Nov 19", "Nov 30", "Dec 9"))
        p <- p + labs(y = expression(phi~PS[2]),
                      x = "Sampling date")
        p <- p + facet_grid(Organ ~ Cultivar)
        p <- p + theme_bw()
        p <- p + theme(legend.position = "none")
p
phips2_stem_plot <- p

p <- ggplot(df.graph[df.graph$Organ == "Awn", ], 
            aes(x = as.factor(Date), y = PhiPS2))
        p <- p + geom_boxplot(aes(fill     = CO2_treatment,
                                  linetype = is_growth_CO2))
        p <- p + scale_fill_manual(label.CO2, values = c("white", "grey"))
        p <- p + scale_linetype("Measured at")
        p <- p + scale_y_continuous(limits = c(0, 0.23))
        p <- p + scale_x_discrete(labels = c("Oct 28", "Nov 08", "Nov 19", "Nov 30"))
        p <- p + labs(y = expression(phi~PS[2]),
                      x = "Sampling date")
        p <- p + facet_grid(Organ ~ Cultivar)
        p <- p + theme_bw()
        p <- p + theme(legend.position = "none")
p
phips2_awn_plot <- p

p <- ggplot(df.graph[df.graph$Organ == "Leaf", ], 
            aes(x = as.factor(Date), y = 1 - qP))
        p <- p + geom_boxplot(aes(fill     = CO2_treatment,
                                  linetype = is_growth_CO2))
        p <- p + scale_fill_manual(label.CO2, values = c("white", "grey"))
        p <- p + scale_linetype("Measured at")
        p <- p + scale_y_continuous(limits = c(0, 1))
        p <- p + scale_x_discrete(labels = c("Oct 06", "Oct 28", "Nov 08", "Nov 19", "Nov 30", "Dec 9"))
        p <- p + labs(y = expression(1 - q[P]),
                      x = "Sampling date")
        p <- p + facet_grid(Organ ~ Cultivar)
        p <- p + theme_bw()
        p <- p + theme(legend.position = "none")
p
qP_leaf_plot <- p

p <- ggplot(df.graph[df.graph$Organ == "Stem", ], 
            aes(x = as.factor(Date), y = 1 - qP))
        p <- p + geom_boxplot(aes(fill     = CO2_treatment,
                                  linetype = is_growth_CO2))
        p <- p + scale_fill_manual(label.CO2, values = c("white", "grey"))
        p <- p + scale_linetype("Measured at")
        p <- p + scale_y_continuous(limits = c(0, 1))
        p <- p + scale_x_discrete(labels = c("Oct 06", "Oct 28", "Nov 08", "Nov 19", "Nov 30", "Dec 9"))
        p <- p + labs(y = expression(1 - q[P]),
                      x = "Sampling date")
        p <- p + facet_grid(Organ ~ Cultivar)
        p <- p + theme_bw()
        p <- p + theme(legend.position = "none")
p
qP_stem_plot <- p

p <- ggplot(df.graph[df.graph$Organ == "Awn", ], 
            aes(x = as.factor(Date), y = 1 - qP))
        p <- p + geom_boxplot(aes(fill     = CO2_treatment,
                                  linetype = is_growth_CO2))
        p <- p + scale_fill_manual(label.CO2, values = c("white", "grey"))
        p <- p + scale_linetype("Measured at")
        p <- p + scale_y_continuous(limits = c(0, 1))
        p <- p + scale_x_discrete(labels = c("Oct 28", "Nov 08", "Nov 19", "Nov 30"))
        p <- p + labs(y = expression(1 - q[P]),
                      x = "Sampling date")
        p <- p + facet_grid(Organ ~ Cultivar)
        p <- p + theme_bw()
        p <- p + theme(legend.position = "none")
p
qP_awn_plot <- p

p <- ggplot(df.graph[df.graph$Organ == "Leaf", ], 
            aes(x = as.factor(Date), y = Fv..Fm.))
        p <- p + geom_boxplot(aes(fill     = CO2_treatment,
                                  linetype = is_growth_CO2))
        p <- p + scale_fill_manual(label.CO2, values = c("white", "grey"))
        p <- p + scale_linetype("Measured at")
        p <- p + scale_y_continuous(limits = c(0, 0.6))
        p <- p + scale_x_discrete(labels = c("Oct 06", "Oct 28", "Nov 08", "Nov 19", "Nov 30", "Dec 9"))
        p <- p + labs(y = expression(F[v]~"'"/F[m]~"'"),
                      x = "Sampling date")
        p <- p + facet_grid(Organ ~ Cultivar)
        p <- p + theme_bw()
        p <- p + theme(legend.position = "none")
p
FvFm_leaf_plot <- p

p <- ggplot(df.graph[df.graph$Organ == "Stem", ], 
            aes(x = as.factor(Date), y = Fv..Fm.))
        p <- p + geom_boxplot(aes(fill     = CO2_treatment,
                                  linetype = is_growth_CO2))
        p <- p + scale_fill_manual(label.CO2, values = c("white", "grey"))
        p <- p + scale_linetype("Measured at")
        p <- p + scale_y_continuous(limits = c(0, 0.6))
        p <- p + scale_x_discrete(labels = c("Oct 06", "Oct 28", "Nov 08", "Nov 19", "Nov 30", "Dec 9"))
        p <- p + labs(y = expression(F[v]~"'"/F[m]~"'"),
                      x = "Sampling date")
        p <- p + facet_grid(Organ ~ Cultivar)
        p <- p + theme_bw()
        p <- p + theme(legend.position = "none")
p
FvFm_stem_plot <- p

p <- ggplot(df.graph[df.graph$Organ == "Awn", ], 
            aes(x = as.factor(Date), y = Fv..Fm.))
        p <- p + geom_boxplot(aes(fill     = CO2_treatment,
                                  linetype = is_growth_CO2))
        p <- p + scale_fill_manual(label.CO2, values = c("white", "grey"))
        p <- p + scale_linetype("Measured at")
        p <- p + scale_y_continuous(limits = c(0, 0.6))
        p <- p + scale_x_discrete(labels = c("Oct 28", "Nov 08", "Nov 19", "Nov 30"))
        p <- p + labs(y = expression(F[v]~"'"/F[m]~"'"),
                      x = "Sampling date")
        p <- p + facet_grid(Organ ~ Cultivar)
        p <- p + theme_bw()
        p <- p + theme(legend.position = "none")
p
FvFm_awn_plot <- p

# Arrange Fig. 1
grid.arrange(phips2_leaf_plot, phips2_stem_plot, phips2_awn_plot, 
             qP_leaf_plot, qP_stem_plot, qP_awn_plot,
             FvFm_leaf_plot, FvFm_stem_plot, FvFm_awn_plot,
             ncol = 3, nrow = 3)

pdf(file = "Fig2_chl_fl_leaf_stem_awn.pdf", width = 21, height = 14)
        grid.arrange(phips2_leaf_plot, phips2_stem_plot, phips2_awn_plot, 
             qP_leaf_plot, qP_stem_plot, qP_awn_plot,
             FvFm_leaf_plot, FvFm_stem_plot, FvFm_awn_plot,
             ncol = 3, nrow = 3)
dev.off()

# Fig 3: Antioxidants
p <- ggplot(df.graph[df.graph$Organ == "Leaf", ], 
            aes(x = as.factor(Date), y = ASC_umol_per_g))
        p <- p + geom_boxplot(aes(fill     = CO2_treatment,
                                  linetype = is_growth_CO2))
        p <- p + scale_fill_manual(label.CO2, values = c("white", "grey"))
        p <- p + scale_linetype("Measured at")
        p <- p + scale_y_continuous(limits = c(0, 44))
        p <- p + scale_x_discrete(labels = c("Oct 06", "Oct 28", "Nov 08", "Nov 19", "Nov 30", "Dec 9"))
        p <- p + labs(y = expression(Ascorbat~(ASC)~"["~mu*mol~g^-1~"]"),
                      x = "Sampling date")
        p <- p + facet_grid(Organ ~ Cultivar)
        p <- p + theme_bw()
        p <- p + theme(legend.position = "none")
p
asc_leaf_plot <- p

p <- ggplot(df.graph[df.graph$Organ == "Stem", ], 
            aes(x = as.factor(Date), y = ASC_umol_per_g))
        p <- p + geom_boxplot(aes(fill     = CO2_treatment,
                                  linetype = is_growth_CO2))
        p <- p + scale_fill_manual(label.CO2, values = c("white", "grey"))
        p <- p + scale_linetype("Measured at")
        p <- p + scale_y_continuous(limits = c(0, 44))
        p <- p + scale_x_discrete(labels = c("Oct 06", "Oct 28", "Nov 08", "Nov 19", "Nov 30", "Dec 9"))
        p <- p + labs(y = expression(Ascorbat~(ASC)~"["~mu*mol~g^-1~"]"),
                      x = "Sampling date")
        p <- p + facet_grid(Organ ~ Cultivar)
        p <- p + theme_bw()
        p <- p + theme(legend.position = "none")
p
asc_stem_plot <- p

p <- ggplot(df.graph[df.graph$Organ == "Awn", ], 
            aes(x = as.factor(Date), y = ASC_umol_per_g))
        p <- p + geom_boxplot(aes(fill     = CO2_treatment,
                                  linetype = is_growth_CO2))
        p <- p + scale_fill_manual(label.CO2, values = c("white", "grey"))
        p <- p + scale_linetype("Measured at")
        p <- p + scale_y_continuous(limits = c(0, 44))
        p <- p + scale_x_discrete(labels = c("Oct 28", "Nov 08", "Nov 19", "Nov 30"))
        p <- p + labs(y = expression(Ascorbat~(ASC)~"["~mu*mol~g^-1~"]"),
                      x = "Sampling date")
        p <- p + facet_grid(Organ ~ Cultivar)
        p <- p + theme_bw()
        p <- p + theme(legend.position = "none")
p
asc_awn_plot <- p

p <- ggplot(df.graph[df.graph$Organ == "Leaf", ], 
            aes(x = as.factor(Date), y = GSH_nmol_per_g))
        p <- p + geom_boxplot(aes(fill     = CO2_treatment,
                                  linetype = is_growth_CO2))
        p <- p + scale_fill_manual(label.CO2, values = c("white", "grey"))
        p <- p + scale_linetype("Measured at")
        p <- p + scale_x_discrete(labels = c("Oct 06", "Oct 28", "Nov 08", "Nov 19", "Nov 30", "Dec 9"))
        p <- p + labs(y = expression(Glutathion~(GSC)~"["~nmol~g^-1~"]"),
                      x = "Sampling date")
        p <- p + facet_grid(Organ ~ Cultivar)
        p <- p + theme_bw()
        p <- p + theme(legend.position = "none")
p
gsc_leaf_plot <- p

p <- ggplot(df.graph[df.graph$Organ == "Stem", ], 
            aes(x = as.factor(Date), y = ASC_umol_per_g))
        p <- p + geom_boxplot(aes(fill     = CO2_treatment,
                                  linetype = is_growth_CO2))
        p <- p + scale_fill_manual(label.CO2, values = c("white", "grey"))
        p <- p + scale_linetype("Measured at")
        p <- p + scale_y_continuous(limits = c(0, 20))
        p <- p + scale_x_discrete(labels = c("Oct 06", "Oct 28", "Nov 08", "Nov 19", "Nov 30", "Dec 9"))
        p <- p + labs(y = expression(Glutathion~(GSC)~"["~nmol~g^-1~"]"),
                      x = "Sampling date")
        p <- p + facet_grid(Organ ~ Cultivar)
        p <- p + theme_bw()
        p <- p + theme(legend.position = "none")
p
gsc_stem_plot <- p

p <- ggplot(df.graph[df.graph$Organ == "Awn", ], 
            aes(x = as.factor(Date), y = ASC_umol_per_g))
        p <- p + geom_boxplot(aes(fill     = CO2_treatment,
                                  linetype = is_growth_CO2))
        p <- p + scale_fill_manual(label.CO2, values = c("white", "grey"))
        p <- p + scale_linetype("Measured at")
        p <- p + scale_y_continuous(limits = c(0, 20))
        p <- p + scale_x_discrete(labels = c("Oct 28", "Nov 08", "Nov 19", "Nov 30"))
        p <- p + labs(y = expression(Glutathion~(GSC)~"["~nmol~g^-1~"]"),
                      x = "Sampling date")
        p <- p + facet_grid(Organ ~ Cultivar)
        p <- p + theme_bw()
        p <- p + theme(legend.position = "none")
p
gsc_awn_plot <- p

grid.arrange(asc_leaf_plot, asc_stem_plot, asc_awn_plot,
             gsc_leaf_plot, gsc_stem_plot, gsc_awn_plot,
             ncol = 3, nrow = 2)

pdf(file = "Fig3_antioxidants_leaf_stem_awn.pdf", width = 21, height = 14)
        grid.arrange(asc_leaf_plot, asc_stem_plot, asc_awn_plot,
                     gsc_leaf_plot, gsc_stem_plot, gsc_awn_plot,
             ncol = 3, nrow = 2)
dev.off()

# sample sizes
mis.photo <- ddply(df.graph,
                .(Cultivar, Organ, CO2_treatment, Ring, is_growth_CO2, Date),
                summarise,
                length = length(Photo),
                missing = sum(is.na(Photo)))
