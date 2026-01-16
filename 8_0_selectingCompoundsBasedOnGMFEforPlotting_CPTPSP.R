




data_selectedCpt_all_CPTPSP <- readRDS("datasets/data_selectedCpt_all_CPTPSP.RDS")
nca_selectedCpt_all_CPTPSP <- readRDS("datasets/nca_selectedCpt_all_CPTPSP.RDS")
pure_selectedCpt_all_CPTPSP <- readRDS("datasets/pure_selectedCpt_all_CPTPSP.RDS")
cmp3_selectedCpt_all_CPTPSP <- readRDS("datasets/cmp3_selectedCpt_all_CPTPSP.RDS")
PKmodel_selectedCpt_all_CPTPSP <- readRDS("datasets/PKmodel_selectedCpt_all_CPTPSP.RDS")



selectedCompoundsAll_XYZ <- c("Compound A","Compound B","Compound C","Compound D","Compound E","Compound F")


ggplot(data_selectedCpt_all_CPTPSP, aes(x = TIME, color = "PBPK-ML")) +
  geom_ribbon(aes(ymin = min_conc_PBPK, ymax = max_conc_PBPK), fill = "grey", alpha = 0.5) +
  geom_point(data = data_selectedCpt_all_CPTPSP,aes(x=TIME, y= DV_NM , color = "Data") ) +
  geom_line(data = nca_selectedCpt_all_CPTPSP,aes(x=time, y= c1 , color = "ML-NCA") ) +
  geom_line(data = pure_selectedCpt_all_CPTPSP,aes(x=TIME, y= conc_Prediction , color = "PURE-ML") ) +
  geom_line(data = cmp3_selectedCpt_all_CPTPSP,aes(x=time, y= prediction , color = "3CMT-PINN"), size = 1  ) +
  geom_line(data = PKmodel_selectedCpt_all_CPTPSP,aes(x=time, y= c1 , color = "3CMT-ML") ) +
  scale_y_log10(breaks = c(1,10,100,1000,10000,100000),
                #labels = scales::trans_format("log10", scales::math_format(10^.x))
                labels = c(1,10,100,1000,10000,100000)) +
  ggplot2::ylab("Plasma concentration [nM]") +
  ggplot2::xlab("Time [hr]") +
  #ggplot2::facet_wrap(~factor(SAMPLING + MO_PREFERRED_ID, levels=selectedCompounds)) +
  ggplot2::facet_wrap(~factor( SAMPLING, scales ="free_x" , levels=selectedCompoundsAll_XYZ)) +
  ggplot2::facet_wrap(~factor( Compound_XYZ, levels=selectedCompoundsAll_XYZ)) +
  theme(strip.text = element_blank()) + # Hide the facet labels
  coord_cartesian(ylim = c(1, NA)) +
  labs(color = "Models")+
  theme(axis.title.y = element_text(size = rel(1.9) ),
        axis.title.x = element_text(size = rel(1.9) ),
        axis.text.x = element_text(face="bold", size=7),
        axis.text.y = element_text(face="bold", size=7))#+

# Save plot
ggsave( "Rat_plasmaConc_allPKmodels_vs_Observation_predicted_0h_8h_24h.tiff" ,
        plot = p,
        device = "tiff",
        #width = 7,
        #height = 5,
        #units = "in",
        #width = 17,
        #height = 12,
        #units = "cm",
        dpi = 300)#,
#compression = "lzw")


