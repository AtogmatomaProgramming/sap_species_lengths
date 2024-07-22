#' Auxiliar script to operate with the measure of the lengths 
#' and the species' lengths


#### Import libraries ####

#library(ggplot2)

#### a) Import working datasets ####

#Define the data directory source

data_source <- paste0(getwd(), "/data/")

result_directory <- "results"

#Import the work files

measure_ranges <- readRDS(paste0(data_source,
                                 "rim_lengths_2019_2023.rds"))

fishing_environment <- read.table(file = paste0(data_source,"caladero.csv"), 
                                  head = TRUE, 
                                  sep = ";",
                                  fill = TRUE, 
                                  colClasses = c("character", "factor", "character"))

#### b) Add the variable "CALADERO" to the "measure_ranges" df ####

measure_ranges <- merge(measure_ranges, 
                        fishing_environment[, c("CALADERO", "COD_ORIGEN")], 
                        by = "COD_ORIGEN", 
                        all.x = TRUE)


#### c) Filter according to "PROCEDENCIA" and "CALADERO" ####

# Fishing grounds name #

fishing_grounds <- unique(measure_ranges$CALADERO)

fishing_grounds <- fishing_grounds[!is.na(fishing_grounds)]


#### d) Function to plot the distribution of tails ####


plotter_function <- function(fishing_ground, 
                             measure_ranges, 
                             result_directory, 
                             ver = "alfa",
                             discarded_sizes = NA){
  
  measure_ranges <- as.data.frame(measure_ranges)
  
  fishing_ground <- as.character(as.vector(fishing_ground))
  
  measure_ranges_filtered <- measure_ranges[measure_ranges$CALADERO == fishing_ground & 
                                              measure_ranges$PROCEDENCIA == "IEO", 
                                            c("COD_ESP_CAT", "ESP_CAT", "SEXO", "TALLA")]
  
  measure_ranges_filtered <- measure_ranges_filtered[!is.na(measure_ranges_filtered$TALLA), ]

  measure_ranges_filtered <- measure_ranges_filtered |>
    dplyr::group_by(ESP_CAT, SEXO) |>
    dplyr::mutate(TOTAL_TALLAS = dplyr::n_distinct(TALLA))
  
  measure_ranges_filtered_one_measure <- measure_ranges_filtered[measure_ranges_filtered$TOTAL_TALLAS == 1, 
                                                                    c("COD_ESP_CAT", "TOTAL_TALLAS")]
  
  one_measure_specie <- unique(as.character(measure_ranges_filtered_one_measure$COD_ESP_CAT))
  
  measure_ranges_filtered <- measure_ranges_filtered[!(measure_ranges_filtered$COD_ESP_CAT %in% one_measure_specie), 
                                                     c("COD_ESP_CAT", "ESP_CAT", "SEXO", "TALLA")]
  
  measure_ranges_filtered$ESP_CAT <- paste0(measure_ranges_filtered$ESP_CAT, 
                                            "-", 
                                            measure_ranges_filtered$SEXO)
  
  if (is.data.frame(discarded_sizes) && nrow(discarded_sizes) > 0) {
    
    discarded_sizes[discarded_sizes$CALADERO==fishing_ground, ]
    
    discarded_sizes$CALADERO <- NA

    measure_ranges_filtered$ESP_CAT <- paste0(measure_ranges_filtered$ESP_CAT, "_", measure_ranges_filtered$TALLA)
    
    discarded_sizes$ESP_CAT <- paste0(discarded_sizes$ESP_CAT, "_", discarded_sizes$TALLA)
    
    measure_ranges_filtered <- measure_ranges_filtered[!(measure_ranges_filtered$ESP_CAT %in% discarded_sizes$ESP_CAT), ]
      
    measure_ranges_filtered <- measure_ranges_filtered |> tidyr::separate(col = ESP_CAT,
                                                                           into = c("ESP_CAT", "TALLA"),
                                                                           sep = "_")
    
    measure_ranges_filtered$TALLA <- as.numeric(measure_ranges_filtered$TALLA)
    
  }


  plot <- ggplot2::ggplot(data = measure_ranges_filtered, ggplot2::aes(x = ESP_CAT, y = TALLA)) +
    ggplot2::stat_boxplot(geom = "errorbar", width = 0.5) +
    ggplot2::geom_boxplot(fill = "#4271AE", outlier.colour = "red", alpha = 0.9) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggplot2::ggtitle(paste0("Distribución tallas caladero ", fishing_ground))
  
  p <- plotly::ggplotly(plot)
  
  htmlwidgets::saveWidget(
    p,
    paste0(
      getwd(),
      "/",
      result_directory,
      "/grafico_interactivo_tallas_",
      ver,
      "_",
      fishing_ground,
      ".html"
    )
  )
  
}


#### e) Plot the bloxplots for measure_ranges ####

lista_fishing_grounds <- as.list(fishing_grounds)

lapply(lista_fishing_grounds, 
       plotter_function, 
       measure_ranges, 
       result_directory)

#### f) Delete the sizes that are outer the whiskers ####

# Create the object with the specie and the sizes

##### Fishing Ground: AC #####

specie_sargus <- data.frame(ESP_CAT = c("Diplodus sargus-U"), 
                            TALLA = c(40))

specie_fullonica <- data.frame(ESP_CAT = c("Leucoraja fullonica-M"), 
                               TALLA = c(43))

specie_naevus_m <- data.frame(ESP_CAT = c("Leucoraja naevus-M"), 
                              TALLA = c(82))

specie_naevus_h <- data.frame(ESP_CAT = c("Leucoraja naevus-H"), 
                              TALLA = c(35:37, 82))

specie_loligo_spp <- data.frame(ESP_CAT = c("Loligo spp-U"), 
                                TALLA = c(38, 46))

specie_loligo_vulgaris <- data.frame(ESP_CAT = c("Loligo vulgaris-U"), 
                                     TALLA = c(46))

specie_lepturus <- data.frame(ESP_CAT = c("Trichiurus lepturus-U"), 
                              TALLA = c(132))

specie_lepturus <- data.frame(ESP_CAT = c("Melanogrammus aeglefinus-U"), 
                              TALLA = c(98))

discarded_sizes_ac <- rbind(specie_sargus, specie_fullonica, specie_naevus_m,
                            specie_naevus_h, specie_loligo_spp, 
                            specie_loligo_vulgaris, specie_lepturus)


discarded_sizes_ac$CALADERO <- "AC"


##### Fishing Ground: CN #####


specie_carbo_cn <- data.frame(ESP_CAT = c("Aphanopus carbo-U"), 
                              TALLA = c(51, 161, 185))

specie_capriscus_cn <- data.frame(ESP_CAT = c("Balistes capriscus-U"), 
                                  TALLA = c(83, 89))

specie_cancer_cn <- data.frame(ESP_CAT = c("Cancer pagurus-U"), 
                               TALLA = c(1.5))

specie_dentex_cn <- data.frame(ESP_CAT = c("Dentex dentex-U"), 
                               TALLA = c(88))

specie_labrax_cn <- data.frame(ESP_CAT = c("Dicentrarchus labrax-U"), 
                               TALLA = c(1))

specie_atlanticus_cn <- data.frame(ESP_CAT = c("Hoplostethus atlanticus-M"), 
                                   TALLA = c(42))
                        #Revisar

specie_octopus_cn <- data.frame(ESP_CAT = c("Octopus vulgaris-U"), 
                                TALLA = c(1.6, 56))
                        #Averiguar donde fue el muestreo de la talla 1.6

specie_scophthalmus_cn <- data.frame(ESP_CAT = c("Scophthalmus maximus-U"),
                                     TALLA = c(5))

specie_loligo_cn <- data.frame(ESP_CAT = c("Loligo spp-U"), 
                               TALLA = c(62))

specie_molva_cn <- data.frame(ESP_CAT = c("Molva molva-U"), 
                                           TALLA = c(132, 137))

specie_polyprion_cn <- data.frame(ESP_CAT = c("Polyprion americanus-U"), 
                                  TALLA = c(87))

specie_brachyura_cn <- data.frame(ESP_CAT = c("Raja brachyura-M"), 
                                  TALLA = c(115))

specie_miraletus_h_cn <- data.frame(ESP_CAT = c("Raja miraletus-H"), 
                                    TALLA = c(59, 68))

specie_miraletus_m_cn <- data.frame(ESP_CAT = c("Raja miraletus-M"), 
                                    TALLA = c(67, 72, 81))

specie_rhombosepion_cn <- data.frame(ESP_CAT = c("Rhombosepion orbignyanum-U"),
                                     TALLA = c(18))

specie_scombrus_cn <- data.frame(ESP_CAT = c("Scomber scombrus-U"),
                                 TALLA = c(58))

specie_scorpaena_cn <- data.frame(ESP_CAT = c("Scorpaena porcus-U"), 
                                  TALLA = c(45:48))

specie_canicula_h_cn <- data.frame(ESP_CAT = c("Scyliorhinus canicula-H"), 
                                   TALLA = c(81:82))

specie_canicula_m_cn <- data.frame(ESP_CAT = c("Scyliorhinus canicula-M"), 
                                   TALLA = c(81))

specie_canicula_u_cn <- data.frame(ESP_CAT = c("Scyliorhinus canicula-U"), 
                                   TALLA = c(40, 78))

specie_tunnus_cn <- data.frame(ESP_CAT = c("Thunnus alalunga-U"), 
                               TALLA = c(42))

specie_sagittatus_cn <- data.frame(ESP_CAT = c("Todarodes sagittatus-U"), 
                                   TALLA = c(54, 62))

specie_canariensis_cn <- data.frame(ESP_CAT = c("Umbrina canariensis-U"), 
                                    TALLA = c(48))

specie_xiphius_cn <- data.frame(ESP_CAT = c("Xiphias gladius-U"), 
                                TALLA = c(25, 274, 298, 300))

specie_zeus_cn <- data.frame(ESP_CAT = c("Zeus faber-U"), 
                             TALLA = c(80, 83))


discarded_sizes_cn <- rbind(specie_carbo_cn, specie_capriscus_cn, specie_cancer_cn, 
                            specie_dentex_cn, specie_labrax_cn, specie_atlanticus_cn, 
                            specie_octopus_cn, specie_scophthalmus_cn, specie_loligo_cn, 
                            specie_molva_cn, specie_polyprion_cn, specie_brachyura_cn, 
                            specie_miraletus_h_cn, specie_miraletus_m_cn, 
                            specie_rhombosepion_cn, specie_scombrus_cn, 
                            specie_scorpaena_cn, specie_canicula_h_cn, 
                            specie_canicula_m_cn, specie_canicula_u_cn, 
                            specie_tunnus_cn, specie_sagittatus_cn, 
                            specie_canariensis_cn, specie_xiphius_cn, 
                            specie_zeus_cn)


discarded_sizes_cn$CALADERO <- "CN"


##### Fishing Ground: GC #####


specie_balistes_gc <- data.frame(ESP_CAT = c("Balistes capriscus-U"), 
                                 TALLA = c(9))

specie_boops_gc <- data.frame(ESP_CAT = c("Boops boops-U"), 
                              TALLA = c(41))

specie_lucerna_gc <- data.frame(ESP_CAT = c("Chelidonichthys lucerna-U"), 
                                TALLA = c(37, 44:45))

specie_obscurus_gc <- data.frame(ESP_CAT = c("Chelidonichthys obscurus-U"), 
                                 TALLA = c(36))

specie_labrosus_gc <- data.frame(ESP_CAT = c("Chelon labrosus-U"), 
                                 TALLA = c(52, 60))

specie_linguata_gc <- data.frame(ESP_CAT = c("Citharus linguatula-U"), 
                                 TALLA = c(37, 40, 43))

specie_canariensis_gc <- data.frame(ESP_CAT = c("Dentex canariensis-U"),
                                    TALLA = c(12, 48))

specie_labrax_gc <- data.frame(ESP_CAT = c("Dicentrarchus labrax-U"),
                               TALLA = c(68, 74))

specie_puntazzo_gc <- data.frame(ESP_CAT = c("Diplodus puntazzo-U"),
                                 TALLA = c(17))

specie_didactylus_gc <- data.frame(ESP_CAT = c("Halobatrachus didactylus-U"),
                                   TALLA = c(16))

specie_barbatus_gc <- data.frame(ESP_CAT = c("Mullus barbatus-U"),
                                 TALLA = c(39))

specie_encrasicolus_gc <- data.frame(ESP_CAT = c("Engraulis encrasicolus-U"),
                                     TALLA = c(0.95, 1.00, 1.05, 
                                               1.10, 1.15, 1.20, 
                                               1.25))

specie_caudatus_gc <- data.frame(ESP_CAT = c("Lepidopus caudatus-U"),
                                 TALLA = c(60))

specie_surmuletus_gc <- data.frame(ESP_CAT = c("Mullus surmuletus-U"),
                                   TALLA = c(2, 3))

specie_umbra_gc <- data.frame(ESP_CAT = c("Sciaena umbra-U"),
                              TALLA = c(67))

specie_minutus_gc <- data.frame(ESP_CAT = c("Trisopterus luscus-U"), 
                                TALLA = c(51))

specie_auriga_gc <- data.frame(ESP_CAT = c("Pagrus auriga-U"),
                               TALLA = c(6:8))

specie_pagrus_gc <- data.frame(ESP_CAT = c("Pagrus pagrus-U"), 
                               TALLA = c(75, 84, 90))

specie_punctatus_gc <- data.frame(ESP_CAT = c("Dicentrarchus punctatus-U"),
                                  TALLA = c(49:50))

specie_blennoides_gc <- data.frame(ESP_CAT = c("Phycis blennoides-U"),
                                   TALLA = c(54, 58))

specie_physis_gc <- data.frame(ESP_CAT = c("Phycis phycis-U"),
                               TALLA = c(22))

specie_mediterraneus_gc <- data.frame(ESP_CAT = c("Plectorhincus mediterraneus-U"),
                                      TALLA = c(61, 64, 65))

specie_incisus_gc <- data.frame(ESP_CAT = c("Pomadasys incisus-U"),
                                TALLA = c(38, 39))

specie_saltatrix_gc <- data.frame(ESP_CAT = c("Pomatomus saltatrix-U"),
                                  TALLA = c(72, 73))

specie_undulata_m_gc <- data.frame(ESP_CAT = c("Raja undulata-M"),
                                   TALLA = c(50))

specie_sarda_gc <- data.frame(ESP_CAT = c("Sarda sarda-U"),
                              TALLA = c(65, 66))

specie_scombrus_gc <- data.frame(ESP_CAT = c("Scomber scombrus-U"),
                                 TALLA = c(12))

specie_rhombus_gc <- data.frame(ESP_CAT = c("Scophthalmus rhombus-U"),
                                TALLA = c(16, 53))

specie_notata_gc <- data.frame(ESP_CAT = c("Scorpaena notata-U"),
                               TALLA = c(23))

specie_canicula_m_gc <- data.frame(ESP_CAT = c("Scyliorhinus canicula-M"),
                                   TALLA = c(25, 28))

specie_solea_gc <- data.frame(ESP_CAT = c("Solea senegalensis-M"),
                              TALLA = c(53))

specie_cantharus_gc <- data.frame(ESP_CAT = c("Spondyliosoma cantharus-U"),
                                  TALLA = c(5:7))

specie_kleini_gc <- data.frame(ESP_CAT = c("Synapturichthys kleinii-U"),
                               TALLA = c(18, 47))

specie_trachinus_gc <- data.frame(ESP_CAT = c("Trachinus draco-U"),
                                  TALLA = c(38))

specie_ronchus_gc <- data.frame(ESP_CAT = c("Umbrina ronchus-U"),
                                TALLA = c(46))

specie_scaber_gc <- data.frame(ESP_CAT = c("Uranoscopus scaber-U"),
                               TALLA = c(36))

specie_octopus_gc <- data.frame(ESP_CAT = c("Octopus vulgaris-U"), 
                                TALLA = c(3))
                      #Averiguar donde fue el muestreo de la talla 3


discarded_sizes_gc <- rbind(specie_caudatus_gc, specie_surmuletus_gc, 
                            specie_umbra_gc, specie_minutus_gc, 
                            specie_encrasicolus_gc, specie_auriga_gc)


discarded_sizes_gc$CALADERO <- "GC"


# Final dataframe. Above's one fusion.

bersion <- "betha"

discarded_sizes <- rbind(discarded_sizes_ac, 
                    discarded_sizes_cn, 
                    discarded_sizes_gc)


#### g) Plot again the sizes after of being filtered ####

lapply(lista_fishing_grounds, function(fg) {
     plotter_function(fg, measure_ranges, result_directory, "betha", discarded_sizes)
   })



#### Annex ####

##### Other sizes AC #####

# specie_decadactylus <- data.frame(ESP_CAT = c("Beryx decadactylus-U"),
#                                  TALLA = c(65, 67))

# specie_splendens <- data.frame(ESP_CAT = c("Beryx splendens-U"),
#                                  TALLA = c(56:57, 61))

#specie_cuculus <- data.frame(ESP_CAT = c("Chelidonichthys cuculus-U"),
#                                  TALLA = c(49, 53))

#specie_lucerna <- data.frame(ESP_CAT = c("Chelidonichthys lucerna-U"),
#                             TALLA = c(75))

#specie_gunardus <- data.frame(ESP_CAT = c("Eutrigla gurnardus-U"),
#                              TALLA = c(51))

#specie_cynoglosus <- data.frame(ESP_CAT = c("Glyptocephalus cynoglossus-U"),
#                              TALLA = c(53))

#specie_conger <- data.frame(ESP_CAT = c("Conger conger-U"), 
#                            TALLA = c(200, 206, 208))


#specie_lophius_budegassa <- data.frame(ESP_CAT = c("Lophius budegassa-U"), 
#                                       TALLA = c(97:99, 101:102, 107, 111,
#                                                 122, 128, 139, 157, 164))

#specie_lophius_piscatorius <- data.frame(ESP_CAT = c("Lophius piscatorius-U"), 
#                                       TALLA = c(137:139, 141, 144, 147, 
#                                                 150, 153, 158, 170))

#specie_merluccius <- data.frame(ESP_CAT = c("Merluccius merluccius-U"), 
#                                         TALLA = c(116, 123))

#specie_macrophthalma <- data.frame(ESP_CAT = c("Molva macrophthalma-U"), 
#                                        TALLA = c(54, 126, 130))

#specie_molva <- data.frame(ESP_CAT = c("Molva molva-U"), 
#                                           TALLA = c(151, 153, 182, 197))

#specie_moro <- data.frame(ESP_CAT = c("Mora moro-U"), 
#                                   TALLA = c(41:46))

#specie_surmulletus <- data.frame(ESP_CAT = c("Mullus surmuletus-U"), 
#                                 TALLA = c(47, 49))

#specie_flesus <- data.frame(ESP_CAT = c("Platichthys flesus-U"), 
#                                 TALLA = c(47))

#specie_pollachius <- data.frame(ESP_CAT = c("Pollachius pollachius-U"), 
#                                TALLA = c(98))

#specie_americanus <- data.frame(ESP_CAT = c("Polyprion americanus-U"), 
#                                TALLA = c(94, 98, 102:104))

#specie_canicula_h <- data.frame(ESP_CAT = c("Scyliorhinus canicula-H"), 
#                              TALLA = c(72))

#specie_canicula_m <- data.frame(ESP_CAT = c("Scyliorhinus canicula-M"), 
#                                TALLA = c(77, 84))

#specie_eblanae <- data.frame(ESP_CAT = c("Todaropsis eblanae-U"), 
#                                TALLA = c(30))

#specie_lyra <- data.frame(ESP_CAT = c("Trigla lyra-U"), 
#                              TALLA = c(24, 25, 64, 67))

#specie_boscii <- data.frame(ESP_CAT = c("Lepidorhombus boscii-U"),
#                               TALLA = c(53:55))

#specie_circularis <- data.frame(ESP_CAT = c("Leucoraja circularis-U"),
#                               TALLA = c(60))

#specie_forbesii <- data.frame(ESP_CAT = c("Loligo forbesii-U"),
#                               TALLA = c(45:46, 49:50, 52:53))


#discarded_sizes_ac <- rbind(specie_conger, specie_sargus, specie_fullonica, 
#                            specie_naevus_h, specie_naevus_m, specie_loligo_spp,
#                            specie_loligo_vulgaris, specie_lophius_budegassa, 
#                            specie_lophius_piscatorius, specie_merluccius, 
#                            specie_macrophthalma, specie_molva, specie_moro, 
#                            specie_surmulletus, specie_flesus, specie_pollachius, 
#                            specie_americanus, specie_canicula_h, specie_canicula_m,
#                            specie_eblanae, specie_lepturus, specie_lyra)



##### Other sizes CN #####


specie_argentina_cn <- data.frame(ESP_CAT = c("Argentina sphyraena-U"), 
                                  TALLA = c(29))

specie_beryx_cn <- data.frame(ESP_CAT = c("Beryx decadactylus-U"), 
                              TALLA = c(65))

specie_boops_cn <- data.frame(ESP_CAT = c("Boops boops-U"), 
                              TALLA = c(8))

specie_brama_cn <- data.frame(ESP_CAT = c("Brama brama-U"), 
                              TALLA = c(23))

specie_lastoviza_cn <- data.frame(ESP_CAT = c("Chelidonichthys lastoviza-U"), 
                                  TALLA = c(36, 38))

specie_obscurus_cn <- data.frame(ESP_CAT = c("Chelidonichthys obscurus-U"), 
                                 TALLA = c(35))

specie_conger_cn <- data.frame(ESP_CAT = c("Conger conger-U"), 
                               TALLA = c(203, 208, 210, 214))

specie_calceus_cn <- data.frame(ESP_CAT = c("Deania calceus-U"), 
                                TALLA = c(75, 79))


specie_engraulis_cn <- data.frame(ESP_CAT = c("Engraulis encrasicolus-U"), 
                                  TALLA = c(22.5))

specie_melastomus_h_cn <- data.frame(ESP_CAT = c("Galeus melastomus-H"), 
                                     TALLA = c(39:43, 80))

specie_melastomus_m_cn <- data.frame(ESP_CAT = c("Galeus melastomus-M"), 
                                     TALLA = c(41:44))



specie_mediterraneus_cn <- data.frame(ESP_CAT = c("Hoplostethus mediterraneus-M"), 
                                      TALLA = c(15))

specie_illex_cn <- data.frame(ESP_CAT = c("Illex coindetii-U"), 
                              TALLA = c(44))

specie_katsuwonus_cn <- data.frame(ESP_CAT = c("Katsuwonus pelamis-U"), 
                                   TALLA = c(59))

specie_bergyta_cn <- data.frame(ESP_CAT = c("Labrus bergylta-U"), 
                                TALLA = c(59))

specie_mixtus_cn <- data.frame(ESP_CAT = c("Labrus mixtus-U"), 
                               TALLA = c(39, 42))

specie_naevus_cn <- data.frame(ESP_CAT = c("Leucoraja naevus-H"), 
                               TALLA = c(30, 78, 84, 94))

specie_forbesi_cn <- data.frame(ESP_CAT = c("Loligo forbesii-U"), 
                                TALLA = c(57:60, 62:64, 67))



specie_vulgaris_cn <- data.frame(ESP_CAT = c("Loligo vulgaris-U"), 
                                 TALLA = c(51, 57))

specie_budegassa_cn <- data.frame(ESP_CAT = c("Lophius budegassa-U"), 
                                  TALLA = c(118, 122))

specie_piscatorius_cn <- data.frame(ESP_CAT = c("Lophius piscatorius-U"), 
                                    TALLA = c(151, 153))

specie_squinado_cn <- data.frame(ESP_CAT = c("Maja squinado-U"), 
                                 TALLA = c(29))


specie_squinado_cn <- data.frame(ESP_CAT = c("Maja squinado-U"), 
                                 TALLA = c(29))

specie_merluccius_cn <- data.frame(ESP_CAT = c("Merluccius merluccius-U"), 
                                   TALLA = c(110:115, 117))


specie_microchirus_cn <- data.frame(ESP_CAT = c("Microchirus variegatus-U"), 
                                    TALLA = c(39))

specie_micromesistus_cn <- data.frame(ESP_CAT = c("Micromesistius poutassou-U"), 
                                      TALLA = c(54))

specie_molva_cn <- data.frame(ESP_CAT = c("Molva molva-U"), 
                              TALLA = c(132, 137))

specie_acarne_cn <- data.frame(ESP_CAT = c("Pagellus acarne-U"), 
                               TALLA = c(54:58))



specie_lascaris_cn <- data.frame(ESP_CAT = c("Pegusa lascaris-U"), 
                                 TALLA = c(40))

specie_lascaris_cn <- data.frame(ESP_CAT = c("Platichthys flesus-U"), 
                                 TALLA = c(61:63))

specie_pollachius_cn <- data.frame(ESP_CAT = c("Pollachius pollachius-U"), 
                                   TALLA = c(90:95))



specie_clavata_h_cn <- data.frame(ESP_CAT = c("Raja clavata-H"), 
                                  TALLA = c(105, 114))

specie_clavata_m_cn <- data.frame(ESP_CAT = c("Raja clavata-M"), 
                                  TALLA = c(108, 110, 118))





specie_montagui_h_cn <- data.frame(ESP_CAT = c("Raja montagui-H"), 
                                   TALLA = c(118:119))

specie_montagui_m_cn <- data.frame(ESP_CAT = c("Raja montagui-M"), 
                                   TALLA = c(118))



specie_sarda_cn <- data.frame(ESP_CAT = c("Sarda sarda-U"),
                              TALLA = c(87:90, 92:93, 95, 97,
                                        99:105, 107:112, 114:118, 
                                        121, 123, 127, 130, 132))





specie_scophthalmus_rombus_cn <- data.frame(ESP_CAT = c("Scophthalmus rhombus-U"),
                                            TALLA = c(23))

specie_elegans_cn <- data.frame(ESP_CAT = c("Sepia elegans-U"), 
                                TALLA = c(12, 17, 19, 21, 22))

specie_cabrilla_cn <- data.frame(ESP_CAT = c("Serranus cabrilla-U"), 
                                 TALLA = c(35, 36))

specie_solea_cn <- data.frame(ESP_CAT = c("Solea solea-U"), 
                              TALLA = c(58))



specie_draco_cn <- data.frame(ESP_CAT = c("Trachinus draco-U"), 
                              TALLA = c(38))



##### Other sizes GC #####

specie_regius_gc <- data.frame(ESP_CAT = c("Argyrosomus regius-U"), 
                                  TALLA = c(86, 88, 90, 92, 94:96, 
                                            98:118, 120, 122:134, 
                                            136:151, 153, 155, 161, 
                                            172))



specie_cepola_gc <- data.frame(ESP_CAT = c("Cepola macrophthalma-U"), 
                               TALLA = c(85))



specie_linguatula_gc <- data.frame(ESP_CAT = c("Citharus linguatula-U"),
                                   TALLA = c(31, 37, 40, 43))



specie_cuneata_gc <- data.frame(ESP_CAT = c("Dicologlossa cuneata-U"),
                                TALLA = c(8, 28))

specie_cervinus_gc <- data.frame(ESP_CAT = c("Diplodus cervinus-U"),
                                 TALLA = c(50))



specie_vulgaris_gc <- data.frame(ESP_CAT = c("Diplodus vulgaris-U"),
                                 TALLA = c(8:9, 11))



specie_boscii_gc <- data.frame(ESP_CAT = c("Lepidorhombus boscii-U"),
                                 TALLA = c(37))

specie_mormyrus_gc <- data.frame(ESP_CAT = c("Lithognathus mormurys-U"),
                               TALLA = c(13, 38))

specie_vulgaris_gc <- data.frame(ESP_CAT = c("Loligo vulgaris-U"),
                                 TALLA = c(52))

specie_vulgaris_gc <- data.frame(ESP_CAT = c("Loligo vulgaris-U"),
                                 TALLA = c(52))

specie_budegassa_gc <- data.frame(ESP_CAT = c("Lophius budegassa-U"),
                                  TALLA = c(60:63, 66, 68:70, 72, 80))

specie_piscatorius_gc <- data.frame(ESP_CAT = c("Lophius piscatorius-U"),
                                  TALLA = c(58, 60:62, 65, 71:76, 80:83, 98, 103))

specie_merluccius_gc <- data.frame(ESP_CAT = c("Merluccius merluccius-U"),
                                    TALLA = c(58, 62:63, 70, 78))

specie_vulgaris_gc <- data.frame(ESP_CAT = c("Octupis vulgaris-U"),
                                   TALLA = c(29))

specie_bellottii_gc <- data.frame(ESP_CAT = c("Pagellus bellottii-U"),
                                  TALLA = c(8))

specie_bogaraveo_gc <- data.frame(ESP_CAT = c("Pagellus bogaraveo-U"),
                                  TALLA = c(12:18, 60))

specie_erythrinus_gc <- data.frame(ESP_CAT = c("Pagellus erythrinus-U"),
                                  TALLA = c(45))

specie_auriga_gc <- data.frame(ESP_CAT = c("Pagrus auriga-U"),
                                   TALLA = c(6:8, 48:49, 52))

specie_pagrus_gc <- data.frame(ESP_CAT = c("Pagrus pagrus-U"),
                               TALLA = c(22, 62))


specie_asterias_gc <- data.frame(ESP_CAT = c("Raja asterias-H"),
                                  TALLA = c(38))

specie_clavata_h_gc <- data.frame(ESP_CAT = c("Raja clavata-H"),
                                 TALLA = c(78))

specie_clavata_m_gc <- data.frame(ESP_CAT = c("Raja clavata-M"),
                                  TALLA = c(38, 88, 103))













specie_canicula_h_gc <- data.frame(ESP_CAT = c("Scyliorhinus canicula-H"),
                               TALLA = c(26, 29:33, 35:36, 67))





specie_aurata_gc <- data.frame(ESP_CAT = c("Sparus aurata-U"),
                              TALLA = c(13:17, 51:52, 55, 59))



specie_mantis_gc <- data.frame(ESP_CAT = c("Squilla mantis-H"),
                                  TALLA = c(4.60, 5.20))













