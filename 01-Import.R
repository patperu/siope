
# Todo
# - Shiny

# shapefiles - http://www.istat.it/it/archivio/104317

library("ggplot2")
library("ggmap")
library("dplyr")
library("rgdal")

op <- options()

# http://danielphadley.com/Automate-Map-Making/
Read_shp <- function(layer) {
    m <- readOGR(dsn = "./shp", layer = layer)
    m@data$id = rownames(m@data)
    #names(m)
    m.points <- fortify(m, region = "id")
    m.df <- inner_join(m.points, m@data, by = "id")
    return(m.df)
}

options(stringsAsFactors = FALSE)
options(digits = 15)

###############################################################

xsh.df <- Read_shp("Prov2011_WGS84")

###############################################################

# "000000019597654","2013","01","1202","000000000093132"
# "000000019597654","2013","01","1212","000000000057034"
# "000000019597654","2013","01","1311","000000000177899"

x <- read.csv("./data/ENTI_USCITE_MENSILI.csv", header = FALSE)
colnames(x) <- tolower(c("COD_ENTE", "ANNO", "PERIODO", 
                         "CODICE_GESTIONALE", "IMP_USCITE_ATT"))
x$imp_uscite_att <- round(x$imp_uscite_att / 100, 2)

###############################################################

z <- read.csv("./data/ANAG_ENTI_SIOPE.csv", header = FALSE)
colnames(z) <- tolower(c("COD_ENTE",
                    "DATA_INC_SIOPE",
                    "DATA_ESC_SIOPE",
                    "COD_FISCALE",
                    "DESCR_ENTE",
                    "COD_COMUNE",
                    "COD_PROVINCIA",
                    "NUM_ABITANTI",
                    "SOTTOCOMPARTO_SIOPE"))

z <- tbl_df(z) %>%
      filter(sottocomparto_siope == "COMUNE") %>%
      mutate(id = 1:n())

z_lt100 <- tbl_df(z) %>%
             filter(num_abitanti < 100) %>%
               arrange(num_abitanti)

z <- tbl_df(z) %>%
       filter(num_abitanti > 100) %>%
         arrange(num_abitanti)

###############################################################

x1 <- tbl_df(x) %>% 
        group_by(cod_ente, anno, codice_gestionale) %>% 
        summarise(imp_uscite_att = sum(imp_uscite_att))

x2 <- merge(x1, z, by = "cod_ente", all.x = TRUE)
x2$imp_per_cap <- round(x2$imp_uscite_att / x2$num_abitanti, 2)

x3 <- tbl_df(x2) %>% 
         group_by(anno, sottocomparto_siope, cod_provincia, codice_gestionale) %>% 
          summarise(n = n(),
                    mean_num_abitanti   = mean(num_abitanti, na.rm = TRUE), 
                    sum_num_abitanti    = sum(num_abitanti, na.rm = TRUE), 

                    sum_imp_uscite_att  = sum(imp_uscite_att, na.rm = TRUE), 
                    mean_imp_uscite_att = mean(imp_uscite_att, na.rm = TRUE), 

                    sd_imp_per_cap      = sd(imp_per_cap, na.rm = TRUE),
                    p25_imp_per_cap     = quantile(imp_per_cap, probs = 0.25, na.rm = TRUE),
                    p75_imp_per_cap     = quantile(imp_per_cap, probs = 0.75, na.rm = TRUE),
                    iqr_imp_per_cap     = IQR(imp_per_cap, na.rm = TRUE),
                    median_imp_per_cap  = median(imp_per_cap, na.rm = TRUE),
                    mean_imp_per_cap    = mean(imp_per_cap, na.rm = TRUE),
                    cv_imp_per_cap      = sd_imp_per_cap / mean_imp_per_cap,
                    mean_imp_per_cap_ag = sum_imp_uscite_att / sum_num_abitanti)
x3

###############################################################

# Glossario-Enti-locali-per-gli-anni-2013-e-successivi-1.xls
# 2113  Beni di valore culturale, storico, archeologico, ed artistico
#       Spesa sostenuta per l'acquisizione e la manutenzione straordinaria 
#       (diretta a ripristinare o aumentare il valore originario) di fabbricati 
#       relativi alle opere di scultura o struttura architettonica di valore 
#       culturale, storico, archeologico ed artistico. 
#       Comprende le Biblioteche e i Musei.

f1 <- filter(x3, anno == 2014, codice_gestionale == 2113)
m.df <- merge(xsh.df, f1, by.x = "COD_PRO", by.y = "cod_provincia")
m.df <- m.df[order(m.df$order), ]

make_plot <- function(m.df, var_name, gtitle) {
  # http://www.kevjohnson.org/making-maps-in-r/
  p <- ggplot() + geom_polygon(data = m.df, 
                  aes_string(x = "long", y = "lat", group = "group",
                             fill = var_name), color = "black", size = 0.25)
  #p <- p + coord_map()
  p <- p + scale_fill_distiller(palette = "Greens")
  p <- p + guides(fill = guide_legend(reverse = TRUE))
  p <- p + theme_nothing(legend = TRUE)
  p <- p + ggtitle(gtitle)
  return(p)
}

p_mean <- make_plot(m.df, 
                    "mean_imp_per_cap", 
                    "2113 - Beni di valore culturale, storico, archeologico, ed artistico\nper Capita (Mean), 2014")
ggsave(p_mean, file = "./result/2113_Beni_di_valore_perC_2014_mean.png", width = 9, height = 9)

p_mean_ag <- make_plot(m.df, 
                    "mean_imp_per_cap_ag", 
                    "2113 - Beni di valore culturale, storico, archeologico, ed artistico\nper Capita (Mean_ag), 2014")
ggsave(p_mean_ag, file = "./result/2113_Beni_di_valore_perC_2014_mean_ag.png", width = 9, height = 9)

p_median <- make_plot(m.df, 
                    "median_imp_per_cap", 
                    "2113 - Beni di valore culturale, storico, archeologico, ed artistico\nper Capita (Median), 2014")
ggsave(p_median, file = "./result/2113_Beni_di_valore_perC_2014_median.png", width = 9, height = 9)

p_cv <- make_plot(m.df, 
                      "cv_imp_per_cap", 
                      "2113 - Beni di valore culturale, storico, archeologico, ed artistico\nper Capita (CV), 2014")
ggsave(p_cv, file = "./result/2113_Beni_di_valore_perC_2014_cv.png", width = 9, height = 9)

save(x, x2, x3, xsh.df, file = "./result/siope_per_cap.rdata")
write.csv2(x3, file= "./result/siope_per_cap.csv")

options(op)

#
# FINI
#