


library("dplyr")
library("gplots")

load("./result/siope_per_cap.rdata")

# "spread" = Error
#m <- tbl_df(x3) %>%
#       filter(anno == 2014) %>%
#        select_("codice_gestionale", "cod_provincia", "mean_imp_per_cap") # %>%
#         spread(cod_provincia, codice_gestionale)

# autsch,....
m <- as.matrix(tapply(x3$sd_imp_per_cap, 
               list(x3$cod_provincia, x3$codice_gestionale), mean))

png(filename = "./result/hm_sd_imp_per_cap.png", 
    pointsize = 10, width = 29.7, height = 21, 
    units = "cm", res = 150)
    # http://stackoverflow.com/a/20859824
    hm1 <- heatmap.2(m,
               col = colorpanel(100, "red", "yellow", "green"),
               margins = c(8, 12),
               trace = "none", 
               xlab = "",
               lhei = c(2, 8),
               scale = c("col"),
               #symbreaks = min(mat, na.rm = TRUE),
               na.color = "blue",
               cexRow = 0.7, cexCol = 0.7,
               main = "sd_imp_per_cap by cod_provincia / codice_gestionale", 
               dendrogram = "row", 
               Colv = FALSE)
dev.off()

hm1$rowInd

# Lombardia 16  -   ITC46   ITC46   Bergamo
# Veneto    25  -   ITD33   ITH33   Belluno
# Campania  64  -   ITF34   ITF34   Avellino

b0 <- tbl_df(x2) %>%
        filter(cod_provincia == 25 & 
               anno == 2014 & 
               sottocomparto_siope == "COMUNE") %>%
        arrange(codice_gestionale, imp_per_cap) %>%
          group_by(codice_gestionale) %>%
            filter(imp_per_cap == max(imp_per_cap)) %>%
              arrange(codice_gestionale, cod_comune)

write.csv2(b0, file= "./result/SD_max_imp_per_cap.csv")

#
# FINI
#