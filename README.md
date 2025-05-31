# ekonometria-przestrzenna
install.packages("sf") 

install.packages("spdep")
install.packages("viridis")
install.packages("spatialreg")
install.packages("corrplot")
install.packages("standardize")

library(sf)   
library(ggplot2)
library(spdep)
library(dplyr)
library(spatialreg)
library(viridis) 
library(readxl)
library(corrplot)
library(standardize)
czas_trwania_zycia <- read_excel("C:/Users/Paweł/Documents/ekonometria-przestrzenna/ekonometria dane.xlsx")
head(czas_trwania_zycia)
polska_woj <- st_read("C:/Users/Paweł/Documents/ekonometria-przestrzenna/wojewodztwa.shp")
polska_woj_dane <- polska_woj %>%
  left_join(czas_trwania_zycia, by = c("JPT_NAZWA_" = "wojewodztwo"))
  
ggplot(polska_woj_dane) +
  geom_sf(aes(fill = trwanie_zycia), color = "white") +
 scale_fill_gradient(low = "yellow", high = "red", name = "Średni czas trwania życia") +
theme_minimal() +
  labs(title = "Średni czas trwania życia w województwach Polski")
nb <- poly2nb(polska_woj)  # Neighbors list
lw <- nb2listw(nb, style = "W", zero.policy=TRUE) 
moran.test(polska_woj_dane$trwanie_zycia, lw)
moran.plot(polska_woj_dane$trwanie_zycia, lw, labels = FALSE, pch = 20,
           xlab = "Zgony (standardized)", 
           ylab = "Spatial Lag of zgony")
local_moran <- localmoran(polska_woj_dane$trwanie_zycia, lw)
polska_woj_dane$Ii <- local_moran[, 1]
polska_woj_dane$P.Ii <- local_moran[, 5]

print(polska_woj_dane)
polska_woj_dane$significant <- polska_woj_dane$P.Ii < 0.05


ggplot(polska_woj_dane) +
  geom_sf(aes(fill = significant), color = "black", size = 0.1) +
  scale_fill_manual(values = c("white", "red"), labels = c("niesistotne", "istotne")) +
  labs(title = "Lokalna istotność statystyki Moran's I", 
   fill = "Istotność")+
  theme_minimal()
czas_trwania_zycia_std <- czas_trwania_zycia %>% mutate(across(where(is.numeric), scale))
model_stat <- lm(
  trwanie_zycia ~ absolwenci +
  lekarze +
  lozka_szpitalne+
 szpitale +
pieleg_polozne,
 
data = czas_trwania_zycia_std)

summary(model_stat)  
  
model_best_stat <- step(model_stat, direction = "backward")

summary(model_best_stat)  
  install.packages("car")
library("car")  
vif(model_stat)
# Obliczenie korelacji

corr_matrix <- cor(czas_trwania_zycia[, sapply(czas_trwania_zycia, is.numeric) & colnames(czas_trwania_zycia) != "wojewodztwo"], use = "complete.obs")

 # Wizualizacja
corrplot(corr_matrix, method = "color", addCoef.col = "black", number.cex=0.6) 
polska

