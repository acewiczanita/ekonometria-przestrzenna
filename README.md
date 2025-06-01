# ekonometria-przestrzenna
install.packages("sf") 

install.packages("spdep")
install.packages("viridis")
install.packages("spatialreg")
install.packages("corrplot")
install.packages("standardize")
install.packages("tidyr")
install.packages("spatialreg")

library(sf)   
library(ggplot2)
library(spdep)
library(dplyr)
library(spatialreg)
library(viridis) 
library(readxl)
library(corrplot)
library(standardize)
library(tidyr)
library(spatialreg)
czas_trwania_zycia <- read_excel("C:/Users/Paweł/Documents/ekonometria-przestrzenna/ekonometria dane.xlsx")
head(czas_trwania_zycia)
polska_woj <- st_read("C:/Users/Paweł/Documents/ekonometria-przestrzenna/wojewodztwa.shp")
polska_woj_dane <- polska_woj %>%
  left_join(czas_trwania_zycia, by = c("JPT_NAZWA_" = "wojewodztwo"))
  
ggplot(polska_woj_dane) +
  geom_sf(aes(fill = trwanie_zycia), color = "white") +
 scale_fill_gradient(low = "yellow", high = "red", name = "Średni czas trwania życia noworodka") +
theme_minimal() +
  labs(title = "Średni czas trwania życia noworodka w województwach Polski")
  
# statystyki opisowe 
install.packages("e1071")
library(e1071)
dane_numeryczne <- czas_trwania_zycia[sapply(czas_trwania_zycia, is.numeric)]

# wykresy 
dane_long <- czas_trwania_zycia %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "zmienna", values_to = "wartosc")

# Histogramy
ggplot(czas_trwania_zycia, aes(x = trwanie_zycia)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "white") +
  labs(title = "Histogram: trwanie życia", x = "Trwanie życia", y = "Liczba obserwacji") +
  theme_minimal()
ggplot(czas_trwania_zycia, aes(x = trwanie_zycia)) +
  geom_density(fill = "lightblue", color = "darkblue", lwd = 1) +
  labs(title = "Wykres gęstości: trwanie życia", x = "Trwanie życia", y = "Gęstość") +
  theme_minimal()
ggplot(czas_trwania_zycia, aes(x = lekarze, y = trwanie_zycia)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Trwanie życia vs liczba lekarzy na 10 tys. osób", x = "Lekarze (na 10 tys.)", y = "Trwanie życia") +
  theme_minimal()
ggplot(czas_trwania_zycia, aes(x = szpitale, y = trwanie_zycia)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Trwanie życia vs liczba szpitali na 10 tys. osób", x = "Lekarze (na 10 tys.)", y = "Trwanie życia") +
  theme_minimal()
ggplot(czas_trwania_zycia, aes(x = lozka_szpitalne, y = trwanie_zycia)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Trwanie życia vs liczba łóżek szpitalnych na 10 tys. osób", x = "Lekarze (na 10 tys.)", y = "Trwanie życia") +
  theme_minimal()
ggplot(czas_trwania_zycia, aes(x = absolwenci, y = trwanie_zycia)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Trwanie życia vs liczba absolwentów studiów wyższych na 10 tys. osób ", x = "Lekarze (na 10 tys.)", y = "Trwanie życia") +
  theme_minimal()
ggplot(czas_trwania_zycia, aes(x = pieleg_polozne, y = trwanie_zycia)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Trwanie życia vs liczba pielęgniarek i położnych na 10 tys. osób", x = "Lekarze (na 10 tys.)", y = "Trwanie życia") +
  theme_minimal()
  
install.packages("tmap")
library(tmap)
library(dplyr)

# Lista zmiennych do mapowania
zmienne <- c("lekarze", "lozka_szpitalne", "szpitale", "pieleg_polozne", "trwanie_zycia", "absolwenci")

for (zm in zmienne) {
  paleta <- switch(zm,
                   lekarze = "Greens",
                   lozka_szpitalne = "Purples",
                   szpitale = "Oranges",
                   pieleg_polozne = "Blues",
                   trwanie_zycia = "RdYlGn",
                   absolwenci = "Greys",
                   "Blues")  # domyślna paleta
  
  print(
    tm_shape(polska_woj_dane) +
      tm_polygons(zm,
                  palette = paleta,
                  title = zm) +
      tm_layout(main.title = paste("Rozkład:", zm),
                legend.outside = TRUE)
  )
}

# Zastosowanie funkcji do wszystkich kolumn
statystyki_opisowe <- as.data.frame(t(sapply(dane_numeryczne, oblicz_statystyki)))


print(statystyki_opisowe)
#Obliczenie globalnego wskaźnika autokorelacji przestrzennej Morana (Moran’s I)
nb <- poly2nb(polska_woj)  
lw <- nb2listw(nb, style = "W", zero.policy=TRUE) 
moran.test(polska_woj_dane$trwanie_zycia, lw)
moran.plot(polska_woj_dane$trwanie_zycia, lw, labels = FALSE, pch = 20,
           xlab = "Zgony (standardized)", 
           ylab = "Spatial Lag of zgony")
#Obliczenie lokalnych wskaźników LISA i ich wizualizacja 
local_moran <- localmoran(polska_woj_dane$trwanie_zycia, lw)
polska_woj_dane$Ii <- local_moran[, 1]
polska_woj_dane$P.Ii <- local_moran[, 5]
print(local_moran)

print(polska_woj_dane)
polska_woj_dane$significant <- polska_woj_dane$P.Ii < 0.05


ggplot(polska_woj_dane) +
  geom_sf(aes(fill = significant), color = "black", size = 0.1) +
  scale_fill_manual(values = c("white", "red"), labels = c("nieistotne", "istotne")) +
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
AIC(model_stat)  
model_best_stat <- step(model_stat, direction = "backward")

# test morana dla reszt
moran_test <- moran.test(residuals(model_stat), lw, zero.policy = TRUE)
print(moran_test)
# wybór testu  
lm.LMtests(model_stat, lw, test = "all")
# wybieramy  Robust LM Error (adjRSerr = 4.014 , df = 1,  p-value = 0.04513
library(spatialreg)

sem_model <- errorsarlm(
  formula = trwanie_zycia ~ absolwenci +
  lekarze +
  lozka_szpitalne+
 szpitale +
pieleg_polozne,
  data = czas_trwania_zycia_std,
  listw = lw,
  method = "eigen"
)
summary(sem_model)
AIC(sem_model)
  install.packages("car")
library("car")  
vif(model_stat)
# Obliczenie korelacji

corr_matrix <- cor(czas_trwania_zycia[, sapply(czas_trwania_zycia, is.numeric) & colnames(czas_trwania_zycia) != "wojewodztwo"], use = "complete.obs")

 # Wizualizacja
corrplot(corr_matrix, method = "color", addCoef.col = "black", number.cex=0.6) 


