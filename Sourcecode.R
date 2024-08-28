#Packete installieren
install.packages("zoo")
install.packages("dplyr")
install.packages("tidyr")  
install.packages("ggplot2")
install.packages("stringr")
install.packages("gridExtra")
install.packages("gt")
install.packages("kableExtra")

#Bibliotheken einbinden
library(knitr)
library(kableExtra)
library(tidyr)   
library(readxl)
library(dplyr)
library(zoo)
library(ggplot2)
library(stringr) # Für Zeichenmanipulation
library(gridExtra)
library(gt)

#Pfade zu den Excel dateien
internetaccess_path <- "C:/Users/gerin/Google Drive/IU/Statistical-Computing/Breitband.xlsx"
householdspending_path <- "C:/Users/gerin/Google Drive/IU/Statistical-Computing/Householdspending.xlsx"
population_path <- "C:/Users/gerin/Google Drive/IU/Statistical-Computing/Population.xlsx"
#in dataframes einlesen aus excel
population <- read_excel(population_path, col_names = TRUE)
householdspending <- read_excel(householdspending_path, col_names = TRUE)
internetaccess <- read_excel(internetaccess_path, col_names = TRUE)

#Fehlende werte mit na.locf einfügen
internetaccess <- t(internetaccess)
internetaccess <- apply(internetaccess, 2, zoo::na.locf, na.rm = FALSE)
internetaccess <- t(internetaccess)

#Umwandlung in Dataframe
internetaccess <- as.data.frame(internetaccess)

#Umwandeln in long Datenformat um die Daten verarbeiten zu können 
#Ohne diesen Schritt wäre jedes Jahr eine Spalte
internetaccess_long <- internetaccess %>%
  select(Country, starts_with("20")) %>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Prozent")%>%
  mutate(Year = as.integer(Year))


# Beide Tabellen zusammenführen
combined_data <- householdspending %>%
  inner_join(population, by = "Country" , suffix = c("_spending", "_population"))


#combined_data <- combined_data %>%
 # mutate(across(`2012_spending`:`2022_spending`, ~ . / get(sub("spending", "population", cur_column()))))


# Datenrahmen in langes Format bringen
spending_long <- combined_data %>%
  select(Country, ends_with("_spending")) %>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Spending") %>%
  mutate(Year = as.integer(str_remove(Year, "_spending"))) 

#Fehlende werte mit na.locf einfügen

spending_long <- apply(spending_long, 2, zoo::na.locf, na.rm = FALSE)
#Umwandlung in Dataframe
spending_long <- as.data.frame(spending_long)%>%
mutate(Year = as.integer(Year),Spending=as.numeric(Spending))
str(spending_long)

#Umwandeln in long Datenformat um die Daten verarbeiten zu können
population_long <- combined_data %>%
  select(Country, ends_with("_population")) %>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Population") %>%
  mutate(Year = as.integer(str_remove(Year, "_population")))

# Haushaltsausgaben pro Einwohner berechnen
data_long <- spending_long %>%
  inner_join(population_long, by = c("Country", "Year")) %>%
  mutate(Spending_per_Capita = Spending / Population)

# Normalisierung:  durch Division durch den Durchschnitt über alle Jahre
# Dieser Schritt dient zur besseren anzeige 
mean_values <- data_long %>%
  group_by(Country) %>%
  summarize(mean_spending_per_capita = mean(Spending_per_Capita, na.rm = TRUE))
#Einfügen des am durchschnitt  normalisierten wertes                                                            
data_long <- data_long %>%
  group_by(Country) %>%
  mutate(Normalized_Spending_per_Capita = Spending_per_Capita / mean(Spending_per_Capita))



#Funktion zur visualisierung des Internetzugangs
visualisierung_internetzugang <- function(internetparam){
  # Prozent-Spalte in numerische Werte umwandeln
  internetparam$Prozent <- as.numeric(internetparam$Prozent)
  
  # Daten für Länder von A-F
  internet_AtoF <- internetparam %>%
    filter(str_starts(Country, "^[A-F]"))
  
  # Daten für Länder von G-L
  internet_GtoL <- internetparam %>%
    filter(str_starts(Country, "^[G-L]"))
  
  # Daten für Länder von M-R
  internet_MtoR <- internetparam %>%
    filter(str_starts(Country, "^[M-R]"))
  
  # Daten für Länder von S-Z
  internet_StoZ <- internetparam %>%
    filter(str_starts(Country, "^[S-Z]"))
  
  # Erstellung der Plots
  p1 <- ggplot(internet_AtoF, aes(x = as.integer(Year), y = Prozent, color = Country, group = Country)) +
    geom_line() +
    labs(title = "Internetaccess Percent countrys A-F",
         x = "Year",
         y = "Internetaccess") +
    scale_y_continuous(breaks = seq(0, 100, by = 5)) +  # Y-Achse in 5er-Schritten 
    scale_x_continuous(breaks = scales::pretty_breaks(n = length(unique(internet_AtoF$Year)))) +
    theme_minimal()
  
  p2 <- ggplot(internet_GtoL, aes(x = as.integer(Year), y = Prozent, color = Country, group = Country)) +
    geom_line() +
    labs(title = "Internetaccess Percent countrys G-L",
         x = "Year",
         y = "Internetaccess") +
    scale_y_continuous(breaks = seq(0, 100, by = 5)) +  # Y-Achse in 5er-Schritten 
    scale_x_continuous(breaks = scales::pretty_breaks(n = length(unique(internet_GtoL$Year)))) +
    theme_minimal()
  
  p3 <- ggplot(internet_MtoR, aes(x = as.integer(Year), y = Prozent, color = Country, group = Country)) +
    geom_line() +
    labs(title = "Internetaccess Percent countrys M-R",
         x = "Year",
         y = "Internetaccess") +
    scale_y_continuous(breaks = seq(0, 100, by = 5)) +  # Y-Achse in 5er-Schritten 
    scale_x_continuous(breaks = scales::pretty_breaks(n = length(unique(internet_MtoR$Year)))) +
    theme_minimal()
  
  p4 <- ggplot(internet_StoZ, aes(x = as.integer(Year), y = Prozent, color = Country, group = Country)) +
    geom_line() +
    labs(title = "Internetaccess Percent countrys S-Z",
         x = "Year",
         y = "Internetaccess") +
    scale_y_continuous(breaks = seq(0, 100, by = 5)) +  # Y-Achse in 5er-Schritten 
    scale_x_continuous(breaks = scales::pretty_breaks(n = length(unique(internet_StoZ$Year)))) +
    theme_minimal()
  
  # Plots in einem Layout anzeigen
  grid.arrange(p1, p2, p3, p4, ncol = 2)
}


visualisierung_internetzugang(internetaccess_long)

#Funktion zur visualisierung der Haushaltsaufgaben
visualisierung_haushaltsausgaben <- function(householdparam){
  
  #AUfteilen in 4 Gruppen nach dem Alphabet
  #Länder von A-F  
  data_AtoF <- householdparam %>%
    filter(str_starts(Country, "^[A-F]"))
  #Länder von G-L  
  data_GtoL <- householdparam %>%
    filter(str_starts(Country, "^[G-L]"))
  #Länder von M-R  
  data_MtoR <- householdparam %>%
    filter(str_starts(Country, "^[M-R]"))
  #Länder von S-Z  
  data_StoZ <- householdparam %>%
    filter(str_starts(Country, "^[S-Z]"))
  

# Visualisierung der normalisierten Haushaltsausgaben pro Einwohner A-F
  p1 <- ggplot(data_AtoF, aes(x = as.integer(Year), y = Spending_per_Capita, color = Country, group = Country)) +
  geom_line() +
  labs(title = "Household Spending per Capita A-F",
       x = "Year",
       y = "Spending per Capita in US Dollar") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = length(unique(data_long$Year)))) +
  theme_minimal()


# Visualisierung der normalisierten Haushaltsausgaben pro Einwohner G-L
  p2 <- ggplot(data_GtoL, aes(x = as.integer(Year), y = Spending_per_Capita, color = Country, group = Country)) +
  geom_line() +
  labs(title = "Household Spending per Capita G-L",
       x = "Year",
       y = "Spending per Capita in US Dollar") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = length(unique(data_long$Year)))) +
  theme_minimal()


# Visualisierung der normalisierten Haushaltsausgaben pro Einwohner M-R
p3 <- ggplot(data_MtoR, aes(x = as.integer(Year), y = Spending_per_Capita, color = Country, group = Country)) +
  geom_line() +
  labs(title = "Household Spending per Capita M-R",
       x = "Year",
       y = "Spending per Capita in US Dollar") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = length(unique(data_long$Year)))) +
  theme_minimal()

# Visualisierung der normalisierten Haushaltsausgaben pro Einwohner S-Z
p4 <- ggplot(data_StoZ, aes(x = as.integer(Year), y = Spending_per_Capita, color = Country, group = Country)) +
  geom_line() +
  labs(title = "Household Spending per Capita S-Z",
       x = "Year",
       y = "Spending per Capita in US Dollar") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = length(unique(data_long$Year)))) +
  theme_minimal()

# Plots in einem Layout anzeigen
grid.arrange(p1, p2, p3, p4, ncol = 2)
}
visualisierung_haushaltsausgaben(data_long)

#Korrellation#######################################################################################################

#Daten in einem dataframe kombinieren um Regressionen durchführen zu können
combined_data <- data_long %>%
  inner_join(internetaccess_long, by = c("Year", "Country"))%>%
  mutate(Prozent =as.numeric(Prozent))



#Korrelation zwischen Breitbandzugang und Haushaltsausgaben pro Einwohner über die Zeit
correlation_results <- combined_data %>%
  group_by(Country) %>%
  summarize(Correlation = cor(Spending_per_Capita, Prozent, use = "complete.obs"))

#Visualisierung der Korrelation
print(n=20,correlation_results) 
windows() 
ggplot(combined_data, aes(x = Spending_per_Capita, y = Prozent, color = as.factor(Year))) +
  geom_point() +
  labs(title = "Zusammenhang zwischen Haushaltsausgaben und Breitbandzugang",
       x = "Haushaltsausgaben pro Einwohner",
       y = "Breitbandzugang in Prozent",
       color = "Jahr") +
  theme_minimal()

#Korrelation zwischen Breitbandzugang und Haushaltsausgaben pro Einwohner zwischen den Ländern
aggregated_data <- combined_data %>%
  group_by(Year) %>%
  summarize(
    Avg_Spending_per_Capita = mean(Spending_per_Capita, na.rm = TRUE),
    Avg_Prozent = mean(Prozent, na.rm = TRUE)
  )
#Korrelationskoeffizient über alle Länder
correlation_yearly <- cor(aggregated_data$Avg_Spending_per_Capita, aggregated_data$Avg_Prozent, use = "complete.obs")

#Korrelation printen
print(correlation_yearly)

windows() 
ggplot(combined_data, aes(x = Spending_per_Capita, y = Prozent, color = Country)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Korrelation zwischen Haushaltsausgaben und Breitbandzugang",
       x = "Haushaltsausgaben pro Einwohner",
       y = "Breitbandzugang (%)") +
  theme_minimal()


###Regression################################################################################################

# Regression für Breitbandzugang
# Für jedes Land ein Modell erstellen und Vorhersagen berechnen
# Funktion zur Modellierung und Vorhersage
predict_for_country <- function(country_data) {
  model <- lm(Prozent ~ Year, data = country_data)
  predict(model, newdata = data.frame(Year = c(2023, 2024)))
}

# Daten für jedes Land gruppieren
predicted_data_internet <- internetaccess_long %>%
  group_by(Country) %>%
  do(data.frame(Country = .$Country[1],
                Year = c(2023, 2024),
                Prozent = predict_for_country(.)))


#in dataframe umwandeln
internetaccess_long <- as.data.frame(internetaccess_long)
#prozent als numeric statt als char für die weitere Verarbeitung
internetaccess_long <- internetaccess_long %>%
  mutate(Prozent = as.numeric(Prozent))

#Vorhersage für 2023 und 2024 hinzufügen
internetaccess_long_extended <- bind_rows(internetaccess_long, predicted_data_internet)
#liste wieder sortieren
internetaccess_long_extended <-internetaccess_long_extended %>%
  arrange("Country")

visualisierung_internetzugang(internetaccess_long_extended)



#Model erstellen
predict_for_country_spending <- function(country_data) {
  model_spending <- lm(Spending_per_Capita ~ Year, data = country_data)
  model_normalized <- lm(Normalized_Spending_per_Capita ~ Year, data = country_data)
  #predict(model, newdata = data.frame(Year = c(2023, 2024)))
  
  predictions_normalized <- predict(model_normalized, newdata = data.frame(Year = c(2023, 2024)))
  predictions_spending <- predict(model_spending, newdata = data.frame(Year = c(2023, 2024)))

  data.frame(
    Year = c(2023, 2024),
    Normalized_Spending_per_Capita = predictions_normalized,
    Spending_per_Capita = predictions_spending
  )
 
}
# Daten für jedes Land gruppieren und Model anwenden
predicted_data_spending <- data_long %>%
  group_by(Country) %>%
  do({
    predictions <- predict_for_country_spending(.)
    data.frame(
      Country = .$Country[1],
      Year = predictions$Year,
      Normalized_Spending_per_Capita = predictions$Normalized_Spending_per_Capita,
      Spending_per_Capita = predictions$Spending_per_Capita
    )
  })
# Daten für jedes Land gruppieren und Model anwenden
#predicted_data_spending <- data_long %>%
 # group_by(Country) %>%
  #do(data.frame(Country = .$Country[1],
             #   Year = c(2023, 2024),
              #  Normalized_Spending_per_Capita = predict_for_country_spending(.)))

#Vorhersage für 2023 und 2024 hinzufügen
data_long_extended <- bind_rows(data_long, predicted_data_spending)
#Haushaltsausgaben mit den Vorhersagewerten visualisieren
visualisierung_haushaltsausgaben(data_long_extended)

####################Preis Berechnung########################################################################
#Preis der APP in Deutschland
price_de <- 12

#Spending per Capita DE in Variable speichern
Spending_per_CapitaDE <- data_long_extended %>%
  filter(Country == "Germany" & Year == 2022) %>%
  pull(Spending_per_Capita)
str(Spending_per_CapitaDE)

#Prozentueller Anteil des Apppreises an den Haushaltsausgaben einer Person in DE
AnteilDE <- (price_de / Spending_per_CapitaDE)*100


# FÜr Jedes Land den entsprechenden Preis berechnen um den selben Anteil
# an den Householdspendings darzustellen
  data_long_extended <- data_long_extended %>%
  mutate(App_Price = ifelse(Year == 2024, Spending_per_Capita * (AnteilDE / 100), NA))
  
  #Preise in eigenem Dataframe speichern
  Price_Country <- data_long_extended %>%
    filter(Year == 2024) %>%
    select(Country, App_Price)

  #Visualisierung in Html Tabelle
  Price_Country %>%
    select(Country, App_Price) %>%  # Wähle die relevanten Spalten aus
    kable("html", col.names = c("Land", "App Preis (EUR)"), digits = 2) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"), # Stiloptionen für die Tabelle
      full_width = FALSE,   # Tabelle nicht auf volle Breite strecken
      position = "left"     # Tabelle linksbündig ausrichten
    ) %>%
    column_spec(2, bold = TRUE, color = "darkblue")  # Formatierung der Preisspalte
  
  
#InternetAccessDeutschland Prozentsatz des Internetacces in Deutschland in Variable speichern
IADE <- internetaccess_long_extended %>%
  filter(Country == "Germany" & Year == 2024) %>%
  select(Prozent)

#Toleranzfeld festlegen
price_tolerance <- 0.15
price_min <- price_de * (1 - price_tolerance)
price_max <- price_de * (1 + price_tolerance)


similar_price_countries <- Price_Country %>%
  filter(App_Price >= price_min & App_Price <= price_max)



# Füge Internetzugangsdaten hinzu
similar_price_countries_with_internet <- similar_price_countries %>%
  left_join(internetaccess_long_extended %>% filter(Year == 2024), by = "Country") %>%
  filter(Prozent >= IADE)

# Wähle relevante Spalten für die Ausgabe
result <- similar_price_countries_with_internet %>%
  select(Country, App_Price, Prozent)%>%
  head(10)#Die 10 besten anzeigen


#Visualisierung in Html Tabelle
result %>%
  select(Country, App_Price, Prozent) %>%  # Wähle die relevanten Spalten aus
  kable("html", col.names = c("Land", "App Preis (EUR)","Internetzugang (Prozent)"), digits = 2) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"), # Stiloptionen für die Tabelle
    full_width = FALSE,   # Tabelle nicht auf volle Breite strecken
    position = "left"     # Tabelle linksbündig ausrichten
  ) %>%
  column_spec(2, bold = TRUE, color = "darkblue")  # Formatierung der Preisspalte
