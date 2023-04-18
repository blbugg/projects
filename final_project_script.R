
# load packages

install.packages("zoo")
install.packages("stringr")
install.packages("geojsonio")
install.packages("broom")
install.packages("rjson")
install.packages("png")
install.packages("pandoc")

library(tidyverse)
library(easystats)
library(dplyr)
library(janitor)
library(leaps)
library(vroom)
library(zoo)
library(skimr)
library(stringr)
library(geojsonio)
library(broom)
library(rjson)
library(png)
library(pandoc)


# read in the data

df <- vroom("GSAF5_03:2023.csv")
aus_df <- vroom("australian_shark_incident_database_public.csv")

problems(aus_df)

#cleaning the data

df_1 <- df[, -16:-255] %>% clean_names()


df_2 = select(df_1, -investigator_or_source, -time, -injury,
              -sex, -name, -age, -location)

# fifty_years <- dplyr::filter(df_2, year > 1952)
# hundred_years <- dplyr::filter(df_2, year > 1922)

df_2 <- arrange(country) %>% 
  pull(country) %>% 
  str_to_lower()

#australia <- dplyr::filter(hundred_years, country %in% c("AUSTRALIA")) %>% 
  #dplyr::group_by(year) %>% arrange(year, .by_group=TRUE)

#fishing
df_2 <- df_2 %>%
  mutate(activity = ifelse(grepl("Fishing", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("fishing", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("Fisherman", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("Fish", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("fish", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("Spearfishing", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("spearfishing", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("sardines", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("Clamming", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("Crabbing", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("shrimp", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("lobsters", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("whale", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("Hunting", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("hunting", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("Lobstering", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("Netting", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("netting", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("Net", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("net", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("prawns", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("Shrimping", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("dolphins", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("crocodile", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("turtle", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("crabs", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("stingrays?", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("Oystering", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("pêcheur de bichiques", activity), "fishing", activity)) %>% 
  mutate(activity = ifelse(grepl("Spearfishing", activity), "fishing", activity))

table(df_2$activity)

#swimming_or_diving

df_2 <- df_2 %>% 
  mutate(activity = ifelse(grepl("naked", activity), "swimming_or diving", activity)) %>% 
  mutate(activity = ifelse(grepl("poaching", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("towing a kayak", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("towing an empty barrel", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("carnival", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("floatioon", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Medic", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("house on pilings", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("swimming with boogie board", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("swimming with board", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Swimmingq", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Swimming/Standing", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Swimming/ Treading water", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Swimming/", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("caught in strong backwash", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("carrying tin can with mail", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("after falling", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Swmming", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("dived into the water", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Swimming", activity), "swimming_or_diving", activity)) %>%
  mutate(activity = ifelse(grepl("Swimming", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Wading?", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Swimming", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Treading water", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Treading for clams", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Diving", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Cage Diving", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Trochus diving", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("swimming", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Tech diving", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Freediving", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Free-diving", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Free diving", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Dog paddling", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Dived", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Crouching", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Hardhat diving", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Hard hat diving", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Helmet diving", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Hookah diving", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("waist-deep water", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Jumped into", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Jumped off rocks", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Jumping", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Kneeling", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Lying in 2 feet", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Lying prone", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Night bathing", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Night diving", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Pearl diving", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Playing", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Scuba diving", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Crouching", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("SCUBA", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Shark diving", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Shell diving", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Sitting", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Sittting", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Skin diving", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Skindiving", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Snorkeling", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Splashing", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Sponge", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Squatting", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Standing in knee-deep", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Standing in waist-deep", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Standing in water", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Washing", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Floating or standing", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Floating on his back", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Floating on back", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Fell into the water", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Fell from his", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Fell from wharf", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("Dangling feet", activity), "swimming_or_diving", activity)) %>% 
  mutate(activity = ifelse(grepl("chest-deep water", activity), "swimming_or_diving", activity))
  

table(df_2$activity)

#air_or_sea_disaster

df_2 <- df_2 %>% 
  mutate(activity = ifelse(grepl("German submarine", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("USS", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("C124", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("U-177", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("troopship", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Geiser", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Bonnie Dundee", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Greycliffe", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("SS Potlach", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Trashman", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Gooney Bird", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("982-ton", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Michael Howell", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("yachting accident", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Wreck", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Esperanza", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Kapuna", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Dona Marilyn", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("3540-ton", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("426-ton", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("500-ton", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("6711-ton", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("240-ton", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Permina", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Vessel", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Two canoes", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Sinking", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Shipwrecked", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Tahitienne", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Mosli", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("schooner Elizabeth", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Vula", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Dwarka", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Cutty Sark", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Pioneer Cebu", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("bailing out of jet", activity), "air_or_sea_disaster", activity)) %>%
  mutate(activity = ifelse(grepl("Columbus", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Sea Disaster", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Sea disaster", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Ramos", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("S2F-1", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("S2N", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("DC-6B", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("R5D", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Portuguese Airliner", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Plane forced", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("powerboat", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("DC7B", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Baby Princesa", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Rebel Belle", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Light aircraft", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Battle of the Bismarck", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Bokuyo Maru", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Principessa", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Camperdown", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("U-559", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Duncan DD", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Arisan Maru", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("light cruiser", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("La Seyne", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Caribou", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Caribbee", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Belmore", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("9 m launch", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("destroyer", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Raytheon", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Shipwreck", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("William Penn", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("torpedoed", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Schooner sank", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Sagay", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Plane crashed", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("cable ship", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("blackbirding", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Explosion", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Dona Paz", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Ferry", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Ditched", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Avianca", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Rio Atrato", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Woodvale", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Copra", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Crossing inlet", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Cutter", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Nazarene", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Ejected", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Fell overboard", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Fell oveboard", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Fell or jumped", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Yacht", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("yachtsman", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("deckhand", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Went overboard", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Swept off", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Survived US Naval", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("C-46", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Steamer", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Sitting in bow", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Rizal", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Ship's boat capsized", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Ship lay at anchor", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("torpedoes &", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Sailing on catamaran", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Salvaging", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Sulphur Queen", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Rowing", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Rolled off", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("ship sunk", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Painting", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Paddling rescue", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Paddling outrigger", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Paddling an outrigger", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Paddling a canoe", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Paddling & sailing", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Pacific Seafarer", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Overturned", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("life raft", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("On inflatable raft", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("earthquake", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("On boat", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Ocean racing", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("native boats", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("fell overboard", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Tropical sank", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Lifeboat capsized", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Knocked overboard", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Jumped overboard", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("over side of boat", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Watercraft", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Washed overboard", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Huncliff", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Washed off catamaran", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Washed off raft", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Boat", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("boat", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Boat exploded", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Boating", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Boeing", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Aircraft", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Air Disaster", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Air disaster", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Mormackite", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Orator", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Argentine", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Arsinoe", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("B-24", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Boarding a ship", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Missouri", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("C47", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Macedon", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Canoe", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Canoeing", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("canoeing", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Dara", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("El Gamil", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("cargo ship", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Clinging", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("210-ton", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("barque", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("a canoe", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("dhow capsized", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("survivors on a raft", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Abandoning", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("dragged overboard", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("thrown overboard", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Adrift", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Cutter", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Dropped overboard", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Crossing river", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Fell from the", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Fell from yardarm", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Lakonia", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Dorsetshire", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Swept", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Penang's", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("New Venture", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("catamaran capsized", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Happy Jack", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Chieh Lee", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Irene", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("European civilians", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Aircraft exploded", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("shipwrecked", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Fleet of canoes", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Mezada", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Kormoran", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Sailing", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Gambier", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Hoel", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Vessel", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("C124", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Airforce crewman", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("U-177", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Two canoes", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Transatlantic", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Torpedoed", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Thrown overboard", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Thrown from destroyer", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("25,000-ton", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Mosli", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Esperanza", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Wahoo", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Britannia", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Thingvalla", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Bonnie Dundee", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Greycliffe", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Christie", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("San Basilio", activity), "air_or_sea_disaster", activity)) %>% 
  mutate(activity = ifelse(grepl("Skakel", activity), "air_or_sea_disaster", activity)) 
  

#surfing

table(df_2$activity)

df_2 <- df_2 %>%
  mutate(activity = ifelse(grepl("Surf", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Windsurfing", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Wind surfing", activity), "surfing", activity)) %>%
  mutate(activity = ifelse(grepl("Water-skiing", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Wakeboarding", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Lying on surfboard", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("surfboard", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Paddle Boarding", activity), "surfing", activity)) %>%
  mutate(activity = ifelse(grepl("Paddle boarding", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Paddle Skiing", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Paddle-boarding", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Paddle-surfing", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Paddleboarding", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Paddleskiing", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Paddling on kneeboard", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Paddle on surfboard", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Boggie", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Boogie boarding", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Boogie Boarding", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Bodyboarding", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Body-boarding", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Body surfing", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Body-surfing", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Bodysurfing", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Body boarding", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Body Boarding", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Kite boarding", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Kite Boarding", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Kite surfing", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Kite-Boarding", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Kiteboarding", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Kitesurfing", activity), "surfing", activity)) %>% 
  mutate(activity = ifelse(grepl("Surfing", activity), "surfing", activity))
  
  
#snorkel and scuba?
#sheer_stupidity

# south_africa <- dplyr::filter(hundred_years, country %in% c("SOUTH AFRICA"))

# reunion <- dplyr::filter(hundred_years, country %in% c("REUNION ISLAND"))

# usa <- dplyr::filter(hundred_years, country %in% c("USA"))

# geojson map

temp_shapefile <- tempfile()
download.file("https://github.com/gregoiredavid/france-geojson/blob/master/regions/la-reunion/communes-la-reunion.geojson", temp_shapefile)
unzip(temp_shapefile)

?toJSON

reunion_geojson <- geojson_read("https://github.com/gregoiredavid/france-geojson/blob/master/regions/la-reunion/communes-la-reunion.geojson")



