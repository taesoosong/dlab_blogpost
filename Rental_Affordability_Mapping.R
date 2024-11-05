############################################################
# Title: Mapping rental affordability in SF Bay Area
# Author: Taesoo Song
# Date: 11/04/2024
############################################################

# install.packages("tidycensus") Unhashtag and download the package if you haven't installed tidycensus yet.

library(pacman) # `pacman` package allows you to load multiple libraries easily using p_load
p_load(tidycensus, sf)

# Load rent data at the census tract-level
tract_data <- get_acs(variables =
                        c("median_rent" = "B25064_001", # median gross rent
                          "median_income" = "B19013_001", # median household income
                          "pop_total" = "B03002_001",
                          "pop_white" = "B03002_003" 
                          ),
                      geography = "tract",
                      state = "California",
                      county = c("Alameda", "Contra Costa", "Marin",
                                 "Napa", "San Francisco", "San Mateo",
                                 "Santa Clara", "Solano", "Sonoma"),
                      year = 2022,
                      survey = "acs5",
                      geometry = TRUE)

# Check data structure
head(tract_data)

# Pivot the dataframe
tract_data <- tract_data |>
  select(-moe) |> # Remove the margin of error column
  pivot_wider(names_from = "variable",
              values_from = "estimate")

print(tract_data)

map_median_rent <- tract_data |>
  ggplot() + geom_sf(aes(fill=median_rent))

print(map_median_rent)

map_median_rent2 <- tract_data |>
  ggplot() + 
  geom_sf(color = NA, aes(fill = median_rent)) +
  scale_fill_gradientn(
    colors = c("lightgreen", "green", "yellow", "orange", "red"), 
    labels = scales::dollar_format()
  ) +
  labs(
    title = "Median Rent by Census Tract (2022)",
    subtitle = "San Francisco Bay Area",
    fill = "Median Rent",
    caption = "Data: American Community Survey (2018-2022)"
  ) +
  theme_void()

map_median_rent2

tract_data <- tract_data |>
  mutate(rent_to_income = median_rent/(median_income/12))

tract_data |>
  ggplot() + geom_histogram(aes(x=rent_to_income))

tract_data <- tract_data |>
  mutate(rent_to_income_grouped = case_when(
    rent_to_income < 0.2 ~ "< 20%",
    rent_to_income < 0.3 ~ "< 30%",
    rent_to_income < 0.4 ~ "< 40%",
    rent_to_income >= 0.4 ~ ">= 40%",
  ))

map_affordability_ratio <- tract_data |>
  ggplot() + geom_sf(color=NA) + aes(fill=rent_to_income_grouped) +
  scale_fill_manual(
    values = c("< 20%" = "green", "< 30%" = "yellow", "< 40%" = "orange", ">= 40%" = "red")) +
  labs(title = "Median rent to Median monthly household income ratio\nby census tracts in SF Bay Area",
       fill = "Ratio",
       caption = "Data: American Community Survey (2018-2022)") +
  theme_void() 

print(map_affordability_ratio)