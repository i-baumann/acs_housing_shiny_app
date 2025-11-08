# Student Housing Affordability App: Data Extraction

library(shiny)
library(tidyverse)
library(tidycensus)
library(leaflet)
library(tigris)
library(janitor)
library(sf)
library(shinydashboard)
library(plotly)
library(scales)

#####
# Define state, county, metro area/city of interest

st <- 'WI'
ct <- 'Dane'
cty <- 'Madison'

#####
# Query API to get most recent year available
current_year <- year(Sys.time())

year_attmpt <- try(
  get_acs(
    state = 'WI',
    county = 'Dane',
    geography = 'tract',
    survey = 'acs5',
    year = 2023,
    variables = 'B14001_008',
    geometry = FALSE
  )
)

max_yr <- ifelse(is.data.frame(year_attmpt),
                 current_year - 1,
                 current_year - 2)

#####
# Set levels of choice variables for user input
acs_years <- seq(2023, max_yr - 1, 1) # seq(2020, 2021, 1)
acs_years_micro <- acs_years[which(acs_years != 2020)]
student_type <- c('Undergraduate', 'Grad/Professional')

#####
# Pull data

### Student population estimates

student_estimates <- NULL
puma_shapefiles <- NULL
microdata_1_yr <- NULL
microdata_5_yr <- NULL

### Set variables to query for microdata
vars <- c(puma = 'PUMA',
          sex = 'SEX', 
          ten = 'TEN',
          p_age = 'AGEP', 
          hh_type = 'HHT', 
          grade_att = 'SCHG', 
          p_rent = 'RNTP', 
          rent_flag = 'FRNTP',
          p_total_income = 'PINCP',
          wage_p = 'WAGP', 
          hh_income = 'HINCP',
          p_inc_poverty_ratio = 'POVPIP',
          adj_income_factor = 'ADJINC', 
          p_rent_income_prop = 'GRPIP', 
          food_stamp = 'FS',
          health_ins_assistance = 'HINS4',
          p_public_asst_income = 'PAP'
)

for (yr in acs_years) {
  
  # ACS 5-year estimates
  estimates_temp <- get_acs(
    state = st,
    county = ct,
    geography = "tract",
    survey = 'acs5',
    variables = c(
      # Total and student population
      'B14001_008', # total undergrads enrolled
      'B14001_009', # total grads enrolled
      'B14001_010', # total not enrolled
      'B01001_001', # total pop
      # Rental beds
      'B25042_010', # 0 bedrooms
      'B25042_011', # 1 bed
      'B25042_012', # 2 beds
      'B25042_013', # 3 beds
      'B25042_014', # 4 beds
      'B25042_015', # 5+ beds
      # Estimated median rents
      'B25031_002', # 0 beds
      'B25031_003', # 1 bed
      'B25031_004', # 2 beds
      'B25031_005', # 3 beds
      'B25031_006', # 4 beds
      'B25031_007', # 5+ beds
      # Poverty level estimates
      'B14006_011', # unenrolled below PL past 12 mo.
      'B14006_009', # undergrads below PL past 12 mo.
      'B14006_010' # grads below PL past 12 mo.
    ),
    geometry = TRUE,
    year = yr,
    output = 'wide'
  ) |>
    rename(
      'total_est' = B01001_001E,
      'total_moe' = B01001_001M,
      'undergrad_est' = B14001_008E,
      'undergrad_moe' = B14001_008M,
      'grad_est' = B14001_009E,
      'grad_moe' = B14001_009M,
      'unenrolled_est' = B14001_010E,
      'unenrolled_moe' = B14001_010M,
      'bed_0_est' = B25042_010E,
      'bed_0_moe' = B25042_010M,
      'bed_1_est' = B25042_011E,
      'bed_1_moe' = B25042_011M,
      'bed_2_est' = B25042_012E,
      'bed_2_moe' = B25042_012M,
      'bed_3_est' = B25042_013E,
      'bed_3_moe' = B25042_013M,
      'bed_4_est' = B25042_014E,
      'bed_4_moe' = B25042_014M,
      'bed_5p_est' = B25042_015E,
      'bed_5p_moe' = B25042_015M,
      'rent_0_est' = B25031_002E,
      'rent_0_moe' = B25031_002M,
      'rent_1_est' = B25031_003E,
      'rent_1_moe' = B25031_003M,
      'rent_2_est' = B25031_004E,
      'rent_2_moe' = B25031_004M,
      'rent_3_est' = B25031_005E,
      'rent_3_moe' = B25031_005M,
      'rent_4_est' = B25031_006E,
      'rent_4_moe' = B25031_006M,
      'rent_5p_est' = B25031_007E,
      'rent_5p_moe' = B25031_007M,
      'unenrolled_pl_est' = B14006_011E,
      'unenrolled_pl_moe' = B14006_011M,
      'undergrad_pl_est' = B14006_009E,
      'undergrad_pl_moe' = B14006_009M,
      'grad_pl_est' = B14006_010E,
      'grad_pl_moe' = B14006_010M
    ) |>
    mutate(year = yr)
  
  student_estimates <- student_estimates |>
    bind_rows(estimates_temp)
  
  ## PUMA microdata
  ## Microdata are not available for the 2020 ACS due to low response rates
  
  if(yr != 2020){
    
    # Get PUMA shapefiles
    puma_temp <- pumas(state = st, 
                       cb = TRUE, 
                       year = 2020
    ) |> 
      clean_names() |> 
      filter(str_detect(namelsad20, ct) | str_detect(namelsad20, cty)) |> # Used to be name10
      mutate(year = yr)
    
    puma_shapefiles <- puma_shapefiles |>
      bind_rows(puma_temp)
    
    # Get microdata: ACS 1 Yr
    micro_1_temp <- get_pums(
      variables = vars,
      state = st,
      puma = unique(puma_temp$pumace10),
      survey = 'acs1',
      year = yr
    ) |> 
      mutate(SPORDER = as.numeric(SPORDER),
             year = yr)
    
    microdata_1_yr <- microdata_1_yr |>
      bind_rows(micro_1_temp)
    
    # Get microdata: ACS 5 Yr
    micro_5_temp <- get_pums(
      variables = vars,
      state = st,
      puma = unique(puma_temp$pumace10),
      survey = 'acs5',
      year = yr
    ) |> 
      mutate(SPORDER = as.numeric(SPORDER),
             year = yr)
    
    microdata_5_yr <- microdata_5_yr |>
      bind_rows(micro_5_temp)
    
  }
  
}

# Clean wide-form ACS estimates data
student_estimates_wide <- student_estimates |>
  clean_names() |> 
  mutate(est_rental_capacity = bed_0_est * 1 +
           bed_1_est * 1 +
           bed_2_est * 2 +
           bed_3_est * 3 +
           bed_4_est * 4 +
           bed_5p_est * 5,
         student_rental_coverage = (est_rental_capacity / (undergrad_est + grad_est)) * 100,
         total_rental_coverage = (est_rental_capacity / total_est) * 100,
  )

# Clean and pivot data ACS estimates for mapping
student_estimates_long <- student_estimates |>
  clean_names() |> 
  mutate(
    tract = str_extract(name, "(?<=Census )[^,]*(?=,)"),
    pct_students = case_when(
      total_est == 0 ~ 0,
      TRUE ~ ((undergrad_est + grad_est) / total_est) * 100
    ),
    pct_undergrad = case_when(
      total_est == 0 ~ 0,
      TRUE ~ (undergrad_est / total_est) * 100
    ),
    pct_grad = case_when(
      total_est == 0 ~ 0,
      TRUE ~ (grad_est / total_est) * 100
    ),
    pct_undergrad_below_pl = 
      case_when(
        total_est == 0 ~ 0,
        TRUE ~ (undergrad_pl_est / total_est) * 100
      ),
    pct_students_undergrad_below_pl = 
      case_when(
        total_est == 0 ~ 0,
        TRUE ~ (undergrad_pl_est / undergrad_est) * 100
      ),
    pct_grad_below_pl = 
      case_when(
        total_est == 0 ~ 0,
        TRUE ~ (grad_pl_est / total_est) * 100
      ),
    pct_students_grad_below_pl = 
      case_when(
        total_est == 0 ~ 0,
        TRUE ~ (grad_pl_est / grad_est) * 100
      )
  ) |> 
  pivot_longer(cols = !c(year, tract, geoid, geometry, name), 
               names_to = 'var', 
               values_to = 'val')

# Clean microdata and join in PUMA shapefiles
microdata_1_yr <- microdata_1_yr |> 
  clean_names() |> 
  left_join(puma_shapefiles |>
              select(year, pumace20, namelsad20, -geometry),
            by = c('puma' = 'pumace20', 'year' = 'year')
  )

microdata_5_yr <- microdata_5_yr |> 
  clean_names() |> 
  left_join(puma_shapefiles |>
              select(year, pumace20, namelsad20, -geometry),
            by = c('puma' = 'pumace20', 'year' = 'year')
  )

# Save out data
saveRDS(student_estimates_long, file = 'student_estimates_long.rds')
saveRDS(student_estimates_wide, file = 'student_estimates_wide.rds')
saveRDS(microdata_1_yr, file = 'microdata_1_yr.rds')
saveRDS(microdata_5_yr, file = 'microdata_5_yr.rds')
