
# 0) PACKAGE SETUP
required_pkgs <- c(
  "tidyverse","lubridate","janitor","skimr","naniar",
  "broom","infer","tidymodels","ggplot2","DescTools","patchwork"
)
to_install <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(required_pkgs, library, character.only = TRUE))

theme_set(theme_minimal())



install.packages("nycflights13")
library(nycflights13) # import nycllights13

data ("flights")
str(flights) # view the structure of flights

ggplot (flights, aes(x = dep_delay, у = arr_delay, color = carrier)) + geom_point_v(alpha = 0.4) # Change the points color by carrier
# Set transparency to 0.4

#find the average of air_time for different carrier

flights %>%
  group_by(carrier) %>%
  summarise(average_air_time = mean (air_time, na.rm = TRUE)) %>%
  filter (average_air_time > 10)



# 1) DATA LOAD USING RELATIVE PATHS
data_dir <- "/Users/moody/Desktop/APU - Ai/2-Year/3-Sem/R programming/GROUP ASSIGNMENT DFDA/Flight-Dataset/"
flights_path <- file.path(data_dir, "flights.csv")
airlines_path <- file.path(data_dir, "iata_airline_codes.csv")
airports_path <- file.path(data_dir, "iata_airport_codes.csv")

flights <- read_csv(flights_path, show_col_types = FALSE) %>% clean_names()
airlines <- if (file.exists(airlines_path)) read_csv(airlines_path, show_col_types = FALSE) %>% clean_names() else NULL
airports <- if (file.exists(airports_path)) read_csv(airports_path, show_col_types = FALSE) %>% clean_names() else NULL

names(flights) <- toupper(names(flights))




# 2) STRUCTURAL AUDIT
flights <- flights %>%
  mutate(
    AIRLINE = as.factor(AIRLINE),
    ORIGIN_AIRPORT = as.factor(ORIGIN_AIRPORT),
    DESTINATION_AIRPORT = as.factor(DESTINATION_AIRPORT),
    DIVERTED = as.factor(DIVERTED),
    CANCELLED = as.factor(CANCELLED),
    CANCELLATION_REASON = as.factor(ifelse(CANCELLATION_REASON == "", "None", CANCELLATION_REASON))
  )

glimpse(flights)
skimr::skim(flights)


# Cardinality & frequency for key categoricals
if ("AIRLINE" %in% names(flights)) print(count(flights, AIRLINE, sort = TRUE))
if ("ORIGIN_AIRPORT" %in% names(flights)) print(count(flights, ORIGIN_AIRPORT, sort = TRUE))
if ("DESTINATION_AIRPORT" %in% names(flights)) print(count(flights, DESTINATION_AIRPORT, sort = TRUE))




#-----------study ORIGIN_AIRPORT and DESTINATION problem----------------
# Find unique values that contain only numbers
unique_numbers <- unique(grep("^[0-9]+$", flights$ORIGIN_AIRPORT, value = TRUE))

# Find unique values that contain three uppercase letters
unique_iata <- unique(grep("^[A-Z]{3}$", flights$ORIGIN_AIRPORT, value = TRUE))

# See what other non-standard entries exist
non_standard <- unique(grep("[^A-Z0-9]", flights$ORIGIN_AIRPORT, value = TRUE))

print(paste("Unique numeric codes:", paste(unique_numbers, collapse = ", ")))
print(paste("Unique IATA codes:", paste(unique_iata, collapse = ", ")))

# Calculate the proportion of affected rows
is_wmo <- !grepl("^[A-Z]{3}$", flights$ORIGIN_AIRPORT)
total_flights <- nrow(flights)
flights_with_error <- sum(is_wmo)
percentage_error <- (flights_with_error / total_flights) * 100
print(paste("Percentage of rows with numeric codes:", round(percentage_error, 2), "%"))

# Correlate the issue with time
table(flights[is_wmo, c("YEAR", "MONTH")])

# Correlate the issue with airlines
table(flights[is_wmo, "AIRLINE"])



clean_flights <- function(flights_df) {
  

  # Deduplication
  # -------------------------
  delay_cols <- c("AIR_SYSTEM_DELAY","SECURITY_DELAY","WEATHER_DELAY",
                  "LATE_AIRCRAFT_DELAY","AIRLINE_DELAY")
  
  flights_df <- flights_df %>%
    mutate(non_na_delay_count = rowSums(!is.na(across(all_of(delay_cols))))) %>%
    arrange(desc(non_na_delay_count),
            DEPARTURE_TIME,
            ARRIVAL_TIME,
            TAIL_NUMBER) %>%
    distinct(YEAR, MONTH, DAY, FLIGHT_NUMBER, SCHEDULED_DEPARTURE, .keep_all = TRUE) %>%
    select(-non_na_delay_count)
  

  # Handle cancelled/diverted flights
  # -------------------------
  flights_df <- flights_df %>%
    mutate(
      ARRIVAL_DELAY   = if_else(CANCELLED == 1 | DIVERTED == 1, NA_real_, ARRIVAL_DELAY),
      DEPARTURE_DELAY = if_else(CANCELLED == 1 | DIVERTED == 1, NA_real_, DEPARTURE_DELAY),
      AIR_TIME        = if_else(CANCELLED == 1 | DIVERTED == 1, NA_real_, AIR_TIME),
      ELAPSED_TIME    = if_else(CANCELLED == 1 | DIVERTED == 1, NA_real_, ELAPSED_TIME),
      rootcause_flag  = case_when(
        CANCELLED == 1 ~ "Cancelled → structural missingness",
        DIVERTED  == 1 ~ "Diverted → structural missingness",
        TRUE           ~ NA_character_
      )
    )
  

  # Safe HHMM → POSIXct conversion
  # -------------------------
  hhmm_to_posix_vec <- function(date, hhmm) {
    valid <- !is.na(date) & !is.na(hhmm)
    out <- as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")
    out[valid] <- ymd(date[valid]) +
      hours(floor(hhmm[valid] / 100)) +
      minutes(hhmm[valid] %% 100)
    out
  }
  
  flights_df <- flights_df %>%
    mutate(flight_date = make_date(YEAR, MONTH, DAY)) %>%
    mutate(
      SCHEDULED_DEPARTURE = hhmm_to_posix_vec(flight_date, SCHEDULED_DEPARTURE),
      DEPARTURE_TIME      = hhmm_to_posix_vec(flight_date, DEPARTURE_TIME),
      WHEELS_OFF          = hhmm_to_posix_vec(flight_date, WHEELS_OFF),
      WHEELS_ON           = hhmm_to_posix_vec(flight_date, WHEELS_ON),  
      SCHEDULED_ARRIVAL   = hhmm_to_posix_vec(flight_date, SCHEDULED_ARRIVAL),
      ARRIVAL_TIME        = hhmm_to_posix_vec(flight_date, ARRIVAL_TIME)
    ) %>%
    select(-flight_date)
  

  # Handle rollover (arrival after midnight)
  # -------------------------
  flights_df <- flights_df %>%
    mutate(
      ARRIVAL_TIME = if_else(
        !is.na(DEPARTURE_TIME) & !is.na(ARRIVAL_TIME) & ARRIVAL_TIME < DEPARTURE_TIME,
        ARRIVAL_TIME + days(1),
        ARRIVAL_TIME
      ),
      rootcause_flag = if_else(
        !is.na(DEPARTURE_TIME) & !is.na(ARRIVAL_TIME) & ARRIVAL_TIME < DEPARTURE_TIME,
        "Rollover not encoded in raw data",
        rootcause_flag
      )
    )
  

  # Validate elapsed time
  # -------------------------
  flights_df <- flights_df %>%
    mutate(
      computed_elapsed = as.numeric(difftime(ARRIVAL_TIME, DEPARTURE_TIME, units = "mins")),
      elapsed_diff     = computed_elapsed - ELAPSED_TIME,
      rootcause_flag   = if_else(
        !is.na(elapsed_diff) & abs(elapsed_diff) > 10,
        "Elapsed time mismatch",
        rootcause_flag
      )
    )
  
  return(flights_df)
}



# -------------------------
# Run cleaning pipeline
# -------------------------
flights <- clean_flights(flights)

# -------------------------
# Post-cleaning validation
# -------------------------

# Flag flights with elapsed mismatches > 15 min
flights <- flights %>%
  mutate(
    elapsed_flag = case_when(
      is.na(computed_elapsed) ~ "No elapsed (NA)",
      abs(elapsed_diff) <= 15 ~ "OK",
      abs(elapsed_diff) > 15 & abs(elapsed_diff) <= 180 ~ "Moderate mismatch",
      abs(elapsed_diff) > 180 ~ "Severe mismatch"
    )
  )

flights %>%
  count(elapsed_flag) %>%
  mutate(prop = round(n / sum(n), 3))

# Winsorize elapsed_diff at ±180 minutes
winsorize_elapsed <- function(x, limit = 180) {
  pmax(pmin(x, limit), -limit)
}

flights <- flights %>%
  mutate(
    elapsed_diff_winsor = winsorize_elapsed(elapsed_diff, 180),
    ELAPSED_TIME = computed_elapsed - elapsed_diff_winsor
  )


  
  

# Map cancellation reasons
if ("CANCELLATION_REASON" %in% names(flights)) {
  flights <- flights %>% mutate(
    CANCELLATION_REASON = recode(CANCELLATION_REASON,
                                 "A"="Airline/Carrier",
                                 "B"="Weather",
                                 "C"="National Air System",
                                 "D"="Security")
  )
}

# Winsorization for extreme delays
for (c in c("ARRIVAL_DELAY","DEPARTURE_DELAY","AIR_TIME","TAXI_OUT","TAXI_IN")) {
  if (c %in% names(flights)) {
    qs <- quantile(flights[[c]], probs = c(0.01,0.99), na.rm = TRUE)
    flights[[paste0(c,"_W")]] <- pmin(pmax(flights[[c]], qs[[1]]), qs[[2]])
  }
}

  
  # 3) AIRPORT CODE STANDARDIZATION
  # ---  Airport ID to IATA conversion ---
  Airport_ID_to_IATA <- function(x, airport_id_file = NULL, airport_file = NULL) {
    tryCatch({
      library(dplyr)
      
      # Helper: make duplicate descriptions unique
      make_unique_descriptions <- function(df, key_col) {
        df <- df %>%
          group_by(Description) %>%
          mutate(
            dup_count = row_number(),
            Description = ifelse(
              n() > 1,
              paste0(Description, " (", !!sym(key_col), ")"),
              Description
            )
          ) %>%
          select(-dup_count) %>%
          ungroup()
        return(df)
      }
      
      # --- Load lookup tables ---
      if (is.null(airport_id_file)) {
        airport_id_file <- "/Users/moody/Desktop/APU - Ai/2-Year/3-Sem/R programming/GROUP ASSIGNMENT DFDA/Flight-Dataset/L_AIRPORT_ID.csv"
      }
      dfAirportID <- read.csv(airport_id_file, colClasses = "character")[,1:2]
      names(dfAirportID) <- c("AirportID", "Description")
      dfAirportID <- make_unique_descriptions(dfAirportID, "AirportID")
      
      if (is.null(airport_file)) {
        airport_file <- "/Users/moody/Desktop/APU - Ai/2-Year/3-Sem/R programming/GROUP ASSIGNMENT DFDA/Flight-Dataset/L_AIRPORT.csv"
      }
      dfAirport <- read.csv(airport_file, colClasses = "character")[,1:2]
      names(dfAirport) <- c("IATA_CODE", "Description")
      dfAirport <- make_unique_descriptions(dfAirport, "IATA_CODE")
      
      # --- Conversion ---
      df <- data.frame(ID = as.character(x), rowid = seq_along(x), stringsAsFactors = FALSE)
      df <- df %>%
        left_join(dfAirportID, by = c("ID" = "AirportID")) %>%
        left_join(dfAirport, by = "Description") %>%
        mutate(FinalCode = ifelse(!is.na(IATA_CODE), IATA_CODE, ID)) %>%
        arrange(rowid)
      
      # Debug: show first 5 mappings
      cat("\nSample mappings:\n")
      print(head(df[, c("ID", "FinalCode")], 5))
      
      return(df$FinalCode)
      
    }, error = function(e) {
      message("Error in Airport_ID_to_IATA: ", e$message)
      return(x)  # fallback
    })
  }
  
  
  
  # Apply mapping
  flights <- flights %>%
    left_join(numeric_to_iata, by = c("ORIGIN_AIRPORT" = "numeric_code"), relationship = "many-to-many") %>%
    mutate(ORIGIN_AIRPORT = ifelse(!is.na(IATA), IATA, ORIGIN_AIRPORT)) %>%
    select(-IATA) %>%
    left_join(numeric_to_iata, by = c("DESTINATION_AIRPORT" = "numeric_code"), relationship = "many-to-many") %>%
    mutate(DESTINATION_AIRPORT = ifelse(!is.na(IATA), IATA, DESTINATION_AIRPORT)) %>%
    select(-IATA)
  
  # --- Apply conversion ---
  flights$ORIGIN_AIRPORT     <- Airport_ID_to_IATA(flights$ORIGIN_AIRPORT)
  flights$DESTINATION_AIRPORT<- Airport_ID_to_IATA(flights$DESTINATION_AIRPORT)
  
  # Look for any numeric-only airport codes
  numeric_origins <- unique(flights$ORIGIN_AIRPORT[grepl("^[0-9]+$", flights$ORIGIN_AIRPORT)])
  numeric_destinations <- unique(flights$DESTINATION_AIRPORT[grepl("^[0-9]+$", flights$DESTINATION_AIRPORT)])
  
  cat("Numeric ORIGIN codes left:", length(numeric_origins), "\n")
  cat("Numeric DESTINATION codes left:", length(numeric_destinations), "\n")
  
 
  
  
  glimpse(flights)
  skimr::skim(flights)

  
# 4) FEATURE ENGINEERING
flights <- flights %>%
  mutate(
    SEASON = case_when(
      MONTH %in% c(12,1,2) ~ "Winter",
      MONTH %in% c(3,4,5) ~ "Spring",
      MONTH %in% c(6,7,8) ~ "Summer",
      MONTH %in% c(9,10,11) ~ "Autumn",
      TRUE ~ NA_character_
    ),
    DELAYED_15 = if_else(ARRIVAL_DELAY > 15, 1L, 0L)
  )

# Airline reliability KPI
if (all(c("AIRLINE","ARRIVAL_DELAY","CANCELLED") %in% names(flights))) {
  airline_kpi <- flights %>%
    mutate(ON_TIME = if_else(ARRIVAL_DELAY <= 15 | is.na(ARRIVAL_DELAY), 1L, 0L)) %>%
    group_by(AIRLINE) %>%
    summarise(
      n = n(),
      ON_TIME_RATE = mean(ON_TIME, na.rm = TRUE),
      CANCEL_RATE = mean(replace_na(CANCELLED,0)==1, na.rm = TRUE),
      RELIABILITY_SCORE = ON_TIME_RATE - CANCEL_RATE
    ) %>% arrange(desc(RELIABILITY_SCORE))
  print(airline_kpi, n = 10)
}


# 5) DATA QUALITY & VALIDATION
# Missingness summary
naniar::miss_var_summary(flights) %>% print(n = 50)

# Duplicates: define a business key (adapt as needed)
key_cols <- intersect(c("YEAR","MONTH","DAY","AIRLINE","FLIGHT_NUMBER","ORIGIN_AIRPORT","DESTINATION_AIRPORT","SCHEDULED_DEPARTURE, DESTINATION_AIRPORT"), names(flights))
if (length(key_cols) > 0) {
  dupes <- flights %>% count(across(all_of(key_cols)), name="n") %>% filter(n>1)
  message("Duplicate rows detected: ", nrow(dupes))
}

# Logical validations (only run if columns exist)
# ARRIVAL consistency check
if (all(c("ARRIVAL_TIME","WHEELS_ON","TAXI_IN") %in% names(flights))) {
  flights <- flights %>%
    mutate(
      ARRIVAL_TIME_CHECK = WHEELS_ON + lubridate::minutes(TAXI_IN),
      ARRIVAL_DIFF_MIN   = as.numeric(difftime(ARRIVAL_TIME, ARRIVAL_TIME_CHECK, units = "mins"))
    )
  
  message("Mean abs diff ARRIVAL_TIME vs WHEELS_ON + TAXI_IN: ",
          round(mean(abs(flights$ARRIVAL_DIFF_MIN), na.rm = TRUE), 2), " minutes")
}

# DEPARTURE consistency check
if (all(c("DEPARTURE_TIME","WHEELS_OFF","TAXI_OUT") %in% names(flights))) {
  flights <- flights %>%
    mutate(
      DEPARTURE_TIME_CHECK = WHEELS_OFF - lubridate::minutes(TAXI_OUT),
      DEPARTURE_DIFF_MIN   = as.numeric(difftime(DEPARTURE_TIME, DEPARTURE_TIME_CHECK, units = "mins"))
    )
  
  message("Mean abs diff DEPARTURE_TIME vs WHEELS_OFF - TAXI_OUT: ",
          round(mean(abs(flights$DEPARTURE_DIFF_MIN), na.rm = TRUE), 2), " minutes")
}



# 6) EXPLORATORY ANALYSIS (EDA)

#  Mean arrival delay by month
if (all(c("MONTH","ARRIVAL_DELAY") %in% names(flights))) {
  monthly_delay <- flights %>%
    group_by(MONTH) %>%
    summarise(
      MEAN_ARR_DELAY = mean(ARRIVAL_DELAY, na.rm = TRUE),
      MEDIAN_ARR_DELAY = median(ARRIVAL_DELAY, na.rm = TRUE),
      N_FLIGHTS = n(),
      .groups = "drop"
    )
  print(monthly_delay, n=12)
}

#  Mean arrival delay by airline
if (all(c("AIRLINE","ARRIVAL_DELAY") %in% names(flights))) {
  airline_delay <- flights %>%
    group_by(AIRLINE) %>%
    summarise(
      MEAN_ARR_DELAY = mean(ARRIVAL_DELAY, na.rm=TRUE),
      MEDIAN_ARR_DELAY = median(ARRIVAL_DELAY, na.rm=TRUE),
      N_FLIGHTS = n(),
      .groups = "drop"
    ) %>%
    arrange(MEAN_ARR_DELAY)
  print(airline_delay, n=14)
}

#  Cancellation reasons
if (all(c("CANCELLATION_REASON","CANCELLED") %in% names(flights))) {
  cancel_summary <- flights %>%
    filter(CANCELLED == 1) %>%
    count(CANCELLATION_REASON) %>%
    mutate(perc = n/sum(n)*100)
  
  print(cancel_summary)
  
}


#  Busiest airports
if ("ORIGIN_AIRPORT" %in% names(flights)) {
  busiest_origins <- flights %>%
    count(ORIGIN_AIRPORT, sort=TRUE) %>%
    slice_head(n=10)
  print(busiest_origins)
}

if ("DESTINATION_AIRPORT" %in% names(flights)) {
  busiest_destinations <- flights %>%
    count(DESTINATION_AIRPORT, sort=TRUE) %>%
    slice_head(n=10)
  print(busiest_destinations)
}



# 7 ) Exploratory Visualization

#comparing raw vs winsorized delays
library(ggplot2)

flights_long <- flights %>%
  select(ARRIVAL_DELAY, ARRIVAL_DELAY_W, DEPARTURE_DELAY, DEPARTURE_DELAY_W) %>%
  pivot_longer(everything(), names_to="type", values_to="delay")

ggplot(flights_long, aes(x=type, y=delay)) +
  geom_boxplot() +
  coord_flip() +
  labs(title="Raw vs Winsorized Delays")


# Mean Arrival Delay by Month
if (all(c("MONTH","ARRIVAL_DELAY") %in% names(flights))) {
  flights %>%
    group_by(MONTH) %>%
    summarise(mean_arr_delay = mean(ARRIVAL_DELAY, na.rm = TRUE), .groups="drop") %>%
    ggplot(aes(MONTH, mean_arr_delay)) + geom_col() +
    labs(title="Mean Arrival Delay by Month", x="Month", y="Minutes")
}

# Mean Arrival Delay by Airline
if (all(c("AIRLINE","ARRIVAL_DELAY") %in% names(flights))) {
  flights %>%
    group_by(AIRLINE) %>%
    summarise(mean_arr_delay = mean(ARRIVAL_DELAY, na.rm=TRUE), .groups="drop") %>%
    slice_min(order_by = mean_arr_delay, n = 15) %>%
    ggplot(aes(reorder(AIRLINE, mean_arr_delay), mean_arr_delay)) +
    geom_col() + coord_flip() +
    labs(title="Top-15 Airlines by Lowest Mean Arrival Delay", x="Airline", y="Minutes")
}

# Cancellation Reasons 
if (all(c("CANCELLATION_REASON","CANCELLED") %in% names(flights))) {
  cancel_summary <- flights %>%
    filter(CANCELLED == 1) %>%
    count(CANCELLATION_REASON) %>%
    mutate(perc = round(n / sum(n) * 100, 1)) 
  
  ggplot(cancel_summary, aes(x = reorder(CANCELLATION_REASON, -perc), y = perc, fill = CANCELLATION_REASON)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = paste0(perc, "%")), vjust = -0.3, size = 4) +
    labs(title = "Flight Cancellations by Reason",
         x = "Cancellation Reason", y = "Percentage of Cancellations") +
    theme_minimal()
}

# Busiest Airports (Top 10 Origins & Destinations)
if (all(c("ORIGIN_AIRPORT", "DESTINATION_AIRPORT") %in% names(flights))) {
  # Top 10 origins
  busiest_origins <- flights %>%
    count(ORIGIN_AIRPORT, sort = TRUE) %>%
    slice_head(n = 10)
  
  # Top 10 destinations
  busiest_destinations <- flights %>%
    count(DESTINATION_AIRPORT, sort = TRUE) %>%
    slice_head(n = 10)
  
  # Combine plots with patchwork
  library(patchwork)
  
  p1 <- ggplot(busiest_origins, aes(x = reorder(ORIGIN_AIRPORT, n), y = n)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = "Top 10 Origin Airports",
         x = "Airport",
         y = "Number of Departures") +
    theme_minimal()
  
  p2 <- ggplot(busiest_destinations, aes(x = reorder(DESTINATION_AIRPORT, n), y = n)) +
    geom_col(fill = "darkorange") +
    coord_flip() +
    labs(title = "Top 10 Destination Airports",
         x = "Airport",
         y = "Number of Arrivals") +
    theme_minimal()
  
  # Display side-by-side
  p1 + p2
}


# Delay Distributions (raw vs winsorized)
if (all(c("ARRIVAL_DELAY","ARRIVAL_DELAY_W",
          "DEPARTURE_DELAY","DEPARTURE_DELAY_W") %in% names(flights))) {
  
  delay_long <- flights %>%
    select(ARRIVAL_DELAY, ARRIVAL_DELAY_W, DEPARTURE_DELAY, DEPARTURE_DELAY_W) %>%
    pivot_longer(everything(),
                 names_to = "metric",
                 values_to = "value") %>%
    mutate(
      type = case_when(
        grepl("_W", metric) ~ "Winsorized",
        TRUE ~ "Raw"
      ),
      category = case_when(
        grepl("ARRIVAL", metric) ~ "Arrival Delay",
        grepl("DEPARTURE", metric) ~ "Departure Delay"
      )
    ) %>%
    filter(is.finite(value))   # <- remove NA, NaN, Inf, -Inf
  
  ggplot(delay_long, aes(x = value, fill = type)) +
    geom_histogram(bins = 60, alpha = 0.6, position = "identity") +
    facet_wrap(~category, scales = "free_x") +
    labs(title = "Delay Distributions: Raw vs Winsorized",
         x = "Delay (minutes)",
         y = "Count") +
    theme_minimal()
}




# 8) EXPLORATORY ANALYSIS
# Monthly & airline delays
monthly_delay <- flights %>%
  group_by(MONTH) %>%
  summarise(MEAN_ARR_DELAY = mean(ARRIVAL_DELAY, na.rm = TRUE), .groups="drop")
airline_delay <- flights %>%
  group_by(AIRLINE) %>%
  summarise(MEAN_ARR_DELAY = mean(ARRIVAL_DELAY, na.rm = TRUE), .groups="drop") %>%
  arrange(MEAN_ARR_DELAY)

# Cancellation reasons
cancel_summary <- flights %>%
  filter(CANCELLED == 1) %>%
  count(CANCELLATION_REASON) %>%
  mutate(perc = round(n / sum(n) * 100, 1))

# Busiest airports
busiest_origins <- flights %>% count(ORIGIN_AIRPORT, sort=TRUE) %>% slice_head(n=10)
busiest_destinations <- flights %>% count(DESTINATION_AIRPORT, sort=TRUE) %>% slice_head(n=10)

# VISUALIZATION 
flights_long <- flights %>%
  select(ARRIVAL_DELAY, ARRIVAL_DELAY_W, DEPARTURE_DELAY, DEPARTURE_DELAY_W) %>%
  pivot_longer(everything(), names_to="metric", values_to="delay")

ggplot(flights_long, aes(x=metric, y=delay)) + geom_boxplot() + coord_flip() +
  labs(title="Raw vs Winsorized Delays")




# ------------------------------------------------------------------------------------------------------

# 6.1 Objective 1 – Relationship between Weather Delays and Arrival Delays

# ------------------------------------------------------------------------------------------------------


# Group summary
flights <- flights %>%
  mutate(weather_flag = ifelse(WEATHER_DELAY > 0, "Weather-affected",
                               ifelse(WEATHER_DELAY == 0, "No-weather", "Missing")))

flights %>%
  group_by(weather_flag) %>%
  summarise(
    n = n(),
    mean_arrival = mean(ARRIVAL_DELAY_W, na.rm = TRUE),
    median_arrival = median(ARRIVAL_DELAY_W, na.rm = TRUE)
  )


# Spearman correlation (robust to skewness)
cor.test(flights$WEATHER_DELAY, flights$ARRIVAL_DELAY_W, method = "spearman")

# Regression: arrival delay explained by weather & departure delay
model_weather <- lm(ARRIVAL_DELAY_W ~ WEATHER_DELAY + DEPARTURE_DELAY_W, data=flights)
summary(model_weather)



# Conservative: treat NA as 0
flights_zero <- flights %>%
  mutate(WEATHER_DELAY_0 = ifelse(is.na(WEATHER_DELAY), 0, WEATHER_DELAY))
summary(lm(ARRIVAL_DELAY_W ~ WEATHER_DELAY_0 + DEPARTURE_DELAY_W, data=flights_zero))

# Robustness: include Missing group
flights %>%
  group_by(weather_flag) %>%
  summarise(mean_arrival = mean(ARRIVAL_DELAY_W, na.rm = TRUE), n = n())


#Mediation Analysis

# Ensure factors
flights$AIRLINE <- factor(flights$AIRLINE)
flights$MONTH <- factor(flights$MONTH)

# Mediator model: weather → departure delay
med.fit <- lm(DEPARTURE_DELAY_W ~ WEATHER_DELAY + AIRLINE + MONTH + DISTANCE,
              data = flights, na.action = na.exclude)

# Outcome model: arrivals depend on weather + departure delay
out.fit <- lm(ARRIVAL_DELAY_W ~ WEATHER_DELAY + DEPARTURE_DELAY_W + AIRLINE + MONTH + DISTANCE,
              data = flights, na.action = na.exclude)

# Mediation analysis
library(mediation)
set.seed(123)
med.out <- mediate(med.fit, out.fit, treat="WEATHER_DELAY", mediator="DEPARTURE_DELAY_W",
                   boot=TRUE, sims=100)
summary(med.out)



#Visualizations
# Boxplot by group
ggplot(flights, aes(x=weather_flag, y=ARRIVAL_DELAY_W, fill=weather_flag)) +
  geom_boxplot(outlier.shape=NA, na.rm=TRUE) +
  coord_cartesian(ylim=c(-20,60)) +
  labs(title="Arrival Delays by Weather Reporting Status")

# Hexbin regression plot
ggplot(flights %>% filter(!is.na(WEATHER_DELAY), WEATHER_DELAY < 200),
       aes(x=WEATHER_DELAY, y=ARRIVAL_DELAY_W)) +
  geom_hex(bins=40) +
  geom_smooth(method="lm", se=TRUE, color="blue") +
  labs(title="Weather vs Arrival Delay with Regression Line")


#----------------------------------------------------------------------------------------------
# Objective 2 - Impact of airline-related delays on arrival delay

#Import the reequired libraries
library(dplyr)
library(ggplot2)
library(mediation)
library(hexbin)

#Group Summary
flights <- flights %>%
  mutate(AIRLINE_DELAY_FLAG = case_when(
    AIRLINE_DELAY > 0 ~ "Airline Delay",
    AIRLINE_DELAY == 0 ~ "No Airline Delay",
    TRUE ~ "Missing"
  ))

group_summary <- flights %>%
  group_by(AIRLINE_DELAY_FLAG) %>%
  summarise(mean_arrival_delay = mean(ARRIVAL_DELAY, na.rm = TRUE))

ggplot(group_summary, aes(x = AIRLINE_DELAY_FLAG, y = mean_arrival_delay, fill = AIRLINE_DELAY_FLAG)) +
  geom_bar(stat = "identity") +
  labs(title = "Figure 6.1.1.1: Arrival Delay by Airline Delay Status", x = "Airline Delay Status", y = "Mean Arrival Delay")

#Spearman Correlation and Regression
cor.test(flights$ARRIVAL_DELAY, flights$AIRLINE_DELAY, method = "spearman")
cor.test(flights$ARRIVAL_DELAY, flights$DEPARTURE_DELAY, method = "spearman")

model <- lm(ARRIVAL_DELAY ~ AIRLINE_DELAY + DEPARTURE_DELAY, data = flights)
summary(model)

ggplot(flights, aes(x = AIRLINE_DELAY, y = ARRIVAL_DELAY)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Figure 6.1.1.2: Airline Delay vs Arrival Delay with Regression Line",
    x = "Airline Delay (minutes)",
    y = "Arrival Delay (minutes)"
  ) +
  scale_x_continuous(breaks = seq(0, 1400, by = 200)) +
  scale_y_continuous(breaks = seq(0, 1400, by = 200))

#Conservative
flights_conservative <- flights %>%
  mutate(AIRLINE_DELAY = ifelse(is.na(AIRLINE_DELAY), 0, AIRLINE_DELAY),
         ARRIVAL_DELAY = ifelse(is.na(ARRIVAL_DELAY), 0, ARRIVAL_DELAY))

model_cons <- lm(ARRIVAL_DELAY ~ AIRLINE_DELAY, data = flights_conservative)
summary(model_cons)

#Mediation
model_m <- lm(DEPARTURE_DELAY ~ AIRLINE_DELAY, data = flights)
model_y <- lm(ARRIVAL_DELAY ~ DEPARTURE_DELAY + AIRLINE_DELAY, data = flights)

med_out <- mediate(model_m, model_y, treat = "AIRLINE_DELAY", mediator = "DEPARTURE_DELAY", boot = TRUE)
summary(med_out)



#Arrival Delays by Airline Delay Reporting Status 
#Boxplot
ggplot(flights, aes(x = AIRLINE_DELAY_FLAG, y = ARRIVAL_DELAY)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    title = "Figure 6.1.2.1: Arrival Delays by Airline Delay Reporting Status",
    x = "Airline Delay Status",
    y = "Arrival Delay (minutes)"
  )

# Descriptive statistics
flights %>%
  group_by(AIRLINE_DELAY_FLAG) %>%
  summarise(
    Mean_Arrival_Delay = mean(ARRIVAL_DELAY, na.rm = TRUE),
    Median_Arrival_Delay = median(ARRIVAL_DELAY, na.rm = TRUE),
    Count = n()
  )

#Airline Delay vs Arrival Delay with Regression Line 

# Remove NA values
flights_clean <- flights %>%
  filter(!is.na(AIRLINE_DELAY), !is.na(ARRIVAL_DELAY))

# Hexbin plot
ggplot(flights_clean, aes(x = AIRLINE_DELAY, y = ARRIVAL_DELAY)) +
  stat_binhex(bins = 50) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Figure 6.1.2.2: Airline Delay vs Arrival Delay with Hexbin Regression",
    x = "Airline Delay (minutes)",
    y = "Arrival Delay (minutes)",
    fill = "Count"
  )

# Spearman correlation
cor.test(flights_clean$AIRLINE_DELAY, flights_clean$ARRIVAL_DELAY, method = "spearman")


# ============================================================


# 6.3) Objective 3 — Airport Congestion → Arrival Delays
# IV: Congestion (number of departures per origin airport)
# DV: Average arrival delay


# ============================================================

#Safety Checks

needed_cols <- c("ORIGIN_AIRPORT","ARRIVAL_DELAY","DEPARTURE_DELAY")
stopifnot(all(needed_cols %in% names(flights)))


#Build Congestion Table

airport_congestion <- flights %>%
  group_by(ORIGIN_AIRPORT) %>%
  summarise(
    DEPARTURES = n(),
    AVG_ARR_DELAY = mean(ARRIVAL_DELAY, na.rm = TRUE),
    MED_ARR_DELAY = median(ARRIVAL_DELAY, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(DEPARTURES))

print(head(airport_congestion, 12))



#Scatter Plot

p_congestion_scatter <- ggplot(airport_congestion,
                               aes(x = DEPARTURES, y = AVG_ARR_DELAY)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Airport Congestion vs. Average Arrival Delay",
       x = "Number of Departures (per Origin Airport)",
       y = "Average Arrival Delay (min)") +
  theme_minimal()
print(p_congestion_scatter)


#Correlation

cor_spear <- cor.test(
  airport_congestion$DEPARTURES,
  airport_congestion$AVG_ARR_DELAY,
  method = "spearman",
  exact = FALSE
)
print(cor_spear)



#Simple Regression

fit_congestion <- lm(AVG_ARR_DELAY ~ DEPARTURES, data = airport_congestion)
summary(fit_congestion) %>% print()


#Top-10 vs Rest

top10 <- airport_congestion %>% slice_max(DEPARTURES, n = 10)
rest  <- airport_congestion %>% slice(-(1:10))

wt_congestion <- wilcox.test(top10$AVG_ARR_DELAY, rest$AVG_ARR_DELAY)
print(wt_congestion)


library(ggplot2)

p_radial <- top10 %>%
  ggplot(aes(x = reorder(ORIGIN_AIRPORT, AVG_ARR_DELAY),
             y = AVG_ARR_DELAY,
             fill = AVG_ARR_DELAY)) +
  geom_col(width = 1, color = "white") +
  coord_polar(start = 0) +
  labs(title = "Top-10 Busiest Airports — Mean Arrival Delay (Radial Chart)",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
print(p_radial)


#Summary Table

library(knitr)
kable(
  top10 %>% select(ORIGIN_AIRPORT, DEPARTURES, AVG_ARR_DELAY, MED_ARR_DELAY),
  caption = "Top-10 busiest origin airports with mean/median arrival delays"
)

# ============================
# OBJECTIVE 4 — Departure vs Arrival Delay
# ============================

library(dplyr)
library(ggplot2)
library(ggridges)
library(knitr)

needed_cols <- c("DEPARTURE_DELAY","ARRIVAL_DELAY")
stopifnot(all(needed_cols %in% names(flights)))

# -------- Preprocess: Buckets once --------
flights_buckets <- flights %>%
  mutate(
    DEP_BUCKET = cut(DEPARTURE_DELAY, breaks = c(-Inf,0,15,30,60,120,Inf),
                     labels = c("Early/OnTime","0-15","16-30","31-60","61-120","120+")),
    ARR_BUCKET = cut(ARRIVAL_DELAY, breaks = c(-Inf,0,15,30,60,120,Inf),
                     labels = c("Early/OnTime","0-15","16-30","31-60","61-120","120+"))
  )

# -------- 1. Heatmap --------
flights_heat <- flights_buckets %>%
  count(DEP_BUCKET, ARR_BUCKET)

p_heat <- ggplot(flights_heat, aes(x=DEP_BUCKET, y=ARR_BUCKET, fill=n)) +
  geom_tile(color="white") +
  scale_fill_gradient(low="lightyellow", high="darkred") +
  labs(title="Heatmap: Departure vs Arrival Delay Buckets",
       x="Departure Delay Bucket", y="Arrival Delay Bucket", fill="Flights") +
  theme_minimal()

print(p_heat)

# -------- 2. Ridge Plot --------
p_ridge <- flights_buckets %>%
  filter(!is.na(DEP_BUCKET)) %>%
  ggplot(aes(x=ARRIVAL_DELAY, y=DEP_BUCKET, fill=DEP_BUCKET)) +
  geom_density_ridges(alpha=0.7) +
  labs(title="Distribution of Arrival Delays across Departure Buckets",
       x="Arrival Delay (min)", y="Departure Delay Bucket") +
  theme_minimal() +
  theme(legend.position="none")

print(p_ridge)


# -------- 3. Regression + Diagnostics --------
fit_dep_arr <- lm(ARRIVAL_DELAY ~ DEPARTURE_DELAY, data=flights)
summary(fit_dep_arr)

par(mfrow=c(2,2))
plot(fit_dep_arr)   
par(mfrow=c(1,1))

# -------- 4. Correlation --------
cor_pear <- cor.test(flights$DEPARTURE_DELAY, flights$ARRIVAL_DELAY,
                     method="pearson", use="complete.obs")
cor_spear <- cor.test(flights$DEPARTURE_DELAY, flights$ARRIVAL_DELAY,
                      method="spearman", use="complete.obs")
print(cor_pear)
print(cor_spear)



# =============== ADDITIONAL FEATURE ==================
# ============================================================
# 7) ADDITIONAL FEATURE — Delay Propagation Network 
# ============================================================

# (0) Library Setup
extra_pkgs <- c("igraph","ggraph","tidygraph","scales")
to_install2 <- setdiff(extra_pkgs, rownames(installed.packages()))
if (length(to_install2) > 0) install.packages(to_install2, dependencies = TRUE)
suppressPackageStartupMessages({
  library(igraph); library(ggraph); library(tidygraph); library(scales)
})

# (1) Network Data Preparation
delay_col <- if ("ARRIVAL_DELAY_W" %in% names(flights)) "ARRIVAL_DELAY_W" else "ARRIVAL_DELAY"

routes <- flights |>
  dplyr::filter(!is.na(.data[[delay_col]])) |>
  dplyr::group_by(ORIGIN_AIRPORT, DESTINATION_AIRPORT) |>
  dplyr::summarise(
    n_flights = dplyr::n(),
    avg_delay = mean(.data[[delay_col]], na.rm = TRUE),
    .groups = "drop"
  )

routes_filtered <- routes |> dplyr::filter(n_flights >= 50)

# (2) Graph Construction
g <- igraph::graph_from_data_frame(
  d = routes_filtered |>
    dplyr::transmute(from = ORIGIN_AIRPORT, to = DESTINATION_AIRPORT,
                     weight = n_flights, delay = avg_delay),
  directed = TRUE
)

strength_out_vec <- igraph::strength(g, vids = igraph::V(g), mode = "out", weights = igraph::E(g)$weight)

ends_mat <- igraph::ends(g, igraph::E(g), names = FALSE)   
src_idx  <- ends_mat[,1]
avg_delay_out_by_src <- tapply(igraph::E(g)$delay, src_idx, function(x) mean(x, na.rm = TRUE))
avg_delay_out_vec <- rep(NA_real_, igraph::vcount(g))
avg_delay_out_vec[as.integer(names(avg_delay_out_by_src))] <- as.numeric(avg_delay_out_by_src)

igraph::V(g)$strength_out   <- strength_out_vec
igraph::V(g)$avg_delay_out  <- avg_delay_out_vec
igraph::V(g)$prop_score     <- igraph::V(g)$strength_out * ifelse(is.na(igraph::V(g)$avg_delay_out), 0, igraph::V(g)$avg_delay_out)

# (3) Selecting Top Airports for Visualization
top40_names <- igraph::V(g)$name[order(igraph::V(g)$prop_score, decreasing = TRUE)][1:min(40, igraph::vcount(g))]
subg <- igraph::induced_subgraph(g, vids = igraph::V(g)[name %in% top40_names])


# (4) Visualization 1 — Network Graph
set.seed(42)
p_net <- ggraph(tidygraph::as_tbl_graph(subg), layout = "fr") +
  geom_edge_link(aes(width = scales::rescale(weight, to = c(0.2, 1.5)),
                     alpha = scales::rescale(weight, to = c(0.1, 0.6))),
                 colour = "grey55", show.legend = FALSE) +
  geom_node_point(aes(size = strength_out, fill = avg_delay_out),
                  shape = 21, colour = "grey15") +
  scale_size_continuous(name = "Outgoing traffic", range = c(3,14)) +
  scale_fill_gradient2(name = "Avg outgoing delay (min)",
                       low = "#43a047", mid = "#ffeb3b", high = "#e53935",
                       midpoint = 5, limits = c(0, NA)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  labs(title = "Delay Propagation Network — Top Influential Airports",
       subtitle = "Node size = outgoing traffic · Node color = avg delay · Edge width = traffic volume",
       x = NULL, y = NULL) +
  theme_minimal()
print(p_net)

# (5) Visualization 2 — Top Delay Spreaders Bar Plot
sub_nodes_df <- data.frame(
  name = igraph::V(subg)$name,
  strength_out = igraph::V(subg)$strength_out,
  avg_delay_out = igraph::V(subg)$avg_delay_out,
  prop_score = igraph::V(subg)$prop_score,
  stringsAsFactors = FALSE
)

top_spreaders <- sub_nodes_df |>
  dplyr::arrange(dplyr::desc(prop_score)) |>
  dplyr::slice_head(n = 15)

p_top <- ggplot(top_spreaders, aes(x = reorder(name, prop_score), y = prop_score)) +
  geom_col() + coord_flip() +
  labs(title = "Top Delay Spreaders — Airports",
       x = "Airport (IATA)",
       y = "Propagation Score = outgoing traffic × avg outgoing delay") +
  theme_minimal()
print(p_top)

# (6) Summary Table of Top Airports
library(knitr)
kable(
  top_spreaders |>
    dplyr::transmute(
      AIRPORT = name,
      OUT_TRAFFIC = round(strength_out, 0),
      AVG_DELAY_OUT_MIN = round(avg_delay_out, 2),
      PROPAGATION_SCORE = round(prop_score, 2)
    ),
  caption = "Top-15 airports by delay propagation score"
)

























