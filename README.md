# Aviation Analytics: Diagnostic Study of U.S. Flight Delays
**An R-based language study on operational efficiency and weather-induced delay propagation.**

##  The Problem
Flight delays cost billions annually. This project analyzes a year-long dataset from the U.S. Bureau of Transportation Statistics to move beyond "what happened" to "why it happened" (Diagnostic Analytics).

##  Data Engineering & Quality
- **Systemic Error Correction:** Resolved a reporting issue where 8.34% of airports used DOT IDs instead of IATA codes during October 2015.
- **Outlier Stabilization:** Applied 99th percentile **Winsorization** to reduce maximum arrival delays from 1,391 minutes to a representative 167 minutes for fair comparison.
- **Feature Engineering:** Developed a **Binary Delay Target** (>15 min) and **Departure Time Buckets** to analyze time-of-day effects.

##  Key Research Findings
- **The Propagation Effect:** Mediation analysis confirmed that 84% of weather impact on arrivals is mediated by departure delays.
- **Carrier Benchmarking:** Ultra-low-cost carriers (Spirit, Frontier) recorded 4x higher average delays compared to legacy carriers like Delta.
- **Seasonal Peaks:** Identified February and June as critical months for weather-related disruptions.

##  Statistical Methods Used
- Spearman Rank Correlation
- Linear & Hexbin Regression
- Mediation Analysis (Indirect vs. Direct Effects)
- Winsorization for Outlier Mitigation
