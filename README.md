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

## results:
## Airline Performance :

<img width="465" height="327" alt="image" src="https://github.com/user-attachments/assets/315d3534-800e-4e00-a8a1-ca19693093a6" />

## Hexbin Regression : 
<img width="468" height="410" alt="image" src="https://github.com/user-attachments/assets/24c5b2a2-9c57-4676-b8f7-8c7f5ab8e4cb" />

## before mapping : 
<img width="468" height="267" alt="image" src="https://github.com/user-attachments/assets/e7a82025-82af-446c-b312-75a807d73d85" />


## after mapping (IATA standardization) : 
<img width="468" height="311" alt="image" src="https://github.com/user-attachments/assets/273f6442-2f92-49be-9418-5a9c52ebabf6" />

##  Statistical Methods Used
- Spearman Rank Correlation
- Linear & Hexbin Regression
- Mediation Analysis (Indirect vs. Direct Effects)
- Winsorization for Outlier Mitigation
