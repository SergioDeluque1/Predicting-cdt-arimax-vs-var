# CDT Rates and Inflation in Colombia: ARIMAX and VAR Analysis

This project explores the empirical relationship between the 90-day Certificate of Deposit (CDT) rate and annual inflation in Colombia . It evaluates the transmission of monetary policy by comparing a univariate ARIMAX framework against a bivariate Vector Autoregressive (VAR) system to identify which structure yields superior forecasting accuracy.

## Dataset & Scope
*   **Frequencies & Metrics**: Monthly observations.
    *   **Inflation**: Annualised percentage variation of the Consumer Price Index (IPC), sourced from **DANE** .
    *   **CDT90**: Average monthly interest rate (%) of 90-day Certificates of Deposit, sourced from the **Banco de la República**.
*   **Estimation Window**: April 2008 to July 2026 (220 observations).
*   **Out-of-Sample Validation**: August 2025 to July 2026 (12 months).
*   **Forecast Horizon**: August to December 2026.

## Key Findings

### 1. Stylised Facts & Interaction
*   **High Correlation**: There is a strong positive contemporary correlation of **0.8326** between inflation and the CDT90 rate over the sample period, indicating that interest rates dynamically co-move alongside price rises.
*   **Bidirectional Predictive Causality**: Granger causality tests show a highly significant **bidirectional predictive relationship** (CDT90 $\rightarrow$ Inflation and Inflation $\rightarrow$ CDT90). Past values of each series contain valuable statistical information to forecast the other, though this represents predictive rather than structural economic causality.

### 2. Model Performance & Diagnostics
*   **ARIMAX (ARIMA(2,0,1)(2,0,0)[14])**: Modelled with `CDT90` as an exogenous regressor (coefficient = **+0.1382**). Residuals are clean and behave as white noise, passing the Ljung-Box test for autocorrelation ($p = 0.1396$).
*   **VAR(2)**: Models both variables endogenously. While stable ($VAR stable = TRUE$), the VAR residuals fail diagnostic testing due to significant autocorrelation (Portmanteau $p = 0.00532$), conditional heteroscedasticity (ARCH $p = 0.02744$), and non-normality (Jarque-Bera $p < 2.2 \times 10^{-14}$).
*   **Validation Winner**: The **ARIMAX model decisively outperforms the VAR** in out-of-sample testing, yielding an **RMSE of 0.3636** (vs. **2.0351** for the VAR).

## Inflation Projections (Aug–Dec 2026)
Due to its superior predictive power, the ARIMAX model was selected for the final forecasts. It projects a highly inertial plateau with inflation remaining elevated near **6%** through the end of 2026:
*   **August 2026**: **6.11%** (95% CI: 5.64% to 6.58%)
*   **September 2026**: **6.12%** (95% CI: 5.30% to 6.94%)
*   **October 2026**: **6.00%** (95% CI: 4.83% to 7.18%)
*   **November 2026**: **6.14%** (95% CI: 4.60% to 7.68%)
*   **December 2026**: **6.24%** (95% CI: 4.34% to 8.14%)
