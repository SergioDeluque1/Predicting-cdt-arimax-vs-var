# CDT Rates and Inflation in Colombia: ARIMAX and VAR Analysis

This project explores the empirical relationship between the 90-day Certificate of Deposit (CDT) rate and annual inflation in Colombia [1]. It evaluates the transmission of monetary policy by comparing a univariate ARIMAX framework against a bivariate Vector Autoregressive (VAR) system to identify which structure yields superior forecasting accuracy [1-3].

## Dataset & Scope
*   **Frequencies & Metrics**: Monthly observations [4, 5].
    *   **Inflation**: Annualised percentage variation of the Consumer Price Index (IPC), sourced from **DANE** [4-6].
    *   **CDT90**: Average monthly interest rate (%) of 90-day Certificates of Deposit, sourced from the **Banco de la República** [5, 7, 8].
*   **Estimation Window**: April 2008 to July 2026 (220 observations) [4, 5, 9].
*   **Out-of-Sample Validation**: August 2025 to July 2026 (12 months) [10-12].
*   **Forecast Horizon**: August to December 2026 [4].

## Key Findings

### 1. Stylised Facts & Interaction
*   **High Correlation**: There is a strong positive contemporary correlation of **0.8326** between inflation and the CDT90 rate over the sample period, indicating that interest rates dynamically co-move alongside price rises [13-15].
*   **Bidirectional Predictive Causality**: Granger causality tests show a highly significant **bidirectional predictive relationship** (CDT90 $\rightarrow$ Inflation and Inflation $\rightarrow$ CDT90) [12, 15-17]. Past values of each series contain valuable statistical information to forecast the other, though this represents predictive rather than structural economic causality [6, 7, 16, 18].

### 2. Model Performance & Diagnostics
*   **ARIMAX (ARIMA(2,0,1)(2,0,0)[14])**: Modelled with `CDT90` as an exogenous regressor (coefficient = **+0.1382**) [19-21]. Residuals are clean and behave as white noise, passing the Ljung-Box test for autocorrelation ($p = 0.1396$) [22-24].
*   **VAR(2)**: Models both variables endogenously [3, 25, 26]. While stable ($VAR stable = TRUE$) [27, 28], the VAR residuals fail diagnostic testing due to significant autocorrelation (Portmanteau $p = 0.00532$) [27, 28], conditional heteroscedasticity (ARCH $p = 0.02744$) [28, 29], and non-normality (Jarque-Bera $p < 2.2 \times 10^{-14}$) [17, 29].
*   **Validation Winner**: The **ARIMAX model decisively outperforms the VAR** in out-of-sample testing, yielding an **RMSE of 0.3636** (vs. **2.0351** for the VAR) [11, 12, 30].

## Inflation Projections (Aug–Dec 2026)
Due to its superior predictive power, the ARIMAX model was selected for the final forecasts [12, 31, 32]. It projects a highly inertial plateau with inflation remaining elevated near **6%** through the end of 2026 [33-35]:
*   **August 2026**: **6.11%** (95% CI: 5.64% to 6.58%) [32, 36]
*   **September 2026**: **6.12%** (95% CI: 5.30% to 6.94%) [32, 33, 36]
*   **October 2026**: **6.00%** (95% CI: 4.83% to 7.18%) [32, 34, 36]
*   **November 2026**: **6.14%** (95% CI: 4.60% to 7.68%) [32, 34, 36]
*   **December 2026**: **6.24%** (95% CI: 4.34% to 8.14%) [32, 34, 36]
