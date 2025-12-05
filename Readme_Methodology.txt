# Methodological Procedure: Stock–Flow Consistent Capital Stock Reconstruction (Chile)

This document describes the procedure used to reconstruct long-run fixed capital stocks for Chile by asset, in constant 2003 CLP, ensuring stock–flow consistency, additivity across assets, and compatibility with aggregate demand accounts.  

The implementation is tailored to a panel suitable for macro-historical and political-economy analysis.

---

## 1. Assets, coverage, and notation

We work with the following asset groups:

- **Machinery & equipment:** `ME`
- **Non-residential construction:** `NRC`
- **Residential construction:** `RC`
- **Construction total:** `C = NRC + RC`
- **Non-residential capital:** `NR = ME + NRC`
- **Total fixed capital:** `T = ME + NRC + RC = NR + RC`

For each asset \(a \in \{ME, NRC, RC, C, NR, T\}\) and year \(t\), we observe or reconstruct:

- Gross fixed capital formation: \(I^g_{a,t}\)
- Gross capital stock: \(K^g_{a,t}\)
- Net capital stock: \(K^n_{a,t}\)
- Depreciation: \(D_{a,t}\)
- Retirements / scrapping: \(R_{a,t}\)
- Depreciation rate: \(\delta_{a,t} = D_{a,t} / K^n_{a,t}\)
- Retirement rate (depletion): \(z_{a,t} = R_{a,t} / K^g_{a,t}\)

All stock and flow series are expressed in constant 2003 CLP.

---

## 2. Consistency constraints

The database is constructed to satisfy, as closely as the sources allow, three sets of constraints:

### 2.1 Stock–flow consistency

For each asset \(a\) and year \(t\):

\[
\Delta K^g_{a,t} = K^g_{a,t} - K^g_{a,t-1} = I^g_{a,t} - R_{a,t}
\]

\[
\Delta K^n_{a,t} = K^n_{a,t} - K^n_{a,t-1} = I^g_{a,t} - D_{a,t}
\]

Combining both identities:

\[
D_{a,t} - R_{a,t} = \Delta K^g_{a,t} - \Delta K^n_{a,t}
\]

This is the core stock–flow consistency condition.

### 2.2 Additivity across assets

For any variable \(X \in \{I^g, K^g, K^n, D, R\}\) and year \(t\):

- Non-residential capital:
  \[
  X_{NR,t} = X_{ME,t} + X_{NRC,t}
  \]

- Construction:
  \[
  X_{C,t} = X_{NRC,t} + X_{RC,t}
  \]

- Total fixed capital:
  \[
  X_{T,t} = X_{ME,t} + X_{NRC,t} + X_{RC,t}
  \]

These identities are used both as construction rules and as diagnostic checks (additivity residuals are computed and stored).

### 2.3 Consistency with aggregate demand

Using Pérez–Eyzaguirre’s aggregate demand decomposition in 2003 CLP, we start from:

\[
Y_t = C^{priv}_t + C^{gov}_t + I^{dom}_t + NX_t
\]

with domestic investment:

\[
I^{dom}_t = FBKF^{ME}_t + FBKF^{C}_t + \Delta Inventories_t
\]

We impose that reconstructed gross investment by asset is consistent with the fixed capital formation components reported in the aggregate demand accounts:

- Fixed capital formation in machinery & equipment: \(FBKF^{ME}_t\)
- Fixed capital formation in construction: \(FBKF^{C}_t\)

Our \(I^g_{ME,t}\) and \(I^g_{C,t} = I^g_{NRC,t} + I^g_{RC,t}\) are constrained to match these aggregates.

---

## 3. Re-basing and anchoring (1980 → 2003 CLP)

### 3.1 Hofman stocks and the 1950 anchor

Hofman (2000) reports gross and net capital stocks in 1980 CLP for 1950:

- \(K^{g,H}_{a,1950;1980}\)
- \(K^{n,H}_{a,1950;1980}\)

An auxiliary anchor table provides the corresponding 2003 CLP levels for 1950:

- \(K^{g,H}_{a,1950;2003}\)
- \(K^{n,H}_{a,1950;2003}\)

For each asset \(a\), we define constant re-basing factors:

\[
\phi_{g,a} = \frac{K^{g,H}_{a,1950;2003}}{K^{g,H}_{a,1950;1980}}, 
\qquad
\phi_{n,a} = \frac{K^{n,H}_{a,1950;2003}}{K^{n,H}_{a,1950;1980}}
\]

These factors are used to convert all Hofman series from 1980 to 2003 CLP:

\[
K^{g,H}_{a,t;2003} = \phi_{g,a} \cdot K^{g,H}_{a,t;1980}
\]
\[
K^{n,H}_{a,t;2003} = \phi_{n,a} \cdot K^{n,H}_{a,t;1980}
\]

### 3.2 Re-basing Hofman gross investment

We apply the same gross-stock factor \(\phi_{g,a}\) to Hofman’s gross investment in 1980 CLP:

\[
I^{g,H}_{a,t;2003} = \phi_{g,a} \cdot I^{g,H}_{a,t;1980}
\]

This assumes a common re-basing factor for stocks and flows for each asset.

### 3.3 Aligning Clio-Lab net stocks to Hofman (1950)

Clio-Lab provides net capital stock series already in 2003 CLP. For assets where both sources overlap (ME, C, T), we level-align Clio-Lab to Hofman in 1950.

For each relevant asset \(a\):

\[
\lambda_a = \frac{K^{n,H}_{a,1950;2003}}{K^{n,Clio}_{a,1950;2003}}
\]

\[
K^{n,Clio*}_{a,t;2003} = \lambda_a \cdot K^{n,Clio}_{a,t;2003}
\]

These rescaled Clio-Lab series \(K^{n,Clio*}_{a,t;2003}\) form the **backbone** for net stocks of ME, C, and T.

---

## 4. Construction of gross investment \(I^g_{a,t}\) (2003 CLP)

We construct a path for gross investment by asset that is consistent with aggregate demand and historical composition.

### 4.1 Aggregate demand base (Pérez–Eyzaguirre)

From Pérez–Eyzaguirre (in 2003 CLP):

- \(FBKF^{ME}_t\): fixed capital formation in machinery & equipment  
- \(FBKF^{C}_t\): fixed capital formation in construction

These series provide the aggregate totals for ME and C.

### 4.2 Hofman construction shares (1900–1994)

From Hofman’s investment by asset (in 1980 CLP), for 1900–1994, we compute construction shares:

\[
s^{H}_{NRC,t} = \frac{I^{g,H}_{NRC,t}}{I^{g,H}_{NRC,t} + I^{g,H}_{RC,t}}
\]

\[
s^{H}_{RC,t} = 1 - s^{H}_{NRC,t}
\]

These shares are **independent** of the price base. For years not covered by Hofman, we extend the series using nearest-neighbor fills (forward/backward along the time axis).

### 4.3 Decomposition of construction investment

Using the 2003 CLP aggregate demand and Hofman’s construction shares:

\[
I^g_{NRC,t;2003} = s^{H}_{NRC,t} \cdot FBKF^{C}_t
\]

\[
I^g_{RC,t;2003} = s^{H}_{RC,t} \cdot FBKF^{C}_t
\]

### 4.4 Machinery & composite aggregates

We set:

\[
I^g_{ME,t;2003} = FBKF^{ME}_t
\]

and define composite aggregates:

- Construction:
  \[
  I^g_{C,t;2003} = I^g_{NRC,t;2003} + I^g_{RC,t;2003}
  \]

- Non-residential capital:
  \[
  I^g_{NR,t;2003} = I^g_{ME,t;2003} + I^g_{NRC,t;2003}
  \]

- Total fixed capital:
  \[
  I^g_{T,t;2003} = I^g_{ME,t;2003} + I^g_{NRC,t;2003} + I^g_{RC,t;2003}
  \]

Additivity residuals for these identities are computed and stored but **not** forced to zero.

---

## 5. Construction of gross capital stocks \(K^g_{a,t}\) (2003 CLP)

We combine Tafunell & Ducoing’s gross capital stock indices with Hofman’s level information.

### 5.1 ME and NRC: index path + Hofman anchor

Tafunell & Ducoing provide asset-specific indices \(idx_{a,t}\) (e.g. base \(1929 = 100\)) for \(a \in \{ME, NRC\}\).

Using Hofman’s 1950 level in 1980 CLP:

\[
K^g_{a,t;1980} = K^{g,H}_{a,1950;1980} \cdot \frac{idx_{a,t}}{idx_{a,1950}}
\]

Re-basing to 2003 CLP via \(\phi_{g,a}\):

\[
K^g_{a,t;2003} = \phi_{g,a} \cdot K^g_{a,t;1980}
\]

This yields a **growth-preserving** path anchored to Hofman’s 1950 level.

### 5.2 Composite non-residential capital

Non-residential capital is obtained by aggregation:

\[
K^g_{NR,t;2003} = K^g_{ME,t;2003} + K^g_{NRC,t;2003}
\]

### 5.3 RC, C, T from Hofman (1950–1994)

For RC, C, and T over 1950–1994, we rely directly on Hofman:

\[
K^g_{a,t;2003} = K^{g,H}_{a,t;2003}, \quad a \in \{RC, C, T\}, \ t \in [1950, 1994]
\]

where \(K^{g,H}_{a,t;2003}\) are obtained from Hofman’s 1980 CLP stocks using \(\phi_{g,a}\).

Outside this range, no additional extrapolation of \(K^g_{RC,t}\), \(K^g_{C,t}\), or \(K^g_{T,t}\) is imposed in this step. Additivity identities are checked for overlapping years.

---

## 6. Construction of net capital stocks \(K^n_{a,t}\) (2003 CLP)

Net stocks are constructed using the rescaled Clio-Lab series as a backbone, with Hofman providing the internal construction shares.

### 6.1 Backbone net stocks (ME, C, T)

We use the rescaled Clio-Lab series:

\[
K^n_{a,t;2003} = K^{n,Clio*}_{a,t;2003}, \quad a \in \{ME, C, T\}
\]

### 6.2 Construction net-stock shares from Hofman (1950–1994)

Using Hofman’s net stocks (converted to 2003 CLP):

\[
\theta_{NRC,t} = \frac{K^{n,H}_{NRC,t;2003}}{K^{n,H}_{C,t;2003}}
\]

\[
\theta_{RC,t} = 1 - \theta_{NRC,t}
\]

for \(t \in [1950, 1994]\). These shares are extended to all years in the Clio-Lab construction series via nearest-neighbor filling.

### 6.3 Decomposition of construction net stock

Whenever \(K^{n,Clio*}_{C,t;2003}\) is available:

\[
K^n_{NRC,t;2003} = \theta_{NRC,t} \cdot K^{n,Clio*}_{C,t;2003}
\]

\[
K^n_{RC,t;2003} = \theta_{RC,t} \cdot K^{n,Clio*}_{C,t;2003}
\]

For years where Hofman’s net stocks for NRC and RC exist (1950–1994), we **override** the decomposition and use Hofman directly:

\[
K^n_{NRC,t;2003} = K^{n,H}_{NRC,t;2003}
\]

\[
K^n_{RC,t;2003} = K^{n,H}_{RC,t;2003}
\]

### 6.4 Composite non-residential net capital

Non-residential net capital is constructed via:

\[
K^n_{NR,t;2003} = K^n_{ME,t;2003} + K^n_{NRC,t;2003}
\]

Again, additivity identities are computed and retained as residuals.

---

## 7. Stock–flow consistent reconciliation

Given the reconstructed \(I^g_{a,t;2003}\), \(K^g_{a,t;2003}\), and \(K^n_{a,t;2003}\), we recover implied depreciation and retirement flows, as well as rates and consistency diagnostics.

### 7.1 Stock changes

\[
\Delta K^g_{a,t} = K^g_{a,t} - K^g_{a,t-1}
\]

\[
\Delta K^n_{a,t} = K^n_{a,t} - K^n_{a,t-1}
\]

The first observation after differencing is typically dropped in empirical implementation.

### 7.2 Implied depreciation and retirements

From the stock–flow identities:

\[
D_{a,t} = I^g_{a,t} - \Delta K^n_{a,t}
\]

\[
R_{a,t} = I^g_{a,t} - \Delta K^g_{a,t}
\]

### 7.3 Depreciation and retirement rates

Rates are defined whenever the denominators are non-zero:

\[
\delta_{a,t} = \frac{D_{a,t}}{K^n_{a,t}}
\]

\[
z_{a,t} = \frac{R_{a,t}}{K^g_{a,t}}
\]

These implicit \(\delta_{a,t}\) and \(z_{a,t}\) summarize the joint motion of stocks and flows and are treated as data-driven diagnostics rather than imposed schedules.

### 7.4 Joint stock–flow residual

We assess internal consistency via the joint residual:

\[
\varepsilon^{joint}_{a,t} = (D_{a,t} - R_{a,t}) - (\Delta K^g_{a,t} - \Delta K^n_{a,t})
\]

In a perfectly stock–flow consistent system, \(\varepsilon^{joint}_{a,t} = 0\) for all \(a,t\).

We normalize this residual by different scales:

- Relative to investment:
  \[
  r^{Ig}_{a,t} = \frac{\varepsilon^{joint}_{a,t}}{I^g_{a,t}}
  \]
- Relative to gross stock:
  \[
  r^{Kg}_{a,t} = \frac{\varepsilon^{joint}_{a,t}}{K^g_{a,t}}
  \]
- Relative to net stock:
  \[
  r^{Kn}_{a,t} = \frac{\varepsilon^{joint}_{a,t}}{K^n_{a,t}}
  \]

whenever the denominators are non-zero.

We also define period labels (e.g. 1900–1949, 1950–1994, 1995+) and flags such as:

- `full_SFC_window`: years 1950–1994, where Hofman and Clio-Lab overlap
- `full_ME_window`: extended coverage for ME (e.g. 1900–2008)

These labels are used in the diagnostics and summary statistics.

---

## 8. Diagnostic plots

For each asset \(a\), we produce time-series diagnostics:

- Implicit depreciation rates \(\delta_{a,t}\) versus \(t\)
- Retirement / depletion rates \(z_{a,t}\) versus \(t\)
- Normalized SFC residuals \(r^{Ig}_{a,t}\) versus \(t\)

Key splice years (e.g. 1900, 1940, 1950, 1994) are marked to visually relate changes in methodology or data sources to shifts in implied rates and residuals.

These plots are used to:

- Check for structural breaks or outliers in \(\delta_{a,t}\) and \(z_{a,t}\)
- Evaluate the magnitude and persistence of stock–flow residuals
- Assess whether splices and re-basings preserve coherent accumulation paths

---

## 9. Numerical validation indexes

For each asset \(a\) and period \(p\) (e.g. a sub-interval of years), we compute residual-based validation metrics using \(r^{Ig}_{a,t}\) as the primary scale:

- Mean squared error:
  \[
  MSE_{a,p} = \mathbb{E}_p\left[(r^{Ig}_{a,t})^2\right]
  \]
- Root mean squared error:
  \[
  RMSE_{a,p} = \sqrt{MSE_{a,p}}
  \]
- Mean absolute error:
  \[
  MAE_{a,p} = \mathbb{E}_p\left[|r^{Ig}_{a,t}|\right]
  \]
- Maximum absolute residual:
  \[
  max\_abs\_rIg_{a,p} = \max_t |r^{Ig}_{a,t}|
  \]

We also compute tail probabilities for residuals:

- Share of years with small residuals:
  \[
  p^{(2\%)}_{a,p} = \Pr\left(|r^{Ig}_{a,t}| \leq 0.02\right)
  \]
  \[
  p^{(5\%)}_{a,p} = \Pr\left(|r^{Ig}_{a,t}| \leq 0.05\right)
  \]

For the implied rates \(\delta_{a,t}\) and \(z_{a,t}\), by asset and period, we compute:

- Means:
  \[
  mean\_delta_{a,p}, \quad mean\_z_{a,p}
  \]
- Standard deviations:
  \[
  sd\_delta_{a,p}, \quad sd\_z_{a,p}
  \]
- Coefficients of variation:
  \[
  cv_{\delta,a,p} = \frac{sd\_delta_{a,p}}{|mean\_delta_{a,p}|}
  \]
  \[
  cv_{z,a,p} = \frac{sd\_z_{a,p}}{|mean\_z_{a,p}|}
  \]

Tail probabilities for \(\delta_{a,t}\) and \(z_{a,t}\) include, for example:

- High depreciation probability:
  \[
  p^{high}_{\delta,a,p} = \Pr(\delta_{a,t} > 0.2)
  \]
- Negative depreciation probability:
  \[
  p^{neg}_{\delta,a,p} = \Pr(\delta_{a,t} < 0)
  \]

with analogous definitions for \(z_{a,t}\).

Global indicators are computed for:

- The **full SFC window** (1950–1994, all assets), aggregating across assets and years
- The **ME extended window**, focusing on the long ME series

These metrics summarize how closely the reconstructed panel adheres to stock–flow identities and whether implied rates behave in economically interpretable ranges across historical periods.

---

## 10. Implementation notes

- The methodology is designed to be implemented in a reproducible R pipeline, with:
  - Year–asset panels for \(K^g, K^n, I^g, D, R, \delta, z\)
  - Explicit identity checks and residuals stored alongside the main series
  - Separate scripts for data loading, harmonization, SFC reconciliation, plotting, and validation

- All deviations from exact stock–flow consistency or additivity are **quantified** rather than silently corrected, so that downstream users can evaluate the quality of the harmonized series relative to their research needs.
