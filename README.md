# Statistical Data Analysis Using Linear Regression

Statistical analysis of song popularity using linear regression — **Group 66, Part B**.

This project builds and analyzes a linear regression model that explains a song's popularity score (`song_popularity`) based on its musical and technical attributes (tempo, energy, danceability, duration, emotional valence, and more), while testing interactions and transformations that improve model fit and predictive power.

## Files in this repository

| File | Description |
|------|-------------|
| `group_66_song_popularity_Part_B.R` | Full R code — preprocessing, model building, assumption checks, and model improvement |
| `group_66_song_popularity_Part_B.docx` | Full report: executive summary, methodology, plots, results, and conclusions |

## Objective

To build a linear regression model that explains a song's success, and to test whether more complex effects — interactions between audio features and non-linear transformations — improve the model beyond the separate effect of each variable.

## Analysis steps

**1. Data preprocessing**

- Outlier removal using the IQR method and deduplication of repeated observations.
- Removal of non-contributing variables: `time_signature` (near-zero variance — about 94% of songs are in 4/4) and `audio_mode` (major/minor, near-zero correlation r≈0.005).
- Conversion of `song_duration_ms` to minutes for easier interpretation of coefficients.
- Discretization tests for `tempo` (ANOVA) and `instrumentalness` — both kept continuous after testing.

**2. Dummy and interaction variables**

- Tempo dummy variables: `Standard` (up to 122 BPM, reference group), `Energetic` (122–140), `High Speed` (above 140).
- Three interaction terms based on musical reasoning: `danceability × energy`, `danceability × instrumentalness`, `audio_valence × energy` (including 3D surface plots in plotly).

**3. Variable selection and assumption checks**

- Comparison between Backward Elimination and Forward Selection using the **AIC** criterion; the Backward model was chosen (lower AIC, only 9 variables) for its ability to preserve significant interactions.
- Visual checks (Residuals vs Fitted, Q-Q Plot) and formal tests (**Ramsey RESET** for linearity, **Kolmogorov-Smirnov / Lilliefors** for normality).

**4. Model improvement**

- Adding a quadratic term for energy (`energy²`) to capture a "bell-curve effect" — a saturation point beyond which high energy hurts popularity.
- Removing `duration_min` (not significant), `liveness` and `speechiness` (near-zero contribution), and replacing continuous tempo with a dummy variable due to multicollinearity.
- Removal of influential observations using Cook's distance.

## Key results

- **A drop of about 5,642 points in AIC** (from ~86,676 to ~81,034) and a ~22% increase in explained variance (R²).
- The `danceability × energy` interaction is the most influential factor (coefficient ‎+31.32‎): audiences seek songs that are both rhythmic and energetic.
- Energy alone is harmful (`energy²` negative, ‎-24.11‎), danceable instrumental songs are penalized, and "happy" songs succeed mainly when they are also energetic.
- A clear audience preference for electronic production and fast tempos (above 140 BPM) over an acoustic sound.

## Conclusion

A song's success does not depend on a single variable but on synergistic combinations of musical attributes. High energy on its own is off-putting, but combined with danceability it becomes the key to a hit.

## Running the code

The code is written in R and uses the following packages:

```r
install.packages(c("dplyr", "ggplot2", "gridExtra", "plotly", "lmtest", "nortest"))
```

The code assumes the existence of a `song_popularity` data object in the working environment (the output of Part A of the project).
