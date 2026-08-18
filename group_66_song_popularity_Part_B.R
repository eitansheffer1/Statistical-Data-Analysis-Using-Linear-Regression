

################
## צמצום הנתונים בהתאם לחלק א


library(dplyr) 

# 1. Function to identify non-outliers based on IQR method
identify_non_outliers <- function(data, col_name) {
  # Calculate Q1, Q3, and IQR
  Q1 <- quantile(data[[col_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[col_name]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  Lower_Bound <- Q1 - 1.5 * IQR_val
  Upper_Bound <- Q3 + 1.5 * IQR_val
  
  # Returns a logical vector of non-outliers within the calculated bounds
  return(data[[col_name]] >= Lower_Bound & data[[col_name]] <= Upper_Bound)
}

# 2. Create the clean data frame: DEDUPLICATION + OUTLIER FILTERING
song_popularity_clean <- song_popularity %>%
  
  # REMOVE DUPLICATE ROWS (DEDUPLICATION)
  distinct() %>% 
  
  # FILTER OUT OUTLIERS (for the justified variables)
  filter(
    identify_non_outliers(., "song_duration_ms"), # Note: changed to exact column name if needed
    identify_non_outliers(., "liveness"),
    identify_non_outliers(., "speechiness")
  )



##############################

##Start of part B

##############################



# --- חלק 2.1: גרפים להסרת משתנים (אלטרנטיבה למטריצה) ---

library(ggplot2)
library(dplyr)

# גרף 1: התפלגות המקצב (Time Signature) - הוכחת שונות נמוכה
plot_time <- ggplot(song_popularity_clean, aes(x = as.factor(time_signature))) +
  geom_bar(fill = "steelblue", color = "black", alpha = 0.7) +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5, size=3) +
  labs(title = "Distribution of Time Signature",
       subtitle = "Evidence of Low Variance (Dominance of 4/4)",
       x = "Time Signature (Beats per Bar)",
       y = "Count of Songs") +
  theme_minimal()

# גרף 2: השוואת פופולריות לפי סולם (Audio Mode) - הוכחת חוסר קשר
# המרה לפקטור כדי שהגרף יהיה ברור (0=Minor, 1=Major)
song_popularity_clean$mode_factor <- factor(song_popularity_clean$audio_mode, 
                                            levels = c(0, 1), 
                                            labels = c("Minor", "Major"))

plot_mode <- ggplot(song_popularity_clean, aes(x = mode_factor, y = song_popularity, fill = mode_factor)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 1, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("lightcoral", "lightgreen")) +
  labs(title = "Popularity by Audio Mode",
       subtitle = "Comparison of Distributions (Major vs Minor)",
       x = "Audio Mode",
       y = "Song Popularity (0-100)") +
  theme_minimal() +
  theme(legend.position = "none") # אין צורך במקרא כי הצירים ברורים

# הצגת הגרפים
# אופציה א': אם יש לך gridExtra, נציג אותם יחד
if(require(gridExtra)) {
  grid.arrange(plot_time, plot_mode, ncol = 2)
} else {
  # אופציה ב': הדפסה בנפרד
  print(plot_time)
  print(plot_mode)
}

# ביצוע ההסרה בפועל (כמו קודם)
song_popularity_model_data <- song_popularity_clean %>%
  select(-time_signature, -audio_mode)




#########################################################




# --- תיקון והרצה מחדש של חלק 2.2 ---

library(dplyr)

# 1. איפוס הנתונים (חשוב! מריץ מחדש את המצב של סוף 2.1)
# זה מבטיח שהעמודה song_duration_ms קיימת לפני שנתחיל
song_popularity_model_data <- song_popularity_clean %>%
  select(-time_signature, -audio_mode)

# 2. המרת משך השיר לדקות (השינוי היחיד שמבוצע בפועל)
song_popularity_model_data <- song_popularity_model_data %>%
  mutate(duration_min = song_duration_ms / 60000) %>%
  select(-song_duration_ms) # כעת זה יעבוד כי איפסנו את הטבלה למעלה

# 3. בדיקת דיסקרטיזציה של Tempo (נבחן ונפסל)
# יצירת משתנה זמני לבדיקה
temp_data_check <- song_popularity_model_data %>%
  mutate(tempo_cat = cut(tempo,
                         breaks = c(-Inf, 122, 140, Inf),
                         labels = c("Standard", "Energetic", "High Speed")))

# ANOVA
anova_model <- aov(song_popularity ~ tempo_cat, data = temp_data_check)
print("--- ANOVA Results for Tempo Categories ---")
print(summary(anova_model))
# מסקנה: p-value גבוה, אין הבדל מובהק -> נשארים עם הרציף.

# 4. בדיקת Instrumentalness (נבחן ונפסל)
cor_continuous <- cor(song_popularity_model_data$instrumentalness, 
                      song_popularity_model_data$song_popularity, use="complete.obs")

temp_binary_inst <- ifelse(song_popularity_model_data$instrumentalness > 0.5, 1, 0)
cor_binary <- cor(temp_binary_inst, 
                  song_popularity_model_data$song_popularity, use="complete.obs")

print("--- Instrumentalness Decision Check ---")
print(paste("Original Continuous Correlation:", round(cor_continuous, 4)))
print(paste("Binary Correlation:", round(cor_binary, 4)))
# מסקנה: הרציף חזק יותר -> נשארים עם הרציף.

# 5. סיכום הנתונים
cat("\n--- Data Summary After Adjustments (Table 2.2) ---\n")
print(glimpse(song_popularity_model_data))




########################################################




# --- חלק 2.3: הגדרת משתני דמה (Dummy Variables) - טווחים מעודכנים ---

library(dplyr)

# 1. חלוקה לקטגוריות לפי הטווחים שהוגדרו
# Standard: עד 122 (כולל שירים מתחת ל-100 כדי לא לאבד נתונים)
# Energetic: 122 עד 140
# High Speed: מעל 140

song_popularity_model_data$tempo_cat <- cut(song_popularity_model_data$tempo,
                                            breaks = c(-Inf, 122, 140, Inf),
                                            labels = c("Standard", "Energetic", "HighSpeed"))

# 2. יצירת משתני הדמה (Dummy Encoding)
# קבוצת הייחוס היא "Standard" (עד 122), לכן לא ניצור לה עמודה.

# משתנה דמה 1: האם השיר הוא בטווח האנרגטי (122-140)?
song_popularity_model_data$tempo_Energetic_dummy <- ifelse(song_popularity_model_data$tempo_cat == "Energetic", 1, 0)

# משתנה דמה 2: האם השיר הוא בטווח המהיר מאוד (140+)?
song_popularity_model_data$tempo_HighSpeed_dummy <- ifelse(song_popularity_model_data$tempo_cat == "HighSpeed", 1, 0)

# 3. ניקוי וסיכום
# הסרת עמודת העזר
song_popularity_model_data <- song_popularity_model_data %>% select(-tempo_cat)

# בדיקת כמות התצפיות בכל קטגוריה (לוודא שהחלוקה תקינה)
cat("\n--- Dummy Variables Counts (Section 2.3) ---\n")
cat("Energetic (122-140 BPM): ", sum(song_popularity_model_data$tempo_Energetic_dummy), "\n")
cat("High Speed (>140 BPM):   ", sum(song_popularity_model_data$tempo_HighSpeed_dummy), "\n")
cat("Standard (Ref Group):    ", nrow(song_popularity_model_data) - sum(song_popularity_model_data$tempo_Energetic_dummy) - sum(song_popularity_model_data$tempo_HighSpeed_dummy), "\n")

# הצצה לנתונים
print(glimpse(song_popularity_model_data %>% select(tempo, starts_with("tempo"))))



######################################################################




#---------------2.4-------------------


# יצירת העמודות החדשות בבסיס הנתונים על ידי הכפלת המשתנים הרלוונטיים
library(ggplot2)
library(dplyr)

song_popularity_model_data$inter_dance_energy   <- song_popularity_model_data$danceability * song_popularity_model_data$energy
song_popularity_model_data$inter_dance_instru   <- song_popularity_model_data$danceability * song_popularity_model_data$instrumentalness
song_popularity_model_data$inter_valence_energy <- song_popularity_model_data$audio_valence * song_popularity_model_data$energy

# --- חלק 2.4: גרפים תלת-ממדיים לאינטראקציות (3D Surface Plots) ---

library(plotly)
library(dplyr)

# פונקציה ליצירת גרף תלת-ממדי
# הפונקציה בונה מודל רגרסיה מקומי ומציירת את "משטח החיזוי"
plot_3d_interaction <- function(data, x_var, y_var, z_var, title_text) {
  
  # 1. בניית מודל עזר לחישוב המשטח
  # אנו חייבים מודל כדי לדעת מה הגובה (Z) בכל נקודה במרחב
  formula_str <- paste(z_var, "~", x_var, "*", y_var)
  model <- lm(as.formula(formula_str), data = data)
  
  # 2. יצירת רשת (Grid) של ערכים ל-X ול-Y
  # אנחנו יוצרים "שתי וערב" של קואורדינטות כדי לצייר עליהן את המשטח
  x_seq <- seq(min(data[[x_var]], na.rm=TRUE), max(data[[x_var]], na.rm=TRUE), length.out = 25)
  y_seq <- seq(min(data[[y_var]], na.rm=TRUE), max(data[[y_var]], na.rm=TRUE), length.out = 25)
  
  # 3. חיזוי הגובה (Z) לכל נקודה ברשת
  z_matrix <- outer(x_seq, y_seq, function(x, y) {
    # יצירת דאטה-פריים זמני לחיזוי
    new_data <- data.frame(setNames(list(x, y), c(x_var, y_var)))
    predict(model, newdata = new_data)
  })
  
  # 4. ציור הגרף עם Plotly
  p <- plot_ly(x = ~x_seq, y = ~y_seq, z = ~z_matrix) %>%
    add_surface(
      colorscale = 'Viridis', # צבעים יפים (סגול-צהוב) שמראים גובה
      opacity = 0.9           # שקיפות קלה
    ) %>%
    layout(
      title = list(text = title_text, y = 0.95),
      scene = list(
        xaxis = list(title = x_var),
        yaxis = list(title = y_var),
        zaxis = list(title = "Popularity (Z)"),
        camera = list(eye = list(x = 1.5, y = 1.5, z = 1.2)) # זווית צפייה ראשונית
      )
    )
  
  return(p)
}

# --- יצירת והצגת הגרפים ---

# 1. Danceability * Energy (השערת המסיבה)
# חפש את האזור הצהוב (גבוה) - האם הוא קיים רק כששניהם גבוהים?
p1_3d <- plot_3d_interaction(song_popularity_model_data, 
                             x_var = "danceability", 
                             y_var = "energy", 
                             z_var = "song_popularity",
                             title_text = "3D Interaction: Danceability * Energy")
print(p1_3d)

# 2. Danceability * Instrumentalness (השערת הפיצוי)
p2_3d <- plot_3d_interaction(song_popularity_model_data, 
                             x_var = "danceability", 
                             y_var = "instrumentalness", 
                             z_var = "song_popularity",
                             title_text = "3D Interaction: Danceability * Instrumentalness")
print(p2_3d)

# 3. Valence * Energy (השערת הלהיט השמח)
p3_3d <- plot_3d_interaction(song_popularity_model_data, 
                             x_var = "audio_valence", 
                             y_var = "energy", 
                             z_var = "song_popularity",
                             title_text = "3D Interaction: Valence * Energy")
print(p3_3d)




# --- יצירת טבלת מקדמים למודל המלא (Coefficients Table) ---

library(dplyr)

# 1. הכנת הנתונים למודל
# סינון העמודות: משאירים רק משתנים מספריים (כדי להימנע משגיאות על עמודות טקסט כמו שם השיר)
final_model_data <- song_popularity_model_data %>% select(where(is.numeric))

# 2. הרצת המודל (Full Model)
# המודל כולל את כל המשתנים שנמצאים כרגע בטבלה
full_model <- lm(song_popularity ~ ., data = final_model_data)

# 3. הפקת סיכום המודל ושליפת הטבלה
model_summary <- summary(full_model)
coefficients_table <- model_summary$coefficients

# 4. הדפסת הטבלה בצורה ברורה
cat("\n==========================================================================\n")
cat("               FULL MODEL COEFFICIENTS TABLE\n")
cat("     (Estimate, Std. Error, t value, Pr(>|t|))\n")
cat("==========================================================================\n")

# הדפסה (ניתן להוסיף digits=4 כדי לקבל פלט קריא יותר ומעוגל)
print(coefficients_table, digits = 4)

cat("==========================================================================\n")
cat("Multiple R-squared:", round(model_summary$r.squared, 5), "\n")
cat("Adjusted R-squared:", round(model_summary$adj.r.squared, 5), "\n")



#############################################################################################



# --- חלק 3.1: בחירת משתנים (Stepwise Selection) ---

# --- הרצת Backward Elimination ויצירת טבלת מקדמים ---

library(dplyr)

# 1. הכנת הנתונים (רק מספרים)
data_for_selection <- song_popularity_model_data %>% select(where(is.numeric))

# 2. הרצת האלגוריתם (Backward)
# מתחילים מהמודל המלא ומורידים משתנים לא רלוונטיים
full_model <- lm(song_popularity ~ ., data = data_for_selection)
best_model_backward <- step(full_model, direction = "backward", trace = 0)

# 3. יצירת הטבלה הסופית (בסגנון שביקשת)
# שליפת המקדמים מהמודל המנצח
final_table <- summary(best_model_backward)$coefficients

# 4. הדפסת הטבלה
cat("\n==========================================================================\n")
cat("               FINAL MODEL (BACKWARD ELIMINATION) RESULTS\n")
cat("==========================================================================\n")
# הדפסה עם דיוק של 5 ספרות (כדי לראות P-values נמוכים)
print(final_table, digits = 5)
cat("==========================================================================\n")
cat("AIC Score:", AIC(best_model_backward), "\n")
cat("R-squared:", summary(best_model_backward)$r.squared, "\n")



# --- הרצת Forward Selection ויצירת טבלת מקדמים ---

library(dplyr)

# 1. הכנת הנתונים (רק מספרים)
data_for_selection <- song_popularity_model_data %>% select(where(is.numeric))

# 2. הגדרת נקודות הקצה
# מודל ריק (התחלה): מכיל רק את נקודת החיתוך
null_model <- lm(song_popularity ~ 1, data = data_for_selection)
# מודל מלא (היעד המקסימלי): מכיל את כל המשתנים האפשריים
full_model <- lm(song_popularity ~ ., data = data_for_selection)

# 3. הרצת האלגוריתם (Forward)
# מתחילים מהריק ומוסיפים משתנים. חובה להגדיר scope!
best_model_forward <- step(null_model, 
                           scope = list(lower = null_model, upper = full_model), 
                           direction = "forward", 
                           trace = 0)

# 4. יצירת הטבלה הסופית
# שליפת המקדמים מהמודל המנצח
final_table_fwd <- summary(best_model_forward)$coefficients

# 5. הדפסת הטבלה
cat("\n==========================================================================\n")
cat("               FINAL MODEL (FORWARD SELECTION) RESULTS\n")
cat("==========================================================================\n")
# הדפסה עם דיוק של 5 ספרות
print(final_table_fwd, digits = 5)
cat("==========================================================================\n")
cat("AIC Score:", AIC(best_model_forward), "\n")
cat("R-squared:", summary(best_model_forward)$r.squared, "\n")




#############################################################################


# --- חלק 3.2 (המשך): בדיקת הנחות המודל ---



library(ggplot2)
library(dplyr)

# 1. יצירת דאטה-פריים לאבחון
# אנחנו לוקחים את הנתונים ישירות מתוך המודל
diag_data <- data.frame(
  Fitted = fitted(best_model_backward),           # התחזיות (ציר X)
  Residuals = rstandard(best_model_backward)      # השאריות המנורמלות (ציר Y)
)

# 2. תרשים שאריות מול תחזיות (Residuals vs. Fitted)
# בודק ליניאריות ושוויון שונויות
p1 <- ggplot(diag_data, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.3, color = "lightblue") +      # הנקודות (שקיפות עוזרת לראות צפיפות)
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # קו האפס
  geom_smooth(method = "loess", color = "red", se = FALSE) + # קו המגמה (האדום)
  labs(title = "Standardized Residuals vs. Fitted Values",
      
       x = "Y estimte",
       y = "Standardized Residuals") +
  theme_minimal()

print(p1)

# 3. תרשים QQ Plot
# בודק נורמליות של השאריות
p2 <- ggplot(diag_data, aes(sample = Residuals)) +
  stat_qq(color = "lightblue", alpha = 0.3) +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(title = "Normal Q-Q Plot",
     
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

print(p2)


##############################################

#-------------3.3------------------




# --- בדיקת הנחת הליניאריות: Ramsey RESET Test ---

# טעינת החבילה הנדרשת
if(!require(lmtest)) install.packages("lmtest")
library(lmtest)

# ביצוע המבחן על המודל הנבחר (best_model_backward)
# power = 2:3 אומר למבחן לבדוק חזקות בריבוע ובשלישית
reset_result <- resettest(best_model_backward, power = 2:3, type = "fitted")

# הצגת התוצאות
print("=== Linearity Test (Ramsey RESET) Results ===")
print(reset_result)


# --- התקנה וביצוע מבחן Kolmogorov-Smirnov ---


# 2. טעינת החבילה
library(nortest)

# 3. ביצוע המבחן (Lilliefors / KS)
# אנו בודקים את השאריות של המודל הנבחר (best_model_backward)
ks_result <- lillie.test(residuals(best_model_backward))

# 4. הצגת התוצאות
print("=== Normality Test (Kolmogorov-Smirnov / Lilliefors) Results ===")
print(ks_result)




#########################################################3


#--------------------     4        ------------------


# --- חלק 4: שיפור המודל (Model Improvement) ---

  
library(dplyr)



# 1. הכנת הנתונים
raw_data <- song_popularity_model_data 

data_engineered <- raw_data %>%
  mutate(
    # טרנספורמציות
    log_duration = log(duration_min), 
    energy_sq = energy^2,                 
    tempo_HighSpeed_dummy = ifelse(tempo > 140, 1, 0),
    
    # אינטראקציות
    inter_dance_instru = danceability * instrumentalness,
    inter_dance_energy = danceability * energy,
    inter_valence_energy = audio_valence * energy
  )

# 2. ניקוי חריגים
temp_model <- lm(song_popularity ~ 
                   acousticness + energy_sq + audio_valence + 
                   tempo_HighSpeed_dummy + inter_dance_instru + 
                   inter_dance_energy + inter_valence_energy, 
                 data = data_engineered)

cooks_d <- cooks.distance(temp_model)
cutoff <- 4/nrow(data_engineered) 
data_final_clean <- data_engineered[cooks_d < cutoff, ]

# 3. הרצת המודל הסופי בהחלט (ללא duration)
final_model <- lm(song_popularity ~ 
                    acousticness + 
                    energy_sq + 
                    audio_valence + 
                    tempo_HighSpeed_dummy + 
                    inter_dance_instru + 
                    inter_dance_energy + 
                    inter_valence_energy, 
                  data = data_final_clean)

# 4. תוצאות
print(summary(final_model))


# --- בדיקת מדד AIC (Akaike Information Criterion) ---

# 1. חישוב AIC למודל הסופי שלנו
final_aic <- AIC(final_model)

# 2. לשם השוואה: חישוב AIC למודל "ריק" (רק הממוצע, בלי משתנים בכלל)
# זה עוזר להוכיח שהמשתנים שלנו באמת תורמים מידע
null_model <- lm(song_popularity ~ 1, data = data_final_clean)
null_aic <- AIC(null_model)

# 3. הצגת התוצאות
cat("\n=== AIC Model Comparison ===\n")
cat("AIC Score:", AIC(best_model_backward), "\n")
cat("Final Model AIC (new):  ", final_aic, "\n")
cat("Difference (Improvement): ", null_aic - AIC(best_model_backward), "\n")

