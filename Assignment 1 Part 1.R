library(fpp3) # time series analysis package
library(tsibble) # tsibble objects
library(lubridate) # for data to work with dates and times
library(feasts) 
# library(fable)
# library(tidyverse) # includes readr, dplyr, tidyr, ggplot2, tibble
# library(ggplot2) # 

# read in the data
data <- readr::read_csv("BirthsAndFertilityRatesAnnual.csv")

# obtaining the data specified
BirthAndFertility <- data |>
  pivot_longer( # change rows to column
    cols = `1960`:`2025`,
    names_to = "Year",
    values_to = "Value", # naming it to remember that it needs to be values
    values_transform = list(Value = as.numeric)
  ) |>
  filter(DataSeries %in% c("Total Fertility Rate (TFR)", "Total Live-Births")) |>
  mutate(Year = as.integer(Year)) |>
  filter(Year != 2025) |> # assignment specified until 2024
  pivot_wider( # taking TFR and Total Live-Births into 2 separate columns according to year
  names_from = DataSeries, 
  values_from = Value
  )

# created a tsibble
BirthAndFertility <- as_tsibble(BirthAndFertility, index = Year)

# checking it is a tsibble
ts_BirthAndFertility
is_tsibble(BirthAndFertility)

# Preliminary (exploratory analysis) {then choosing and fitting models, then using & evaluating a forecasting model}

# plot the Total Fertility Rate (TFR) and Total Live-Births (TLB)
autoplot(BirthAndFertility, `Total Fertility Rate (TFR)`) +
  labs(title = "Initial Plot of Total Fertility Rate (TFR)")
# obvious decreasing trend
# cannot determine seasonality from yearly data
# there might be cyclic but will have to take away trend to find out

autoplot(BirthAndFertility,`Total Live-Births`)+
  labs(title = "Initial Plot of Total Live-Births")
# there is a decreasing trend from 1960 to 2024
# cannot determine seasonality from yearly data
# there might also be cyclic but will have to take away trend to find out

# evidence of the presence of business cycles?
# outliers that need to be explained by experts?

# strength of relationships among variables?
BirthAndFertility |>
  ggplot(aes(x = `Total Fertility Rate (TFR)`, y = `Total Live-Births`)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Total Fertility Rate (TFR) vs Total Live-Births (TLB)")
# there seems to be a nonlinear positive relationship between TFR and TLB but need to do further analysis
# noting that higher fertility rates will increase births

# lag plots
BirthAndFertility |>
  gg_lag(`Total Fertility Rate (TFR)`, geom = 'point') +
  labs(title = "Lag Plot of Total Fertility Rate (TFR)")
# stronger positive relationships at lags 1 and 2

BirthAndFertility |>
  gg_lag(`Total Live-Births`, geom = 'point') +
  labs(title = "Lag Plot of Total Live-Births (TLB)")
# stronger positive relationships at lags 1 and 2 as well

# autocorrelation
BirthAndFertility |>
  ACF(`Total Fertility Rate (TFR)`, lag_max = 9) |>
  autoplot() +
  labs(title = "ACF Plot for Total Fertility Rate (TFR)")
# lags 1-9 are significant so more lags should be checked
# slow decrease in the ACF as the lags increase indicates a trend

BirthAndFertility |>
  ACF(`Total Live-Births`, lag_max = 9)
  autoplot() +
  labs(title = "ACF Plot for Total Live-Births (TLB)")
# lags 1-6 are significant suggests short-term dependence
# slow decrease in the ACF as the lags increase indicates a trend
# as there are more than one large spike outside the dotted bounds, this series is probably not white noise

BirthAndFertility |>
  ACF(`Total Fertility Rate (TFR)`, lag_max = 24) |>
  autoplot() +
  labs(title = "ACF Plot for Total Fertility Rate (TFR)")
# lags 1-10 are significant suggests strong persistence
# slow decrease in the ACF as the lags increase indicates a trend
# as there are more than one large spike outside the dotted bounds, this series is probably not white noise

# Box-Cox transformation
lambdatfr <- BirthAndFertility |>
  features(`Total Fertility Rate (TFR)`, features = guerrero) |> # Guerrero method
  pull(lambda_guerrero) # Box-Cox transformation parameter = -0.44514
# as lambda is below zero but halfway to -1 suggests moderate transformation required
BirthAndFertility |>
  autoplot(box_cox(`Total Fertility Rate (TFR)`, lambdatfr)) +
  labs(title = latex2exp::TeX(paste0(
    "Transformed Total Fertility Rate (TFR) with $\\lambda$ = ", round(lambdatfr,2))))
# decreasing trend still obvious
# still signs there might also be cyclic but still will have to take away trend to find out

lambdatlb <- BirthAndFertility |>
  features(`Total Live-Births`, features = guerrero) |> # Guerrero method
  pull(lambda_guerrero) # Box-Cox transformation parameter = -0.89992
# as lambda is below zero but close to -1 suggests strong transformation required
BirthAndFertility |>
  autoplot(box_cox(`Total Live-Births`, lambdatlb)) +
  labs(title = latex2exp::TeX(paste0(
    "Transformed Total Live-Births (TLB) with $\\lambda$ = ", round(lambdatlb,2))))
# the decreasing trend from 1960 to 2024 is less prominent
# might still be cyclic but still will have to take away trend to find out

# STL decomposition method: trend
dcmptfr <- BirthAndFertility |>
  model(stl = STL(`Total Fertility Rate (TFR)`))
components(dcmptfr) |>
  as_tsibble() |>
  autoplot(`Total Fertility Rate (TFR)`), colour = "grey") + # raw data
  geom_line(aes(y = trend), colour = "#D55E00") + # trend-cycle component
  labs(title = "Plot of trend-cycle component (orange) and raw data (grey) for TFR")
# little variability to the raw data, more smooth

dcmptlb <- BirthAndFertility |>
  model(stl = STL(`Total Live-Births`))
components(dcmptlb) |>
  as_tsibble() |>
  autoplot(`Total Live-Births`), colour = "grey") + # raw data
  geom_line(aes(y = trend), colour = "#D55E00") + # trend-cycle component
  labs(title = "Plot of trend-cycle component (orange) and raw data (grey) for TLB")
# more variability to the raw data, bigger changes

# STL decomposition
components(dcmptfr) |> autoplot()
# decreasing trend as mentioned earlier

components(dcmptlb) |> autoplot()
# overall decreasing trend but there was an increase from 1980 with a peak in 1991 before decreasing again

# the remainder of both decompositions resemble each other with sharp dip around 1986 and spike 1988

# moving average
BirthAndFertility_MA_TFR <- BirthAndFertility |>
  mutate(`5-MA_TFR` = slider::slide_dbl(`Total Fertility Rate (TFR)`, mean,
                                  .before = 2, .after = 2, .complete = TRUE))
BirthAndFertility_MA_TFR |>
  autoplot(`Total Fertility Rate (TFR)`) +
  geom_line(aes(y = `5-MA_TFR`), colour = "#D55E00") +
  labs(title = "Total Fertility Rate (black) and 5-MA estimate of trend-cycle (orange)")
# the original data was already quite smooth so it matches quite closely with the trend-cycle

BirthAndFertility_MA_TLB <- BirthAndFertility |>
  mutate(`5-MA_TLB` = slider::slide_dbl(`Total Live-Births`, mean,
                                  .before = 2, .after = 2, .complete = TRUE))
BirthAndFertility_MA_TLB |>
  autoplot(`Total Live-Births`) +
  geom_line(aes(y = `5-MA_TLB`), colour = "#D55E00") +
  labs(title = "Total Live-Births (black) and 5-MA estimate of trend-cycle (orange)")
# can see smooth curve but due to the roughness of the data should increase the average for a smoother curve

BirthAndFertility_MA_TLB7 <- BirthAndFertility |>
  mutate(`7-MA_TLB` = slider::slide_dbl(`Total Live-Births`, mean,
                                  .before = 3, .after = 3, .complete = TRUE))
BirthAndFertility_MA_TLB7 |>
  autoplot(`Total Live-Births`) +
  geom_line(aes(y = `7-MA_TLB`), colour = "#D55E00") +
  labs(title = "Total Live-Births (black) and 7-MA estimate of trend-cycle (orange)")
# the curve is smoother at the cost of the ends
