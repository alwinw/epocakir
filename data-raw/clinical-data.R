clinical_obvs <- tibble::tribble(
  ~`Patient Number`, ~`Admission Date`, ~`Admission Time`, ~`Discharge_date`, ~`Discharge_time`,
  ~`Date of Birth`, ~`Male`, ~`Height`, ~`Surgery`,
  "p10001", "2020-03-05", "14:01:00", "2020-03-10", "16:34:00", "1956-01-09", TRUE, 182, FALSE,
  "p10002", "2020-03-06", "09:10:00", "2020-03-16", "18:51:00", "1997-12-04", FALSE, 161, FALSE,
  "p10003", "2020-03-17", "12:48:00", "2020-03-18", "09:12:00", "1973-05-28", TRUE, 168, TRUE,
)

# Consider finding an alternative with real, open access data instead
usethis::use_data(clinical_obvs, overwrite = TRUE)
