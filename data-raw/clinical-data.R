clinical_obvs <- tibble::tribble(
  ~`Patient Number`, ~`Admission Date`, ~`Admission Time`, ~`Discharge_date`, ~`Discharge_time`, ~`Date of Birth`, ~`Male`,
  "p10001", "05/03/2020", "14:01:00", "10/03/2020", "16:34:00", "09/01/1956", TRUE,
  "p10002", "06/03/2020", "09:10:00", "16/03/2020", "18:51:00", "04/12/1997", TRUE,
  "p10003", "17/03/2020", "12:48:00", "18/03/2020", "09:12:00", "28/05/1973", TRUE,
)

usethis::use_data(clinical_obvs, overwrite = TRUE)
