library(dplyr)
conversion_factors <- tibble::tribble(
  ~parameter, ~metric_units, ~mol_weight, ~description,
  # 2012 AKI Guideline
  "SAmk", "ug/ml", 585.6, "Amikacin (serum, plasma)",
  "BUN", "mg/dl", 28.014, "Blood urea nitrogen",
  "SiCa", "mg/dl", 40.08, "Calcium, ionized (serum)",
  "SCr", "mg/dl", 113.120, "Creatinine (serum)",
  "CLcr", "ml/min", NA, "Creatinine clearance",
  "CGen", "ug/ml", 477.596, "Gentamicin (serum)",
  "Glc", "mg/dl", 180.156, "Glucose",
  "Lac", "mg/dl", 90.08, "Lactate (plasma)",
  "STob", "ug/ml", 467.5, "Tobramycin (serum, plasma)",
  "Urea", "mg/dl", 60.06, "Urea (plasma)" # changed from AKI 2012 Guideline
) %>%
  dplyr::mutate(mol_weight = units::set_units(mol_weight, "g/mol"))
