# # consider using Table 7 as the test cases
#
# test_that("aki() for numeric vector of SCr with no baseline", {
#   SCr <- seq(60, 200, by = 20)
#   aki_stages <- forcats::fct_c(
#     .aki_stages[length(.aki_stages)],
#     .aki_stages[length(.aki_stages)],
#     .aki_stages[1],
#     .aki_stages[2],
#     .aki_stages[2],
#     .aki_stages[2],
#     .aki_stages[3],
#     .aki_stages[3]
#   )
#   expect_identical(aki(SCr), aki_stages)
# })
#
# test_that("aki() for numeric vector of SCr with baseline", {
#   SCr <- seq(60, 200, by = 20)
#   aki_stages <- forcats::fct_c(
#     .aki_stages[length(.aki_stages)],
#     .aki_stages[1],
#     .aki_stages[2],
#     .aki_stages[2],
#     .aki_stages[2],
#     .aki_stages[3],
#     .aki_stages[3],
#     .aki_stages[3]
#   )
#   expect_identical(aki(SCr, bCr = 50), aki_stages)
# })
#
# test_that("aki() for vector of SCr in mg/dl with no baseline", {
#   SCr <- units::set_units(seq(1.5, 4.5, by = 0.5), "mg/dl")
#   aki_stages <- forcats::fct_c(
#     .aki_stages[length(.aki_stages)],
#     .aki_stages[length(.aki_stages)],
#     .aki_stages[1],
#     .aki_stages[2],
#     .aki_stages[2],
#     .aki_stages[3],
#     .aki_stages[3]
#   )
#   expect_identical(aki(SCr), aki_stages)
# })
#
# test_that("aki() for vector of SCr in mg/dl with baseline", {
#   SCr <- units::set_units(seq(2.0, 4.5, by = 0.5), "mg/dl")
#   bCr <- units::set_units(1.5, "mg/dl")
#   aki_stages <- forcats::fct_c(
#     .aki_stages[length(.aki_stages)],
#     .aki_stages[1],
#     .aki_stages[2],
#     .aki_stages[2],
#     .aki_stages[3],
#     .aki_stages[3]
#   )
#   expect_identical(aki(SCr, bCr), aki_stages)
# })
#
# test_that(".generate_cr_ch() for dataframe with and without pt_id grouping", {
#   # Generate sample data
#   data_ <- data.frame(
#     pt_id_ = c(rep("pt1", 3 + 3), rep("pt2", 3)),
#     dttm_ = c(
#       seq(
#         lubridate::as_datetime("2020-10-18 09:00:00", tz = "Australia/Melbourne"),
#         lubridate::as_datetime("2020-10-20 09:00:00", tz = "Australia/Melbourne"),
#         length.out = 3
#       ),
#       seq(
#         lubridate::as_datetime("2020-10-23 09:00:00", tz = "Australia/Melbourne"),
#         lubridate::as_datetime("2020-10-25 21:00:00", tz = "Australia/Melbourne"),
#         length.out = 3
#       ),
#       seq(
#         lubridate::as_datetime("2020-10-18 10:00:00", tz = "Australia/Melbourne"),
#         lubridate::as_datetime("2020-10-19 10:00:00", tz = "Australia/Melbourne"),
#         length.out = 3
#       )
#     ),
#     SCr_ = c(
#       units::set_units(seq(2.0, 3.0, by = 0.5), "mg/dl"),
#       units::set_units(seq(3.5, 4.0, by = 0.25), "mg/dl"),
#       units::set_units(seq(3.3, 3.5, by = 0.10), "mg/dl")
#     ),
#     bCr_ = c(
#       rep(units::set_units(1.8, "mg/dl"), 3 + 3),
#       rep(units::set_units(3.0, "mg/dl"), 3)
#     )
#   )
#
#   # Test with grouping
#   data_gr <- data_[sample(nrow(data_)), ]
#   cr_ch_gr <- cbind(
#     data_[
#       c(2, 3, 3, 5, 6, 8, 9, 9),
#       c("pt_id_", "dttm_", "SCr_")
#     ],
#     data.frame(
#       D.SCr = units::set_units(c(0.5, 0.5, 1.0, 0.25, 0.25, 0.1, 0.1, 0.2), "mg/dl"),
#       D.dttm = lubridate::make_difftime(hours = c(24, 24, 48, 30, 30, 12, 12, 24))
#     )
#   )
#   rownames(cr_ch_gr) <- NULL
#   expect_identical(
#     generate_cr_ch(data_gr, SCr = "SCr_", dttm = "dttm_", pt_id = "pt_id_"),
#     cr_ch_gr
#   )
#
#   # Test without grouping
#   data_ngr <- dplyr::filter(data_gr, .data$pt_id_ == "pt1")
#   cr_ch_ngr <- cr_ch_gr[1:5, 2:5]
#   cr_ch_ngr$D.dttm <- lubridate::make_difftime(days = c(1, 1, 2, 1.25, 1.25))
#   expect_identical(
#     generate_cr_ch(data_ngr, SCr = "SCr_", dttm = "dttm_"),
#     cr_ch_ngr
#   )
# })
#
# test_that("aki() for dataframe of SCr and bCr only", {
#   pt_id <- "pt_id_"
#   dttm <- "dttm_"
#   SCr <- "SCr_"
#   bCr <- "bCr_"
#   aki <- "aki"
#   data_ <- data.frame(
#     pt_id_ = c(rep("pt1", 11 + 7), rep("pt2", 13)),
#     dttm_ = c(
#       seq(
#         lubridate::as_datetime("2020-10-18 09:00:00", tz = "Australia/Melbourne"),
#         lubridate::as_datetime("2020-10-20 09:00:00", tz = "Australia/Melbourne"),
#         length.out = 11
#       ),
#       seq(
#         lubridate::as_datetime("2020-10-23 09:00:00", tz = "Australia/Melbourne"),
#         lubridate::as_datetime("2020-10-25 09:00:00", tz = "Australia/Melbourne"),
#         length.out = 7
#       ),
#       seq(
#         lubridate::as_datetime("2020-10-18 10:00:00", tz = "Australia/Melbourne"),
#         lubridate::as_datetime("2020-10-29 10:00:00", tz = "Australia/Melbourne"),
#         length.out = 13
#       )
#     ),
#     SCr_ = c(
#       units::set_units(seq(2.0, 4.5, by = 0.25), "mg/dl"),
#       units::set_units(seq(2.5, 4.0, by = 0.25), "mg/dl"),
#       units::set_units(seq(3.0, 4.2, by = 0.10), "mg/dl")
#     ),
#     bCr_ = c(
#       rep(units::set_units(1.8, "mg/dl"), 11 + 7),
#       rep(units::set_units(3.0, "mg/dl"), 13)
#     )
#   )
#
#   data <- data_[sample(nrow(data_)), ]
# })
