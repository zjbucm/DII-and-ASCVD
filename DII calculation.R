library(nhanesR)
dex_DII()

function (data, years, day = 1, rawComponet = FALSE, cat = TRUE, 
          Year = FALSE, join = "left") 
{
  years <- data_years(data, years)
  pb <- txtProgressBar(max = 4, width = 30, style = 3)
  setTxtProgressBar(pb = pb, value = 1)
  dt <- db_drtot(carbohydrate_g = "carbohydrates", protein_g = "protein", 
                 total_fat_g = "tfat", alcohol_g = "alcohol", dietary_fiber_g = "fibre", 
                 cholesterol_mg = "cholesterol", total_sfat_g = "saturated_fat", 
                 total_mfat_g = "MUFA", total_pfat_g = "PUFA", pfa_20.5_g = "n3_1", 
                 pfa_22.5_g = "n3_2", pfa_22.6_g = "n3_3", pfa_18.2_g = "n6_1", 
                 pfa_18.3_g = "n6_2", pfa_18.4_g = "n6_3", pfa_20.4_g = "n6_4", 
                 niacin_mg = "niacin", vitamin_A_rae_mcg = "vitamin_A", 
                 thiamin_vitamin_B1_mg = "thiamin", riboflavin_vitamin_B2_mg = "riboflavin", 
                 vitamin_B6_mg = "vb6", vitamin_B12_mcg = "vb12", vitamin_C_mg = "vitamin_C", 
                 vitamin_D_d2_d3_mcg = "vitamin_D", vitamin_E_as_alpha_tocopherol_mg = "vitamin_E", 
                 iron_mg = "Fe", magnesium_mg = "Mg", zinc_mg = "zinc", 
                 selenium_mcg = "selenium", folic_acid_mcg = "folic_acid", 
                 beta_carotene_mcg = "b_carotene", caffeine_mg = "caffeine", 
                 energy_kcal = "energy", years = years, Year = T, day = day, 
                 fun = "mean")
  if (!missing(data)) 
    dt <- dt[dt$seqn %in% data$seqn, ]
  setTxtProgressBar(pb = pb, value = 2)
  dt <- dt[, !sapply(dt, function(i) all(is.na(i))), drop = FALSE]
  dtnames <- colnames(dt)
  if ("caffeine" %in% colnames(dt)) 
    dt$caffeine <- dt$caffeine/1000
  setTxtProgressBar(pb = pb, value = 3)
  ck <- do::left(colnames(dt), 3) == "n3_"
  colnames(dt)[ck]
  if (any(ck)) {
    n3 <- row.sums(dt[, ck, drop = FALSE])
    dt <- dt[, !ck]
    dt$"n-3_fatty_acids" <- n3
  }
  ck <- do::left(colnames(dt), 3) == "n6_"
  if (any(ck)) {
    n6 <- row.sums(dt[, ck, drop = FALSE])
    dt <- dt[, !ck]
    dt$"n-6_fatty_acids" <- n6
  }
  for (i in 3:ncol(dt)) dt[, i] <- dii(colnames(dt)[i], dt[, 
                                                           i])
  dii <- row.sums(dt[, -c(1:2)])
  dt$dii <- dii
  setTxtProgressBar(pb = pb, value = 4)
  if (cat) 
    cat(crayon::red("\n\ndietary inflammatory index components\n\n"))
  i = 0
  food <- c()
  if ("carbohydrates" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(crayon::red(i, ":"), do::equal_length("carbohydrates", 
                                                                      nchar = 22)))
  }
  if ("protein" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("protein", 
                                                                      nchar = 22)))
  }
  if ("tfat" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("total fat", 
                                                                      nchar = 22)))
  }
  if ("alcohol" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("alcohol", 
                                                                      nchar = 22)))
  }
  if ("fibre" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("fibre", nchar = 22)))
  }
  if ("cholesterol" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("cholesterol", 
                                                                      nchar = 22)))
  }
  if ("saturated_fat" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("saturated fat", 
                                                                      nchar = 22)))
  }
  if ("MUFA" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("MUFA", nchar = 22)))
  }
  if ("PUFA" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("PUFA", nchar = 22)))
  }
  if ("n-3_fatty_acids" %in% colnames(dt)) {
    i = i + 1
    food <- paste0(food, paste0("\n", crayon::red(i, ":"), 
                                "n-3 fatty acids\n"))
  }
  if ("n3_1" %in% dtnames) 
    food <- paste0(food, "            eicosapentaenoic(20:5),\n")
  if ("n3_2" %in% dtnames) 
    food <- paste0(food, "            docosapentaenoic(22:5),\n")
  if ("n3_3" %in% dtnames) 
    food <- paste0(food, "            docosahexaenoic(22:6)\n")
  if ("n-6_fatty_acids" %in% colnames(dt)) {
    i = i + 1
    food <- paste0(food, paste0("\n", crayon::red(i, ":"), 
                                "n-6 fatty acids"))
  }
  if ("n6_1" %in% dtnames) 
    food <- paste0(food, "\n            octadecadienoic(18:2)")
  if ("n6_2" %in% dtnames) 
    food <- paste0(food, "            octadecatrienoic(18:3),\n")
  if ("n6_3" %in% dtnames) 
    food <- paste0(food, "            octadecatetraenoic(18:4),\n")
  if ("n6_4" %in% dtnames) 
    food <- paste0(food, "            eicosatetraenoic(20:4),\n")
  if ("niacin" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("niacin", nchar = 22)))
  }
  if ("vitamin_A" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("vitamin A", 
                                                                      nchar = 22)))
  }
  if ("thiamin" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("thiamin(vitamin B1)", 
                                                                      nchar = 22)))
  }
  if ("riboflavin" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("riboflavin(vitamin B2)", 
                                                                      nchar = 22)))
  }
  if ("vb6" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("vitamin B6", 
                                                                      nchar = 22)))
  }
  if ("vb12" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("vitamin B12", 
                                                                      nchar = 22)))
  }
  if ("vitamin_C" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("vitamin C", 
                                                                      nchar = 22)))
  }
  if ("vitamin_D" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("vitamin D", 
                                                                      nchar = 22)))
  }
  if ("vitamin_E" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("vitamin E", 
                                                                      nchar = 22)))
  }
  if ("Fe" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("Fe", nchar = 22)))
  }
  if ("Mg" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("Mg", nchar = 22)))
  }
  if ("zinc" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("zinc", nchar = 22)))
  }
  if ("selenium" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("selenium", 
                                                                      nchar = 22)))
  }
  if ("folic_acid" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("folic acid", 
                                                                      nchar = 22)))
  }
  if ("b_carotene" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("beta-carotene", 
                                                                      nchar = 22)))
  }
  if ("caffeine" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("caffeine", 
                                                                      nchar = 22)))
  }
  if ("energy" %in% dtnames) {
    i = i + 1
    food <- paste0(food, paste0(ifelse(i%%3 == 0, "\n", "      "), 
                                crayon::red(i, ":"), do::equal_length("energy", nchar = 22)))
  }
  if (cat) 
    cat(food)
  if (!rawComponet) 
    dt <- dt[, c("Year", "seqn", "dii")]
  return_data(data, dt, Year, key = "seqn", join = join)
}
<bytecode: 0x000001bbccf04790>
  <environment: namespace:nhanesR>