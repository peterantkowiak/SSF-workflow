# saving first Rdata
save(xmpl, file = "P:/Henriette/BestPracticeR/SSF-workflow/Report/xmpl.Rdata")
save(xmpl.ltr, file = "P:/Henriette/BestPracticeR/SSF-workflow/Report/xmpl.ltr.Rdata")
save(xmpl.cut, file = "P:/Henriette/BestPracticeR/SSF-workflow/Report/xmpl.cut.Rdata")
save(xmpl.steps, file = "P:/Henriette/BestPracticeR/SSF-workflow/Report/xmpl.steps.Rdata")
save(xmpl.spdf, file = "P:/Henriette/BestPracticeR/SSF-workflow/Report/xmpl.spdf.Rdata")
# sould be total final table with everything ready for rambo:
save(xmpl.steps.spdf, file = "P:/Henriette/BestPracticeR/SSF-workflow/Report/xmpl.steps.spdf.Rdata")



# loading the same data
load("P:/Henriette/BestPracticeR/SSF-workflow/Report/xmpl.Rdata")
load("P:/Henriette/BestPracticeR/SSF-workflow/Report/xmpl.spdf.Rdata")
load("P:/Henriette/BestPracticeR/SSF-workflow/Report/xmpl.cut.Rdata")
load("P:/Henriette/BestPracticeR/SSF-workflow/Report/xmpl.steps.Rdata")
load("P:/Henriette/BestPracticeR/SSF-workflow/Report/xmpl.steps.spdf.Rdata")

# loading all spatial covariates
ruggedness <- raster("P:/SSF PROJECT/NEW GIS LAYERS/tri1") 
landcover <- raster("P:/SSF PROJECT/NEW GIS LAYERS/lc_30") 
canopycover <- raster("P:/SSF PROJECT/NEW GIS LAYERS/cc_abmt") 
disthighway <- raster("P:/SSF PROJECT/NEW GIS LAYERS/disthwy")
distroad <- raster("P:/SSF PROJECT/NEW GIS LAYERS/distsmrd")

# saving the raster makes no sense because it only stores a link to the actual folder - risky!
#save(ruggedness, file = "P:/Henriette/BestPracticeR/SSF-workflow/Report/ruggedness.Rdata")
#load("P:/Henriette/BestPracticeR/SSF-workflow/Report/ruggedness.Rdata")
# so don't use!