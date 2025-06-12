library(pavo)

# 1. Loading spectra
specs <- getspec(
  where = getwd(),
  ext = "txt",
  lim = c(300, 700),
  decimal = ",",
  sep = NULL,
  subdir = FALSE,
  subdir.names = FALSE,
  ignore.case = TRUE
)

# 2. Processing and smoothing spectra
specs_proc <- procspec(specs, opt = "smooth", fixneg = "addmin")

# 3. Running model
vismod <- vismodel(specs_proc, visual = "avg.uv", achromatic = "bt.dc")

# 4. Calculating coordenates
cores <- colspace(vismod, space = "tcs")

# 5. Calculating hue, chroma and brightness mean
medias <- colMeans(cores[, c("h.theta", "r.vec", "lum")], na.rm = TRUE)

# 6. Naming
names(medias) <- c("hue", "chroma", "brightness")

# 7. Show means
print(medias)
