# Load the 'here' package
library(here)

# Create a list of directories using 'here()' for relative paths
config <- list(
  configDir = here("src", "config"),
  setupDir = here("src", "setup"),
  scriptsDir = here("scripts"),
  srcDir = here("src"),
  dataDir = here("data"),
  outputDir = here("data", "output")
)

# Save the list to an RData file inside the 'src/config' directory
save(config, file = here("src", "config", "directoryConfig.RData"))
