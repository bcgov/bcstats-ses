# Activate renv when present (it is on the secure LAN working copies).
# The guard matters for fresh checkouts (CI): renv/ is gitignored, so an
# unguarded source() would error every R session at startup there.
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}
