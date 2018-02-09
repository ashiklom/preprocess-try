source(here::here("R", "common.R"))
trait_files <- paste0(c("N_P_LL_SLA", "Vcmax", "Jmax", "Rd"), ".rds")
traits_combined_pre <- here("processed", "traits", trait_files) %>%
  map(read_rds) %>%
  reduce(full_join)

obs_ids <- pull(traits_combined_pre, ObservationID)

ids_species <- trydat %>%
  filter(ObservationID %in% obs_ids) %>%
  distinct(ObservationID, AccSpeciesID) %>%
  collect()

traits_combined <- left_join(traits_combined_pre, ids_species)

mass_traits <- c("SLA", "leaf_lifespan", "Nmass", "Pmass", "Rdmass", "Vcmax_mass", "Jmax_mass")
area_traits <- gsub("mass", "area", mass_traits)

diag_plot <- traits_combined %>%
  gather("trait", "value", -ObservationID, -ReferenceID, na.rm = TRUE) %>%
  ggplot() +
  aes(x = factor(ReferenceID), y = value) +
  geom_jitter(size = 0.05) +
  facet_wrap(~ trait, scales = "free")
diag_plot_log <- diag_plot + scale_y_log10()
if (interactive()) {
  print(diag_plot)
  print(diag_plot_log)
}
ggsave(here("diagnostics", "traits_combined.pdf"), diag_plot)
ggsave(here("diagnostics", "traits_combined_log.pdf"), diag_plot_log)

pdf(here("diagnostics", "traits_combined_pairs.pdf"))
dolog <- function(dat) mutate_all(dat, log10)
traits_combined %>% select(mass_traits) %>% pairs(pch = 19, main = "Natural")
traits_combined %>% select(area_traits) %>% pairs(pch = 19, main = "Natural")
traits_combined %>% select(mass_traits) %>% dolog() %>% pairs(pch = 19, main = "Log")
traits_combined %>% select(area_traits) %>% dolog() %>% pairs(pch = 19, main = "Log")
dev.off()

write_rds(traits_combined, here("processed", "traits", "trait_data.rds"))
