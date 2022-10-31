#install.packages("lhs")

modify_A44.share_serv_fuel <- function(command, ...) {
  if(command == driver.DECLARE_MODIFY) { # objects that will be modified by the function
    return(c(FILE = "energy/A44.share_serv_fuel"))
  } else if(command == driver.DECLARE_INPUTS) { # objects that are inputted from the data system, but not modified
    return(c())
  } else if(command == driver.MAKE) {

    A44.share_serv_fuel <- get_data(load_csv_files("energy/A44.share_serv_fuel", optionals = F), 'energy/A44.share_serv_fuel')

    set.seed(SEED_NUM)

    A44.share_serv_fuel <- A44.share_serv_fuel %>%
      filter(share_TFEbysector != 0) %>%
      # sample by group
      group_by(region_GCAM3, sector) %>%
      mutate(LHS = lhs::randomLHS(1, dplyr::n())[1,] ) %>%
      ungroup %>%
      # convert to increase/decrease, within +- 0.1 of current value, but between 0.00001 and 1
      mutate(adder = 0.2 * LHS - 0.1,
             share_LHS_unscaled = pmin(pmax(0.00001, share_TFEbysector + adder), 1)) %>%
      # scale total to 1
      group_by(region_GCAM3, sector) %>%
      mutate(share_TFEbysector_scaled = share_LHS_unscaled / sum(share_LHS_unscaled) ) %>%
      ungroup %>%
      select(-share_TFEbysector, -adder, -share_LHS_unscaled, -LHS) %>%
      rename(share_TFEbysector = share_TFEbysector_scaled)

    # Produce outputs
    return_modified("energy/A44.share_serv_fuel" = A44.share_serv_fuel)
  } else {
    stop("Unknown command")
  }
}

devtools::load_all()


for (i in 1:3){
  drake::clean(list="modify_A44.share_serv_fuel")
  SEED_NUM <- i

  driver_drake(user_modifications = c("modify_A44.share_serv_fuel"),
               xml_suffix = paste0("__", i))
}


### Visualization ---- Still way too messy----------
drake::clean(list="modify_A44.share_serv_fuel")
SEED_NUM <- 1
gcamdata_plan <- driver_drake(user_modifications = c("modify_A44.share_serv_fuel"),
                              xml_suffix = paste0("__", SEED_NUM),
                              return_plan_only = T)
outdated_files <- drake::outdated(gcamdata_plan)
outdated_plan <- gcamdata_plan %>%
  filter(target %in% outdated_files)

changed_xmls <- c("building_det.xml", "all_energy_emissions.xml", "all_fgas_emissions.xml", "ghg_emissions_USA.xml", "building_SSP2.xml")

precursor_list <- c()
for (xml_fn in changed_xmls){
  tmp <- dstrace(xml_fn, print = F)
  precursor_list <- c(precursor_list, tmp$object_name)
}

targets_to_build <- outdated_plan %>%
  filter(target %in% precursor_list)

commands_to_build <- unique(stringr::str_extract(targets_to_build$command, "^module_.*(?=\\[)"))

modify_command <- outdated_plan %>%
  filter(grepl("modify", target) | grepl("modify", command))

outdated_plan_filtered <- outdated_plan %>%
  filter(target %in% c(targets_to_build$target, commands_to_build, modify_command$target))

# vis_drake_graph(outdated_plan_filtered, targets_only = TRUE, show_output_files = FALSE)

# graph_chunks() modified-------------------------------------------------------------

chunklist = find_chunks(include_disabled = FALSE) %>%
  filter(name %in% outdated_plan_filtered$target)
chunklist$modulenum <- as.numeric(as.factor(chunklist$module))
vertexcolors <- palette()

chunkinputs <- chunk_inputs(chunklist$name)
chunkoutputs <- chunk_outputs(chunklist$name)


# Filter (unless caller has asked to include disabled chunks)
chunklist <- filter(chunklist, !disabled | FALSE)



chunklist$num <- seq_len(nrow(chunklist))
chunkinputs %>%
  left_join(chunklist, by = "name") %>%
  select(name, input, num) ->
  chunkinputs
if(!quiet) cat("Found", nrow(chunkinputs), "chunk data requirements\n")
chunkoutputs %>%
  left_join(chunklist, by = "name") %>%
  select(name, output, to_xml, num) ->
  chunkoutputs
if(!quiet) cat("Found", nrow(chunkoutputs), "chunk data products\n")

# Compute number of outputs
chunkoutputs %>%
  group_by(name) %>%
  summarise(noutputs = dplyr::n()) %>%
  right_join(chunklist, by = "name") ->
  chunklist

chunklist <- chunklist %>%
  mutate(chunk = case_when(
    grepl("xml", chunk) ~ sub("batch_", "", chunk),
    grepl("^L(A|B)?[0-9]{3,4}", chunk) ~ stringr::str_extract(chunk, "^L(A|B)?[0-9]{3,4}"),
    TRUE ~ chunk
  ))

# Compute edges (dependencies)
chunkinputs %>%
  inner_join(chunkoutputs, by = c("input" = "output")) ->
  edgelist

# Make an adjacency matrix
mat <- matrix(0, nrow = nrow(chunklist), ncol = nrow(chunklist))
colnames(mat) <- chunklist$chunk
for(i in seq_len(nrow(edgelist))) {
  mat[edgelist$num.y[i], edgelist$num.x[i]] <- 1
}

colnames(mat)[colnames(mat) == "all_fgas_emissions_xml"] <- "fgas_xml"
colnames(mat)[colnames(mat) == "all_energy_emissions_xml"] <- "en_emiss_xml"
colnames(mat)[colnames(mat) == "building_det_xml"] <- "bld_xml"
colnames(mat)[colnames(mat) == "building_SSP_xml"] <- "bld_SSP_xml"
colnames(mat)[colnames(mat) == "ghg_emissions_USA_xml"] <- "ghg_USA_xml"

# Plot it
set.seed(1224)
g <- igraph::graph.adjacency(mat)
coords <- igraph::layout_as_tree(g, flip.y = T)

# coords <- igraph::igraph_layout_reingold_tilford(g)



vc <- c("gold", "firebrick", "dodgerblue3")[chunklist$modulenum]

png("~/datasystem/sensitivities/A44_mod.png", height = 1500, width = 1300)
plot(g,
     vertex.color = vc,
     #      vertex.size = chunklist$noutputs p* 3,
     #     vertex.label.dist = 1,
     vertex.label.cex = 2.5,
     vertex.label.color = "black",
     vertex.size = 20,
     edge.arrow.size = 3,
     edge.width = 2,
     layout = coords)
dev.off()


