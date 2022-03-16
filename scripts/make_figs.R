# Create a figure for the SciData paper of the data in the database ie across
# tax, year/doctype, action type and threats
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# load data
db <- read.csv("data-raw/data-out/CAN-SAR_database.csv")

# Set colour scheme to either viridis or grey
scale_fill_discrete <- function(...){
  shades::brightness(scale_fill_viridis_d(...), shades::delta(-0.1))
}

fig_path <- "figs/"

# scale_fill_discrete <- function(...){
#   scale_fill_grey(...)
# }
#
# fig_path <- "figs/Grey_figs/"

# change default theme
new_theme <- theme_classic()+
  theme(legend.text = element_text(size = rel(0.7)),
        legend.title = element_text(size = rel(0.8)),
        legend.key.size = unit(1.4, "lines"),
        strip.background = element_blank(),
        strip.text = element_blank())
theme_set(new_theme)

db <- db %>%
  mutate(taxonomic_group = str_to_sentence(taxonomic_group),
         doc_type = str_remove(doc_type, "COSEWIC ") %>% str_to_sentence() %>%
           factor(levels = c("Status reports", "Recovery strategies", "Management plans")),
         doc_type2 = ifelse(doc_type == "Status reports", "Status", "Recovery") %>%
           factor(levels = c("Status", "Recovery"))) %>%
  group_by(speciesID, doc_type2, doc_type) %>%
  filter(year_published == max(year_published))


taxa <- db %>%
  ggplot(aes(taxonomic_group, fill = doc_type))+
  geom_bar()+
  coord_flip()+
  scale_x_discrete(limits = rev)+
  labs(x = "Taxonomic group", y = "Number of documents", fill = "Document type")+
  theme(legend.direction = "horizontal", legend.position = "right")

# summarise to number of species
taxa_sp <- db %>% group_by(speciesID, doc_type, doc_type2) %>%
  summarise(taxonomic_group = first(taxonomic_group)) %>%
  ggplot(aes(taxonomic_group, fill = doc_type))+
  geom_bar()+
  scale_x_discrete(limits = rev)+
  coord_flip()+
  labs(x = "Taxonomic group", y = "Number of wildlife species",
       fill = "Document type")+
  theme(legend.direction = "horizontal", legend.position = "none")+
  facet_wrap(~doc_type2)

year <- ggplot(db, aes(year_published, fill = doc_type))+
  geom_bar()+
  labs(x = "Year published", y = "Number of wildlife species", fill = "Document type")+
  theme(legend.direction = "horizontal", legend.position = "none")+
  facet_wrap(~doc_type2, nrow = 2)

threat_sum <- db %>% ungroup() %>%
  select(speciesID, common_name, doc_type, matches("X.*identified")) %>%
  pivot_longer(names_to = "threat", values_to = "present",
               cols = c(-speciesID, -common_name, -doc_type)) %>%
  mutate(threat = str_remove(threat, "_threat_identified") %>%
           str_remove("X") %>% as.numeric() %>% as.factor()) %>%
  filter(present == 1) %>%
  group_by(threat, doc_type) %>%
  summarise(N = n()) %>%
  ggplot(aes(threat, N, fill = doc_type))+
  geom_col()+
  labs(x = "Threat", y = "Number of documents", fill = "Document type")+
  theme(legend.direction = "horizontal", legend.position = "none")

l1_th_names <- tribble(
  ~code, ~name,
  1, "Residential & commercial\ndevelopment",
  2, "Agriculture & aquaculture",
  3, "Energy production & mining",
  4, "Transportation &\nservice corridors",
  5, "Biological resource use",
  6, "Human intrusions\n& disturbance",
  7, "Natural system modifications",
  8, "Invasive & other problematic\nspecies & genes",
  9, "Pollution",
  10, "Geological events",
  11, "Climate change\n& severe weather"
)

threat_sum_l1 <- db %>%
  select(speciesID, common_name, doc_type, doc_type2,
         matches("X\\d\\d?_.*identified")) %>%
  pivot_longer(names_to = "threat", values_to = "present",
               cols = c(-speciesID, -common_name, -doc_type, -doc_type2)) %>%
  mutate(threat = str_remove(threat, "_threat_identified") %>%
           str_remove("X") %>% as.numeric()) %>%
  filter(present == 1) %>%
  group_by(threat, doc_type, doc_type2) %>%
  summarise(N = n()) %>%
  left_join(l1_th_names, by = c(threat = "code")) %>%
  mutate(name = factor(name, levels = rev(l1_th_names$name))) %>%
  ggplot(aes(name, N, fill = doc_type))+
  geom_col()+
  scale_y_continuous(breaks = c(0,200, 400))+
  labs(x = "Threat", y = "Number of wildlife species", fill = "Document type")+
  theme(legend.direction = "horizontal", legend.position = "none")+
  coord_flip()+
  facet_wrap(~doc_type2)

action_types <- db %>% ungroup() %>% select(rowID, common_name, action_type, doc_type) %>%
  separate_rows(action_type, sep = ", ") %>%
  mutate(action_type = factor(action_type,
                       levels = c("Outreach and stewardship",
                                  "Research and monitoring",
                                  "Habitat management",
                                  "Population management"))) %>%
  filter(!is.na(action_type)) %>%
  ggplot(aes(reorder(action_type, desc(action_type)), fill = doc_type))+
  geom_bar()+
  scale_x_discrete(labels =rev(c("Outreach &\nstewardship",
                             "Research &\nmonitoring",
                             "Habitat\nmanagement",
                             "Population\nmanagement")))+
  scale_fill_manual(values = shades::brightness(viridis::viridis(3),
                                                shades::delta(-0.1))[2:3])+
  labs(x = "Action type", y = "Number of wildlife species", fill = "Document type")+
  theme(legend.direction = "horizontal", legend.position = "none")+
  coord_flip()

cp1 <- cowplot::plot_grid(taxa_sp, threat_sum_l1,
                   #legend = "bottom", common.legend = TRUE,
                   labels = c("a", "c"), align = "v", axis = "l", ncol = 1)

cp2 <- cowplot::plot_grid(year, action_types,
                          #legend = "bottom", common.legend = TRUE,
                          labels = c("b", "d"), align = "v", axis = "l", ncol = 1)

cowplot::plot_grid(cp1, cp2, rel_widths = c(3,2))

ggsave(paste0(fig_path, "CANSARD_summary.png"), width = 183, height = 175, units = "mm")

ggsave(paste0(fig_path, "CANSARD_summary.eps"),
       plot = cowplot::plot_grid(cp1, cp2, rel_widths = c(3,2)),
       width = 183, height = 175, units = "mm")

ggsave(paste0(fig_path, "CANSARD_tax_grps.png"),
       taxa_sp + theme(legend.position = "none"), width = 110,
       height = 80, units = "mm")

ggsave(paste0(fig_path, "CANSARD_year_pub.png"), year, width = 110,
       height = 80, units = "mm")

ggsave(paste0(fig_path, "CANSARD_threats_l1.png"), threat_sum_l1, width = 110,
       height = 80, units = "mm")

ggsave(paste0(fig_path, "CANSARD_action_types.png"), action_types, width = 110,
       height = 80, units = "mm")

ggsave(paste0(fig_path, "CANSARD_legend_only.png"), ggpubr::get_legend(taxa),
       width = 150,
       height = 20, units = "mm")
