# Create a figure for the SciData paper of the data in the database ie across
# tax, year/doctype, action type and threats
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# load data
db <- read.csv("data-raw/data-out/CAN_SARD.csv")

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
        legend.key.size = unit(1.4, "lines"))
theme_set(new_theme)

db <- db %>%
  mutate(large_taxonomic_group = str_to_sentence(large_taxonomic_group),
         doc_type = str_remove(doc_type, "COSEWIC ") %>% str_to_sentence())

taxa <- db %>%
  ggplot(aes(large_taxonomic_group, fill = doc_type))+
  geom_bar()+
  coord_flip()+
  labs(x = "Taxonomic group", y = "Number of documents", fill = "Document type")+
  theme(legend.direction = "horizontal", legend.position = "right")

# summarise to number of species
taxa_sp <- db %>% group_by(uID) %>%
  summarise(large_taxonomic_group = first(large_taxonomic_group)) %>%
  ggplot(aes(large_taxonomic_group))+
  geom_bar()+
  coord_flip()+
  labs(x = "Taxonomic group", y = "Number of species/DUs")

year <- ggplot(db, aes(year_published, fill = doc_type))+
  geom_bar()+
  labs(x = "Year published", y = "Number of documents", fill = "Document type")+
  theme(legend.direction = "horizontal", legend.position = "none")

threat_sum <- db %>%
  select(uID, common_name, doc_type, matches("X.*identified")) %>%
  pivot_longer(names_to = "threat", values_to = "present",
               cols = c(-uID, -common_name, -doc_type)) %>%
  mutate(threat = str_remove(threat, "_threat_identified") %>%
           str_remove("X") %>% as.numeric() %>% as.factor()) %>%
  filter(present == 1) %>%
  group_by(threat, doc_type) %>%
  summarise(N = n()) %>%
  mutate(prop = N/sum(N)) %>%
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
  select(docID, common_name, doc_type, matches("X\\d\\d?_.*identified")) %>%
  pivot_longer(names_to = "threat", values_to = "present",
               cols = c(-docID, -common_name, -doc_type)) %>%
  mutate(threat = str_remove(threat, "_threat_identified") %>%
           str_remove("X") %>% as.numeric()) %>%
  filter(present == 1) %>%
  group_by(threat, doc_type) %>%
  summarise(N = n()) %>%
  mutate(prop = N/sum(N)) %>%
  left_join(l1_th_names, by = c(threat = "code")) %>%
  mutate(name = factor(name, levels = rev(l1_th_names$name))) %>%
  ggplot(aes(name, N, fill = doc_type))+
  geom_col()+
  labs(x = "Threat", y = "Number of documents", fill = "Document type")+
  theme(legend.direction = "horizontal", legend.position = "none")+
  coord_flip()

action_types <- db %>% select(docID, common_name, action_type, doc_type) %>%
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
                                                shades::delta(-0.1))[1:3])+
  labs(x = "Action type", y = "Number of documents", fill = "Document type")+
  theme(legend.direction = "horizontal", legend.position = "none")+
  coord_flip()

ggpubr::ggarrange(taxa, year, threat_sum_l1, action_types, legend = "bottom",
                  common.legend = TRUE,
                  labels = "auto")

ggsave(paste0(fig_path, "CANSARD_summary.png"), width = 183, height = 175, units = "mm")

ggsave(paste0(fig_path, "CANSARD_summary.eps"), width = 183, height = 175, units = "mm")

ggsave(paste0(fig_path, "CANSARD_tax_grps.png"),
       taxa + theme(legend.position = "none"), width = 110,
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
