library(dplyr) 
library(igraph)
library(ggraph)
library(tidygraph)
library(readxl)
library(ggplot2)
library(tidyr)
library(stringr)
library(visNetwork)
library(htmlwidgets)
library(forcats)

dat_disco <- read_xlsx("data/got7_discography.xlsx")
got7 <- c("Jay B", "Mark Tuan", "Jackson Wang", "Jinyoung", "YOUNGJAE", "YUGYEOM", "BamBam")
jj <- c("Jay B", "Jinyoung")
j2 <- c("Jay B", "YUGYEOM")

# Def. is Jay B
disco_pre <- dat_disco %>%
  separate(`Date Released`, into = c("Year", "Month", "Day"), sep = "-") %>%
  select(-c("Month", "Day")) %>%
  
  # Assign the main artist's group once
  mutate(Group = case_when(
    Artist == "GOT7" ~ "GOT7",
    Artist == "JJ Project" ~ "JJ Project",
    Artist == "Jus2" ~ "Jus2",
    Artist == "Def." ~ "Solo (Jay B)",
    TRUE ~ "Solo"
  )) %>%
  
  mutate(Feat_Group = case_when(
    Feat == "GOT7" ~ "GOT7",
    Feat == "JJ Project" ~ "JJ Project",
    Feat == "Jus2" ~ "Jus2",
    Feat == "Def." ~ "Solo (Jay B)",
    TRUE ~ "Solo"
  )) %>% 
  mutate(
    Artist = case_when(
      Artist == "GOT7" ~ paste(got7, collapse = ","),
      Artist == "JJ Project" ~ paste(jj, collapse = ","),
      Artist == "Jus2" ~ paste(j2, collapse = ","),
      Artist == "Def." ~ "Jay B",
      TRUE ~ Artist
    ),
    Feat = case_when(
      Feat == "GOT7" ~ paste(got7, collapse = ","),
      Feat == "JJ Project" ~ paste(jj, collapse = ","),
      Feat == "Jus2" ~ paste(j2, collapse = ","),
      Feat == "Def." ~ "Jay B",
      TRUE ~ Feat
    )
  ) %>%
  rowwise() %>%
  mutate(
    Feat = paste(unique(c(strsplit(Artist, ",\\s*")[[1]], strsplit(Feat, ",\\s*")[[1]])), collapse = ",")
  ) %>%
  ungroup() %>%
  # Separate into individual members
  separate_rows(Artist, sep = ",\\s*") %>%
  separate_rows(Feat, sep = ",\\s*")%>%
  # Remove self-collaborations
  
  filter(Artist != Feat)


#deleting self edges
disco_pre1 <- disco_pre %>%
  mutate(
  Feat = ifelse(Feat == "NA", NA, Feat)
) %>%
  filter(!(Group %in% c("GOT7", "JJ Project", "Jus2") & Feat == "NA"))




#distinct counts for when featured and lead artist

unique_songs_per_artist_feat <- disco_pre1 %>%
  select(SONGS, Artist, Feat, Group, Feat_Group) %>%
  pivot_longer(cols = c(Artist, Feat), names_to = "role", values_to = "Artist") %>%
  filter(!is.na(Artist)) %>%
  distinct(SONGS, Artist, Group, Feat_Group, role)

count_totals_feat <- unique_songs_per_artist_feat %>%
  filter(Artist %in% got7) %>%
  group_by(Group, Feat_Group, Artist, role) %>%
  summarise(total_songs = n_distinct(SONGS))


###---BAR GRAPH---####

plot_data <- count_totals_feat %>%
  mutate(
    song_type = case_when(
      role == "Artist" & Group %in% c("GOT7", "JJ Project", "Jus2") ~ Group,
      role == "Artist" & Group == "Solo (Jay B)" ~ "Solo", #so that it counts when he is labeled as Def. keep this
      role == "Artist" & Group == "Solo" ~ "Solo",
      role == "Feat" ~ "Featured",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(Artist, song_type) %>%
  summarise(total_songs = sum(total_songs), .groups = "drop")%>%
  group_by(Artist) %>%
  mutate(
    percent = 100 * total_songs / sum(total_songs)
  ) %>%
  ungroup()

prop_song <- ggplot(plot_data, aes(
  x = fct_reorder(Artist, -percent, .fun = sum),
  y = percent,
  fill = song_type
)) +
  geom_col(position = "stack", width = 0.8) +  # not "fill"
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Proportion of Songs by Artist (Group, Solo, Featured)",
    x = NULL,
    y = "Percentage of Total Songs",
    fill = "Song Type",
    caption = "Graph by Hortencia Josefina Hernandez\nData: GOT7 Discography · Updated: Nov. 11, 2025"
  ) +
  theme_minimal(base_size = 13) +
  theme(
      axis.text.y = element_text(face = "bold", size = 10),
      axis.text.x = element_text(face = "italic", size = 12),
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 1, face = "italic", size = 10),
      plot.title = element_text(face = "bold", size = 19),
      legend.position = "right"
  ) +
  scale_fill_manual(values = c(
    "GOT7" = "#4CAF50",
    "JJ Project" = "#2196F3",
    "Jus2" = "#9C27B0",
    "Solo" = "#FFC107",
    "Featured" = "#E91E63"
  ))

ggsave("images/got7_disco.png", plot = prop_song, width = 10, height = 8, dpi = 300)

###---NETWORK---###
total <- unique_songs_per_artist_feat %>%
  group_by(Artist) %>%
  summarise(total_songs = n_distinct(SONGS))


edges <- disco_pre1 %>%
  filter(!is.na(Artist) & !is.na(Feat)) %>%
  select(from = Artist, to = Feat) %>%
  separate_rows(from, to, sep = ",\\s*") %>%
  group_by(from, to) %>%
  summarise(weight = n(), .groups = "drop")  %>%
  mutate(
    color = ifelse(from %in% got7 & to %in% got7, "green", "gray70")
  )

disco_graph <- graph_from_data_frame(edges, directed = FALSE)

V(disco_graph)$size <- total$total_songs[match(V(disco_graph)$name, total$Artist)] 

V(disco_graph)$color <- ifelse(V(disco_graph)$name %in% got7, "#3c8350", "gray70")


p1 <- ggraph(disco_graph, layout = "kk") + 
  geom_edge_link(aes(alpha = weight, color = I(color)), width = 0.5) + 
  geom_node_point(aes(size = size, color = I(color))) +  # color by node
  geom_node_text(aes(label = ifelse(name %in% got7, name, "")), size = 1.5, fontface = "bold", repel = FALSE) + 
  scale_size_continuous(range = c(1, 4)) +
  theme_graph() +
  labs(
    title = "GOT7 Network",
    caption = "Graph by Hortencia Josefina Hernandez · Data: GOT7 discography\nIncluding solos, updated: 11 November, 2025"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.caption = element_text(face = "italic", size = 10)
  ) +
  guides(size = "none", edge_alpha = "none")
  
p1
ggsave("images/got7_sna.png", plot = p1, width = 10, height = 8, dpi = 300)


#--- INTERACTIVE----
all_artists <- unique(c(edges$from, edges$to))

nodes <- data.frame(
  id = all_artists,
  label = all_artists,
  stringsAsFactors = FALSE
) %>%
  mutate(
    size = total$total_songs[match(id, total$Artist)],
    color = ifelse(id %in% got7, "#3c8350", "gray70"),
    group = ifelse(id %in% got7, id, NA) 
  )


edges_vis <- edges %>%
  filter(from %in% got7 | to %in% got7) %>%  # keep only edges involving GOT7
  rename(value = weight) %>%
  mutate(
    color = ifelse(from %in% got7 & to %in% got7, "green", "gray70")  # both GOT7 = green
  )

nodes <- nodes %>%
  mutate(
    group = ifelse(id %in% got7, "GOT7", "Other"),
    color = ifelse(group == "GOT7", "#3c8350", "gray70")
  )

graph <- visNetwork(nodes, edges) %>%
  visGroups(groupname = "GOT7", color = "#3c8350") %>%
  visGroups(groupname = "Other", color = "gray70") %>%
  visNodes(
    font = list(size = 50, valign = "top"),
    color = list(
      highlight = list(background = "inherit", border = "inherit"),
      hover = list(background = "inherit", border = "inherit")
    )
  ) %>%
  visEdges(
    smooth = TRUE,
    scaling = list(min = 1, max = 5),
    width = "weight"
  ) %>%
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 1, hover = FALSE),
    nodesIdSelection = list(
      enabled = TRUE,
      values = nodes$id[nodes$group == "GOT7"]  # dropdown only for GOT7 members
    )
  ) %>%
  visPhysics(
    solver = "repulsion",
    repulsion = list(
      nodeDistance = 400,
      springLength = 200,
      springConstant = 0.01,
      damping = 0.1
    ),
    stabilization = list(enabled = FALSE, iterations = 500)
  ) %>%
  visInteraction(navigationButtons = TRUE)

graph


saveWidget(graph, file = "docs/index.html", selfcontained = TRUE)

