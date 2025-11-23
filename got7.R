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
library(RColorBrewer)
library(htmltools)

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

# featured artists 
unique_featured_others <- disco_pre1 %>%
  select(SONGS, Artist, Feat, Group) %>%
  filter(Group == "Solo" | Group == "Solo (Jay B)") %>%
  separate_rows(Feat, sep = ",\\s*") %>%
  filter(Artist %in% got7) %>%
  filter(!is.na(Feat), Feat != "", Feat != Artist) %>%
  distinct(SONGS, Artist, Feat) %>%
  group_by(Artist) %>%
  summarise(
    unique_featured_others = n_distinct(SONGS),
    .groups = "drop"
  )

# featured on - excluding GOT7
# Featured others (excluding GOT7)
unique_featured_others <- disco_pre1 %>%
  select(SONGS, Artist, Feat, Group) %>%
  filter(Group %in% c("Solo", "Solo (Jay B)")) %>%
  filter(Artist %in% got7) %>%
  separate_rows(Feat, sep = ",\\s*") %>%
  filter(!is.na(Feat), Feat != "", !(Feat %in% got7)) %>%  # exclude GOT7 members
  distinct(SONGS, Artist, Feat) %>%
  group_by(Artist) %>%
  summarise(
    unique_featured_others = n_distinct(SONGS),
    .groups = "drop"
  )

# Featured on (excluding GOT7)
unique_featured_on <- disco_pre1 %>%
  select(SONGS, Artist, Feat, Group) %>%
  filter(Group %in% c("Solo", "Solo (Jay B)")) %>%
  separate_rows(Feat, sep = ",\\s*") %>%
  filter(Feat %in% got7) %>%
  filter(!is.na(Artist), Artist != "", !(Artist %in% got7)) %>%  # exclude GOT7 members
  distinct(SONGS, Feat) %>%
  group_by(Feat) %>%
  summarise(
    unique_featured_on = n_distinct(SONGS),
    .groups = "drop"
  ) %>%
  rename(Artist = Feat)


got7_feature_tbl <- unique_featured_others %>%
  left_join(unique_featured_on, by = "Artist") %>%
  arrange(desc(unique_featured_on), desc(unique_featured_others))

got7_feature_tbl

tags$table(
  style = "border-collapse: collapse;width: 100%;font-family: sans-serif",
  tags$thead(
    tags$tr(style = "background-color: #CFA9FF",
            tags$th(style="border:1px solid #ccc;padding:8px", "Artist"),
            
            tags$th(style="border:1px solid #ccc;padding:8px", "# Artist Featured"),
            tags$th(style="border:1px solid #ccc;padding:8px", "# Artists Featured On")
    )
  ),
  tags$tbody(
    lapply(seq_len(nrow(got7_feature_tbl)), function(i) {
      row_color <- ifelse(i %% 2 == 1, "#ffffff", "#E8D9FF")
      tags$tr(style = paste0("background-color: ", row_color),
              tags$td(style="border:1px solid #ccc;padding:8px", got7_feature_tbl$Artist[i]),
              tags$td(style="border:1px solid #ccc;padding:8px", got7_feature_tbl$unique_featured_others[i]),
              tags$td(style="border:1px solid #ccc;padding:8px", got7_feature_tbl$unique_featured_on[i])
              
      )
    })
  )
)





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
  summarise(total_songs = sum(total_songs), .groups = "drop") 

# Adjust to ensure featured does not include being in JJ Project, Jus2 or GOT7
adjustments <- plot_data %>%
  group_by(Artist) %>%
  summarise(
    subtract_featured =
      176 +
      ifelse(any(song_type == "JJ Project"), 8, 0) +
      ifelse(any(song_type == "Jus2"), 14, 0)
  )


plot_data <- plot_data %>%
  left_join(adjustments, by = "Artist") %>%
  mutate(
    total_songs = ifelse(
      song_type == "Featured",
      total_songs - subtract_featured,
      total_songs
    )
  )%>%
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
  #separate_rows(from, to, sep = ",\\s*") %>%
  group_by(from, to) %>%
  summarise(weight = n(), .groups = "drop")  %>%
  mutate(
    color = ifelse(from %in% got7 & to %in% got7, "green", "gray70")
  )

disco_graph <- graph_from_data_frame(edges, directed = FALSE)

V(disco_graph)$size <- total$total_songs[match(V(disco_graph)$name, total$Artist)] 
V(disco_graph)$size[is.na(V(disco_graph)$size)] <- 1

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


#---- Network Analysis----
# betweenness (in and out), and closeness


btw <- betweenness(disco_graph, normalize = TRUE)
deg <- degree(disco_graph)
degMax <- which.max(deg)
cl <- closeness(disco_graph, normalized = TRUE)
# View top members
sort(btw, decreasing = TRUE)[1:20]
sort(cl, decreasing = TRUE)[1:20]
sort(deg, decreasing = TRUE)[1:20]

#### Community Detection####

# Louvain
comm <- cluster_louvain(disco_graph, weights = E(disco_graph)$weight)

membership_vec <- membership(comm)        
node_names <- names(membership_vec)
comm_df <- data.frame(
  id = node_names,
  community = as.integer(membership_vec),
  stringsAsFactors = FALSE
)

nodes_df <- data.frame(id = V(disco_graph)$name, stringsAsFactors = FALSE) %>%
  left_join(total, by = c("id" = "Artist")) %>%
  mutate(total_songs = coalesce(total_songs, 1)) %>%
  left_join(comm_df, by = "id")


n_comm <- length(unique(nodes_df$community))
# found 16 various communities - however want to look at how the artists all connect to each GOT7 member



# Community colors
if (n_comm <= 12) {
  pal <- brewer.pal(max(3, n_comm), "Set3")[1:n_comm]
} else {
  # interpolate a larger palette
  pal <- colorRampPalette(brewer.pal(12, "Paired"))(n_comm)
}
comm_colors <- setNames(pal, sort(unique(nodes_df$community)))


# ensure deg & strength exist and are named vectors
deg <- degree(disco_graph)
strength <- strength(disco_graph, weights = E(disco_graph)$weight)

# add deg/strength to nodes_df
nodes_df <- nodes_df %>%
  mutate(
    deg = deg[id],
    strength = strength[id]
  )

# Build community summary
comm_summary <- nodes_df %>%
  group_by(community) %>%
  summarise(
    community_id = first(community),
    size = n(),
    avg_degree = mean(deg, na.rm = TRUE),
    avg_strength = mean(strength, na.rm = TRUE),
    
    # produce a list of top node names by degree for the group (safe: index-based)
    top_nodes = list(head(id[order(-deg)], 5)),
    
    .groups = "drop"
  ) %>%
  # convert the list column into a single string for display
  mutate(
    top_by_degree = sapply(top_nodes, function(x) paste(x, collapse = ", "))
  ) %>%
  select(-top_nodes) %>%
  arrange(desc(size))


print(comm_summary, n = nrow(comm_summary))


modularity_value <- modularity(comm)

cat("Louvain detected", n_comm, "communities. Modularity =", round(modularity_value, 4), "\n")



nodes_df <- nodes_df %>%
  mutate(
    color_bg = ifelse(id %in% got7,
                      "#3c8350",
                      comm_colors[as.character(community)]),
    color_border = comm_colors[as.character(community)],
    group = paste0("Comm ", community),
    size = scales::rescale(total_songs, to = c(6, 18))
  )

got7_nodes <- got7[got7 %in% V(disco_graph)$name]
distances_to_got7 <- distances(disco_graph, v = got7_nodes, to = V(disco_graph)$name, weights = NA)
#assign to how far artist is from a got7 member
community_assignment <- apply(distances_to_got7, 2, function(x) {
  got7_nodes[which.min(x)]
})

nodes_df$community7 <- community_assignment


# Create a palette for 7 communities
pal7 <- RColorBrewer::brewer.pal(7, "Set2")
comm_colors7 <- setNames(pal7, got7)

nodes_df <- nodes_df %>%
  mutate(
    group = community7,
    color_bg = comm_colors7[community7],
    color_border = color_bg,
    size = scales::rescale(total_songs, to = c(6, 18))
  )


#--- INTERACTIVE----

vis_nodes <- nodes_df %>%
  transmute(
    id = id,
    label = ifelse(id %in% got7, id, ""),
    title = paste0("<b>", id, "</b><br/>Community: ", community7,
                   "<br/>Degree: ", deg[id]),
    group = group,
    value = size,
    color = purrr::pmap(list(color_bg, color_border), function(bg, border) {
      list(background = bg, border = border)
    })
  )

vis_edges <- edges %>%
  mutate(
    color = ifelse(from %in% got7 & to %in% got7, "#3c8350", "gray70"),
    width = scales::rescale(weight, to = c(1, 6))
  ) %>%
  transmute(
    from, to, value = weight, title = paste0("Songs together: ", weight),
    color = lapply(color, function(x) list(color = x)),
    width
  )

net <- visNetwork(vis_nodes, vis_edges, height = "900px", width = "100%") %>%
  visNodes(font = list(size = 40, face = "verdana")) %>%
  visEdges(smooth = TRUE, scaling = list(min = 1, max = 6)) %>%
  visOptions(
    selectedBy = list(variable = "group", style = "width:200px; height:28px;"),
    highlightNearest = list(enabled = TRUE, degree = 1, hover = FALSE),
    nodesIdSelection = FALSE
  ) %>%
  visPhysics(
    solver = "repulsion",
    repulsion = list(
      nodeDistance = 200,  # increase to push nodes farther apart
      springLength = 200,
      springConstant = 0.01,
      damping = 0.1
    ),
    stabilization = list(enabled = FALSE, iterations = 500)
  )%>%
  visInteraction(navigationButtons = TRUE)  %>%
  htmlwidgets::prependContent(
    tags$div(
      style = "text-align:center; margin-bottom:10px;",
      tags$h3("GOT7 Collaboration Network"),
      tags$p("By Hortencia Josefina Hernandez")
    )
    ) %>%
  visLayout(
    improvedLayout = TRUE
  ) 


saveWidget(net, file = "docs/got7_communities.html", selfcontained = TRUE)


# For directionality

edges_directed <- disco_pre1 %>%
  filter(!is.na(Artist) & !is.na(Feat)) %>%
  select(from = Artist, to = Feat) %>%
  group_by(from, to) %>%
  summarise(weight = n(), .groups = "drop")

disco_graph_dir <- graph_from_data_frame(edges_directed, directed = TRUE)
V(disco_graph_dir)$total_songs <- total$total_songs[match(V(disco_graph_dir)$name, total$Artist)]
V(disco_graph_dir)$total_songs[is.na(V(disco_graph_dir)$total_songs)] <- 1

# In-degree / Out-degree
in_deg  <- degree(disco_graph_dir, mode = "in") # featured by others
out_deg <- degree(disco_graph_dir, mode = "out") #features others

btw_full  <- betweenness(disco_graph_dir, directed = TRUE, weights = 1/E(disco_graph_dir)$weight, normalized = TRUE)
clo_out_f <- closeness(disco_graph_dir, mode = "out", weights = 1/E(disco_graph_dir)$weight, normalized = TRUE)
clo_in_f  <- closeness(disco_graph_dir, mode = "in",  weights = 1/E(disco_graph_dir)$weight, normalized = TRUE)

# Metrics table
cent_tbl_full <- tibble(
  Artist        = V(disco_graph_dir)$name,
  in_deg        = in_deg[V(disco_graph_dir)$name],
  out_deg       = out_deg[V(disco_graph_dir)$name],
  betweenness   = btw_full[V(disco_graph_dir)$name],
  closeness_out = clo_out_f[V(disco_graph_dir)$name],
  closeness_in  = clo_in_f[V(disco_graph_dir)$name]
)

cent_tbl_got7 <- cent_tbl_full %>%
  filter(Artist %in% got7) %>%
  select(Artist, betweenness, closeness_out, closeness_in) %>%
  arrange(desc(betweenness), desc(closeness_out), desc(closeness_in))


tags$table(
  style = "border-collapse: collapse;width: 100%;font-family: sans-serif",
  tags$thead(
    tags$tr(style = "background-color: #CFA9FF",
            tags$th(style="border:1px solid #ccc;padding:8px", "Artist"),
            tags$th(style="border:1px solid #ccc;padding:8px", "Betweenness"),
            tags$th(style="border:1px solid #ccc;padding:8px", "Closeness Out"),
            tags$th(style="border:1px solid #ccc;padding:8px", "Closeness In")
    )
  ),
  tags$tbody(
    lapply(seq_len(nrow(cent_tbl_got7)), function(i) {
      row_color <- ifelse(i %% 2 == 1, "#ffffff", "#E8D9FF")
      tags$tr(style = paste0("background-color: ", row_color),
              tags$td(style="border:1px solid #ccc;padding:8px", cent_tbl_got7$Artist[i]),
              tags$td(style="border:1px solid #ccc;padding:8px", round(cent_tbl_got7$betweenness[i], 4)),
              tags$td(style="border:1px solid #ccc;padding:8px", round(cent_tbl_got7$closeness_out[i],4)),
              tags$td(style="border:1px solid #ccc;padding:8px", round(cent_tbl_got7$closeness_in[i], 4))
      )
    })
  )
)



