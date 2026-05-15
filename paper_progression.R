library(httr2)
library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(tibble)

`%||%` <- function(x, y) if (is.null(x)) y else x

author_name <- "cody szuwalski"
start_year <- 2009
end_year <- as.integer(format(Sys.Date(), "%Y"))

# -----------------------------
# 1) Resolve author
# -----------------------------
res_auth <- request("https://api.openalex.org/authors") |>
  req_url_query(search = author_name, per_page = 5) |>
  req_perform() |>
  resp_body_string() |>
  fromJSON()

auth_candidates <- as_tibble(res_auth$results) |>
  select(display_name, id, works_count)

print(auth_candidates)

author_id_full <- auth_candidates$id[1]
author_id <- str_extract(author_id_full, "A\\d+")
matched_name <- auth_candidates$display_name[1]

cat("Using author:", matched_name, "\n")
cat("Using author ID:", author_id, "\n")

# -----------------------------
# 2) Pull works for this author
# -----------------------------
get_works_page <- function(page, author_id) {
  txt <- request("https://api.openalex.org/works") |>
    req_url_query(
      filter = paste0(
        "authorships.author.id:", author_id,
        ",from_publication_date:", start_year, "-01-01"
      ),
      per_page = 100,
      page = page
    ) |>
    req_perform() |>
    resp_body_string()
  
  jsonlite::parse_json(txt, simplifyVector = FALSE)
}

all_results <- list()
page <- 1

repeat {
  res <- get_works_page(page, author_id)
  results <- res$results
  
  if (length(results) == 0) break
  
  all_results <- c(all_results, results)
  cat("Fetched works page", page, "with", length(results), "records\n")
  
  if (length(results) < 100) break
  page <- page + 1
}

# -----------------------------
# 3) Paper-level metadata
# -----------------------------
papers_df <- map_dfr(all_results, function(w) {
  this_work_id_full <- w$id %||% NA_character_
  this_work_id <- str_extract(this_work_id_full, "W\\d+")
  this_title <- w$display_name %||% NA_character_
  this_year <- as.integer(w$publication_year %||% NA_integer_)
  
  authorships <- w$authorships
  first_author_flag <- FALSE
  
  if (!is.null(authorships) && length(authorships) >= 1) {
    first_authorship <- authorships[[1]]
    first_author_id_full <- first_authorship$author$id %||% NA_character_
    first_author_id <- str_extract(first_author_id_full, "A\\d+")
    first_author_flag <- !is.na(first_author_id) && first_author_id == author_id
  }
  
  tibble(
    work_id = this_work_id,
    paper_title = this_title,
    paper_year = this_year,
    first_author = first_author_flag
  )
}) |>
  distinct(work_id, .keep_all = TRUE) |>
  filter(!is.na(work_id), !is.na(paper_year), paper_year >= start_year)

print(papers_df, n = 65)

# -----------------------------
# 4) Citation counts by year for one paper
# Uses OpenAlex filter: cites:<work_id>
# and group_by=publication_year
# -----------------------------
get_citations_by_year <- function(work_id, paper_year, paper_title, first_author) {
  txt <- request("https://api.openalex.org/works") |>
    req_url_query(
      filter = paste0("cites:", work_id),
      group_by = "publication_year",
      per_page = 200
    ) |>
    req_perform() |>
    resp_body_string()
  
  res <- jsonlite::fromJSON(txt)
  
  # If there are no citations yet, return zero-filled years
  if (length(res$group_by) == 0 || is.null(res$group_by)) {
    return(
      tibble(
        year = paper_year:end_year,
        paper_title = paper_title,
        paper_year = paper_year,
        first_author = first_author,
        annual_citations = 0L
      ) |>
        mutate(cumulative_citations = cumsum(annual_citations))
    )
  }
  
  # group_by result usually has key / key_display_name / count
  cite_years <- as_tibble(res$group_by) |>
    transmute(
      year = as.integer(key),
      annual_citations = as.integer(count)
    )
  
  tibble(year = paper_year:end_year) |>
    left_join(cite_years, by = "year") |>
    mutate(
      annual_citations = replace_na(annual_citations, 0L),
      paper_title = paper_title,
      paper_year = paper_year,
      first_author = first_author,
      cumulative_citations = cumsum(annual_citations)
    ) |>
    select(year, paper_title, paper_year, first_author,
           annual_citations, cumulative_citations)
}

# -----------------------------
# 5) Build paper-year panel
# One row per paper-year
# -----------------------------
paper_year_panel <- pmap_dfr(
  list(
    papers_df$work_id,
    papers_df$paper_year,
    papers_df$paper_title,
    papers_df$first_author
  ),
  function(work_id, paper_year, paper_title, first_author) {
    cat("Getting citation history for:", paper_title, "\n")
    out <- get_citations_by_year(
      work_id = work_id,
      paper_year = paper_year,
      paper_title = paper_title,
      first_author = first_author
    )
    Sys.sleep(0.1)
    out
  }
) |>
  arrange(paper_year, paper_title, year)

print(paper_year_panel, n = 50)

library(ggplot2)
ggplot(paper_year_panel, aes(x = year, y = annual_citations , fill = paper_title)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Year",
    y = "Citations",
    fill = "Paper",
    title = "Citations by Paper Over Time"
  ) +
  theme_bw() +
  theme(
    legend.position = "none"
  )


df <- paper_year_panel %>%
  mutate(
    paper_label = paste0(paper_year, ": ", substr(paper_title, 1, 35))
  )




library(dplyr)
library(ggplot2)
library(viridis)
library(patchwork)

df <- df %>%
  group_by(year, paper_label, paper_title, paper_year, first_author) %>%
  summarise(
    annual_citations = sum(annual_citations, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(paper_label, year) %>%
  group_by(paper_label) %>%
  mutate(
    cumulative_citations = cumsum(annual_citations)
  ) %>%
  ungroup()

df <- df[!grepl("appendix", df$paper_title, ignore.case = TRUE), ]

all_paper <- ggplot(
  df,
  aes(x = year, y = annual_citations, fill = paper_label)
) +
  geom_col() +
  labs(
    x = "Year",
    y = "Citations",
    fill = "Paper",
    title = "Citations by Paper Over Time"
  ) +
  theme_bw() +
  theme(legend.position = "right")

first_author <- ggplot(
  df %>% filter(first_author),
  aes(x = year, y = annual_citations, fill = paper_label)
) +
  geom_col() +
  scale_fill_viridis_d(option = "turbo", name = "Paper") +
  labs(
    title = "First-Author Paper Citations Over Time",
    x = "Year",
    y = "Citations",
    fill = "Paper"
  ) +
  theme_bw() +
  theme(legend.position = "right")

sing_traj <- ggplot(
  df %>% filter(first_author),
  aes(x = year, y = cumulative_citations, color = paper_label)
) +
  geom_line(linewidth = 1.2) +
  scale_color_viridis_d(
    option = "turbo",
    name = "Paper",
    guide = "none"   # <- important
  ) +
  labs(
    title = "First-Author Cumulative Citations Over Time",
    x = "Year",
    y = "Cumulative citations"
  ) +
  theme_bw()

bottom_row <-
  (first_author | sing_traj) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

final_plot <-
  all_paper / bottom_row +
  plot_layout(heights = c(2, 1))


final_plot

ggsave(
  filename = "paper_widget/citations_szuwalski.png",
  plot = final_plot,
  width = 20,      # inches
  height = 10,      # inches
  dpi = 300        # publication quality
)
