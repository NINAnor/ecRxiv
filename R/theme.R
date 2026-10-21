# =============================================================================
# THEME.R - shared "skog" house style + mechanism-block plotting helpers
# =============================================================================
#
# Shared design tokens, ggplot theme and plotting helpers for the
# condition-model modules. (R/naturTiltak/naturTiltak_burn.R uses its own
# theme_sj().)
#
# Source this file after 00_setup.R, before doing any plotting:
#   if (!exists("REPO_ROOT")) source("R/00_setup.R")
#   source(file.path(REPO_ROOT, "R", "theme.R"))
# =============================================================================
# -----------------------------------------------------------------------------
# House style (mirrors style.R / style.py - see STYLE_GUIDE.md)
# -----------------------------------------------------------------------------
#
# This section is a self-contained copy of the shared "skog" design tokens
# and theme, so this script does not depend on the deck project's file
# layout. If style.R is ever available at a known relative path in this
# project, replace this block with source("path/to/style.R") instead of
# maintaining two copies - the object names below match style.R exactly so
# the swap is a one-line change.
#
# Colour is used the way the deck uses it everywhere else: g3/p3 ("good" /
# "poor") are the two anchor tones, and everything else is a lighter step
# toward the same two colours. That reads as one consistent house style:
#   - triage tiers: succession is a shade of "poor" (p1/p3), restoration a
#     shade of "good" (g1/g3), no-strong-signal is the neutral tone
#   - SJ1-4 composition: SJ1-like (intact) is "good", SJ4-like
#     (late-successional) is "poor" - same two colours, same meaning
#   - tilstand (mapped condition): God is "good", Svært redusert is "poor"
# =============================================================================

# Palette matches the "Kystlynghei National Run" / "Pipeline Figures"
# artifacts (2026-09-22 redesign) - warm sage-stone ground, heather-purple
# accent (Calluna vulgaris, the kystlynghei-defining bloom colour), moss
# green for "good"/restoration, ember/burnt-sienna for "poor"/succession
# (doubles as the fire-related accent elsewhere). Same token NAMES as
# before, so every module that references SKOG_THEME$whatever keeps working
# unchanged - only the values moved.
skog_light <- list(
  paper = "#EEF0E6", surface = "#FFFFFF", surface2 = "#F5F6EF",
  ink = "#202822", ink2 = "#57614E", ink3 = "#8B927E",
  rule = "#DCDDCC", rule2 = "#C7C9B6",
  accent = "#7A5289", accent_soft = "#EFE6F1",
  p4 = "#7A2E17", p3 = "#B14A2A", p2 = "#CC7C5C", p1 = "#E3AA95",
  g1 = "#A8C9A4", g2 = "#7FAE7C", g3 = "#4F7A52", g4 = "#33573A",
  neutral = "#D3D6C4", neutral2 = "#A8AD97", none = "#E7E9DE",
  conf_hi = "#7A2E17", conf_md = "#B14A2A", conf_lo = "#E3AA95",
  cell1 = "#F1E9F3", cell2 = "#DCC3E2", cell3 = "#A26EAE", cell4 = "#7A5289"
)

skog_dark <- list(
  paper = "#161B15", surface = "#1E2419", surface2 = "#232A1D",
  ink = "#ECEEE2", ink2 = "#B4B9A6", ink3 = "#7D8570",
  rule = "#333C2C", rule2 = "#2A321F",
  accent = "#C093CB", accent_soft = "#2E2533",
  p4 = "#33241D", p3 = "#B14A2A", p2 = "#CC8563", p1 = "#E4835C",
  g1 = "#20291D", g2 = "#4F7A52", g3 = "#7FAE7C", g4 = "#86BC7D",
  neutral = "#3B4235", neutral2 = "#6B7364", none = "#232A1D",
  conf_hi = "#E4835C", conf_md = "#B14A2A", conf_lo = "#7A2E17",
  cell1 = "#241E27", cell2 = "#3A2E40", cell3 = "#6B4A75", cell4 = "#C093CB"
)

# Switch the whole deck to dark mode by changing this one line.
SKOG_THEME <- skog_light

# ---- Fonts -------------------------------------------------------------------
# IBM Plex Sans for all text, IBM Plex Mono for data values (on-bar labels,
# table-like figures), resolved via ragg/systemfonts. Each family falls back
# to the first installed alternative (e.g. Arial/Consolas on Windows), so
# every plot renders on any machine.
pick_font <- function(candidates, fallback) {
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    installed <- unique(systemfonts::system_fonts()$family)
    hit <- candidates[candidates %in% installed]
    if (length(hit) > 0) return(hit[[1]])
  }
  fallback
}

FONT_SANS    <- pick_font(c("IBM Plex Sans", "Arial", "Helvetica", "Liberation Sans", "DejaVu Sans"), "sans")
FONT_DISPLAY <- FONT_SANS
FONT_MONO    <- pick_font(c("IBM Plex Mono", "Consolas", "Liberation Mono", "DejaVu Sans Mono"), "mono")

# SVG px sizes from style.py, so any geom_text/annotate size matches the
# same typographic scale the theme uses for titles/axes/captions.
SIZE_PX <- list(
  figure_title = 16, figure_sub = 12.5, eyebrow = 11,
  row_label = 14, value = 14, big = 19, huge = 27,
  tick = 11.5, footnote = 12.5, on_bar = 19
)

# SVG px at `dpi` -> pt, matching style.R/style.py.
skog_pt <- function(px, dpi = 100) px * 72 / dpi

# pt -> the "mm-like" size unit geom_text/annotate expect. Hard-coded rather
# than read from ggplot2's internal .pt constant, which isn't part of the
# package's public interface.
GG_SIZE_UNIT <- 72.27 / 25.4
gg_text_size <- function(token, dpi = 100) skog_pt(SIZE_PX[[token]], dpi) / GG_SIZE_UNIT

# 1688 -> "1 688", as everywhere else in the deck.
skog_num <- function(n) format(n, big.mark = " ", trim = TRUE, scientific = FALSE)

# No frame, no ticks, horizontal hairline grid only, legend off by default -
# "label in place; turn on only for >2 stacked classes" (the guide's rule;
# each plot below overrides legend.position explicitly where that applies).
#
# TYPOGRAPHY (2026-09-22 redesign, matches the artifact house style):
# Fraunces (warm display serif) for the title only, IBM Plex Sans for every
# other label, IBM Plex Mono for on-bar/data-value text via gg_text_size()
# callers that pass family = "IBM Plex Mono" explicitly (geom_text has no
# theme-level hook, so that's set per plot, not here). All three installed
# as OS-level fonts (~/.local/share/fonts) and resolved through ragg/
# systemfonts - see 00_setup.R's headless-Linux font note, which applies
# identically to these.
theme_report <- function(t = SKOG_THEME, base_family = FONT_SANS, dpi = 100) {
  theme_minimal(base_size = skog_pt(14, dpi), base_family = base_family) +
    theme(
      plot.background      = element_rect(fill = t$surface, colour = NA),
      panel.background     = element_rect(fill = t$surface, colour = NA),
      panel.grid.major.y   = element_line(colour = t$rule, linewidth = 0.35),
      panel.grid.major.x   = element_blank(),
      panel.grid.minor     = element_blank(),
      axis.ticks           = element_blank(),
      axis.title           = element_text(colour = t$ink2, size = skog_pt(SIZE_PX$footnote, dpi)),
      axis.text            = element_text(colour = t$ink3, size = skog_pt(SIZE_PX$tick, dpi)),
      plot.title            = element_text(colour = t$ink, hjust = 0, family = FONT_DISPLAY, face = "bold",
                                           size = skog_pt(SIZE_PX$figure_title, dpi) * 1.15,
                                           margin = margin(b = 4)),
      plot.subtitle         = element_text(colour = t$ink2, hjust = 0,
                                           size = skog_pt(SIZE_PX$figure_sub, dpi),
                                           margin = margin(b = 14)),
      plot.caption          = element_text(colour = t$ink3, hjust = 0, size = skog_pt(SIZE_PX$footnote, dpi)),
      plot.caption.position = "plot",
      plot.title.position   = "plot",
      legend.position       = "none",
      legend.key            = element_rect(fill = t$surface, colour = NA),
      legend.text           = element_text(colour = t$ink2, size = skog_pt(SIZE_PX$footnote, dpi)),
      strip.background      = element_rect(fill = t$surface2, colour = NA),
      strip.text            = element_text(colour = t$ink2, face = "bold", size = skog_pt(SIZE_PX$footnote, dpi),
                                           margin = margin(6, 0, 6, 0)),
      plot.margin           = margin(18, 22, 14, 18)
    )
}

# ---- Terminology -------------------------------------------------------------
# "Successional" = moving toward SJ4-like (later-successional) structure.
# "Restorative"   = moving toward SJ1-like (intact) structure.
# Used consistently as the direction words everywhere below; "succession" /
# "restoration" are the matching nouns used for the triage tier labels.
DIRECTION_SUCCESSIONAL <- "Successional (toward SJ4)"
DIRECTION_RESTORATIVE  <- "Restorative (toward SJ1)"
DIRECTION_NO_CHANGE    <- "No net change"

TRIAGE_LEVELS <- c(
  "Strong succession",
  "Moderate succession",
  "No strong signal",
  "Moderate restoration",
  "Strong restoration"
)

TRIAGE_COLOURS <- c(
  "Strong succession"    = SKOG_THEME$p4,
  "Moderate succession"  = SKOG_THEME$p1,
  "No strong signal"     = SKOG_THEME$neutral,
  "Moderate restoration" = SKOG_THEME$g1,
  "Strong restoration"   = SKOG_THEME$g4
)

# Strong vs Moderate also gets its own point shape, so the two tiers stay
# distinguishable even where semi-transparent, overlapping points make a
# colour-only difference hard to see (the mechanism scatter plots).
TRIAGE_SHAPES <- c(
  "Strong succession"    = 17,
  "Moderate succession"  = 16,
  "No strong signal"     = 16,
  "Moderate restoration" = 16,
  "Strong restoration"   = 17
)

DECLINE_TIER_COLOURS  <- TRIAGE_COLOURS[c("Moderate succession", "Strong succession")]
RECOVERY_TIER_COLOURS <- TRIAGE_COLOURS[c("Moderate restoration", "Strong restoration")]

TILSTAND_LEVELS <- c("God", "Moderat", "Dårlig", "Svært redusert")

# God = "good", Svært redusert = "poor" - same two tones as everything else
# (see the note at the top of this section for the trade-off that choice
# makes).
TILSTAND_COLOURS <- c(
  "God"             = SKOG_THEME$g3,
  "Moderat"         = SKOG_THEME$g1,
  "Dårlig"          = SKOG_THEME$p1,
  "Svært redusert"  = SKOG_THEME$p3
)

# change_pattern categories, plain-language versions. Widespread/partial use
# the same two-step logic as the triage tiers; the two "uninformative"
# categories (mixed-direction, within-class) get neutral tones so they recede.
DECLINE_PATTERN_COLOURS <- c(
  "Widespread change"      = SKOG_THEME$p3,
  "Partial change"          = SKOG_THEME$p1,
  "Mixed-direction change"  = SKOG_THEME$neutral2,
  "Within-class shift"       = SKOG_THEME$none
)

RECOVERY_PATTERN_COLOURS <- c(
  "Widespread change"      = SKOG_THEME$g3,
  "Partial change"          = SKOG_THEME$g1,
  "Mixed-direction change"  = SKOG_THEME$neutral2,
  "Within-class shift"       = SKOG_THEME$none
)

SJ_ZONE_LEVELS <- c("SJ1-like", "SJ2-like", "SJ3-like", "SJ4-like")

# SJ1-like (intact) = "good", SJ4-like (late-successional) = "poor" - the
# same four tones used for the triage tiers, so a composition chart and a
# triage chart read as the same colour story.
SJ_ZONE_COLOURS <- c(
  "SJ1-like" = SKOG_THEME$g3,
  "SJ2-like" = SKOG_THEME$g1,
  "SJ3-like" = SKOG_THEME$p1,
  "SJ4-like" = SKOG_THEME$p3
)

# Dominant-SJ-Class transition matrix: grey for no change, red for a shift
# toward a more successional class, blue for a shift toward a more
# restorative one - same colour story again, applied to which side of the
# diagonal a cell falls on rather than to a triage tier.
DOMINANT_DIRECTION_COLOURS <- c(
  "Restoration" = SKOG_THEME$g3,
  "No change"   = SKOG_THEME$neutral2,
  "Succession"  = SKOG_THEME$p3
)

percent_axis <- function(x) paste0(round(100 * x), "%")

# Wrap long caption/subtitle text so it never gets clipped by the plot
# margin. Use for any caption longer than roughly one sentence.
wrap_caption <- function(x, width = 100) stringr::str_wrap(x, width = width)

# Reusable dotted reference-line layers for any plot whose y-axis is the raw
# AE-SJ score. Not used on plots whose axis is a proportion, a structural-mix
# value, or a trend-equivalent change - the SJ medians are not meaningful
# there.
sj_reference_layers <- function(ref_dat, x_pos, t = SKOG_THEME) {
  list(
    geom_hline(
      data = ref_dat,
      aes(yintercept = reference),
      inherit.aes = FALSE,
      linetype = "dotted",
      linewidth = 0.4,
      colour = t$rule2
    ),
    geom_text(
      data = ref_dat %>% mutate(x = x_pos),
      aes(x = x, y = reference, label = label),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = -0.4,
      size = gg_text_size("eyebrow"),
      colour = t$ink2,
      family = FONT_SANS
    )
  )
}

# ---- Mechanism-block plotting helpers ---------------------------------------
# Used identically by the decline (successional) and recovery (restorative)
# blocks below, with different data and colour palettes passed in, so the
# two blocks stay visually and structurally consistent.

build_period_composition_summary <- function(dat, group_vars = "tilstand") {
  base_long <- dat %>%
    transmute(
      fid,
      across(all_of(group_vars)),
      SJ1_like = p1_base, SJ2_like = p2_base,
      SJ3_like = p3_base, SJ4_like = p4_base
    ) %>%
    mutate(period = "2018")
  
  final_long <- dat %>%
    transmute(
      fid,
      across(all_of(group_vars)),
      SJ1_like = p1_final, SJ2_like = p2_final,
      SJ3_like = p3_final, SJ4_like = p4_final
    ) %>%
    mutate(period = "2025")
  
  bind_rows(base_long, final_long) %>%
    tidyr::pivot_longer(
      cols = c(SJ1_like, SJ2_like, SJ3_like, SJ4_like),
      names_to = "sj_zone",
      values_to = "proportion"
    ) %>%
    mutate(
      sj_zone = factor(
        dplyr::recode(
          sj_zone,
          SJ1_like = "SJ1-like", SJ2_like = "SJ2-like",
          SJ3_like = "SJ3-like", SJ4_like = "SJ4-like"
        ),
        levels = SJ_ZONE_LEVELS
      )
    ) %>%
    group_by(across(all_of(c(group_vars, "period", "sj_zone")))) %>%
    summarise(
      n_fids = n_distinct(fid),
      mean_proportion = mean(proportion, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(period = factor(period, levels = c("2018", "2025")))
}

# "2018" here is really the 2018-2019 mean and "2025" the 2024-2025 mean -
# stated once in each calling plot's subtitle rather than on the axis, to
# keep the tick labels short enough not to overlap in narrow facets.
# 4 stacked classes -> legend stays on (overriding the theme's default off).
make_composition_change_plot <- function(period_summary, title, subtitle, t = SKOG_THEME) {
  label_dat <- period_summary %>% filter(mean_proportion >= 0.05)
  
  ggplot(period_summary, aes(x = period, y = mean_proportion, fill = sj_zone)) +
    geom_col(width = 0.6) +
    geom_text(
      data = label_dat,
      aes(label = percent_axis(mean_proportion)),
      position = position_stack(vjust = 0.5),
      colour = t$surface,
      fontface = "bold",
      family = FONT_MONO,
      size = gg_text_size("on_bar")
    ) +
    facet_wrap(~ tilstand, nrow = 1, drop = FALSE) +
    scale_fill_manual(values = SJ_ZONE_COLOURS, drop = FALSE, name = "SJ Class") +
    scale_y_continuous(limits = c(0, 1), labels = percent_axis, expand = expansion(mult = c(0, 0))) +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Mean proportion of site pixels") +
    theme_report() +
    theme(legend.position = "top")
}

# Square panels (coord_fixed) with the three outcome regions labelled
# directly above the point cloud, rather than explained in prose underneath.
# The vertical divider is only drawn in the upper band, because "Mixed
# direction" applies regardless of extent - it is NOT split into two
# quadrants, and the missing line below the coherence threshold is the
# visual cue for that. Strong vs Moderate gets a colour step, a distinct
# point shape (triangle vs circle), AND draw order (Strong drawn last, on
# top), since colour alone is hard to read once points overlap at alpha < 1.
make_mechanism_scatter <- function(dat, tier_colours, title, subtitle, t = SKOG_THEME) {
  dat <- dat %>% arrange(triage_tier == "Strong")
  
  region_labels <- tibble::tibble(
    x = c((WIDESPREAD_CHANGED_THRESHOLD + 1) / 2, WIDESPREAD_CHANGED_THRESHOLD / 2, 0.5),
    y = c(1.06, 1.06, DIRECTIONAL_COHERENCE_THRESHOLD / 2),
    vjust = c(0.5, 0.5, 0.5),
    label = c("Widespread", "Partial", "Mixed direction")
  )
  
  ggplot(
    dat,
    aes(x = prop_changed_any, y = directional_coherence_changed, colour = triage_class, shape = triage_class)
  ) +
    geom_hline(yintercept = DIRECTIONAL_COHERENCE_THRESHOLD, linetype = "dashed", linewidth = 0.4, colour = t$rule2) +
    annotate(
      "segment", x = WIDESPREAD_CHANGED_THRESHOLD, xend = WIDESPREAD_CHANGED_THRESHOLD,
      y = DIRECTIONAL_COHERENCE_THRESHOLD, yend = 1,
      linetype = "dashed", linewidth = 0.4, colour = t$rule2
    ) +
    geom_text(
      data = region_labels,
      aes(x = x, y = y, label = label, vjust = vjust),
      inherit.aes = FALSE, size = gg_text_size("eyebrow"), colour = t$ink3, fontface = "italic",
      family = FONT_SANS
    ) +
    geom_point(alpha = 0.6, size = 1.9) +
    facet_wrap(~ tilstand, nrow = 1, drop = FALSE) +
    scale_colour_manual(values = tier_colours, name = NULL) +
    scale_shape_manual(values = TRIAGE_SHAPES, name = NULL) +
    scale_x_continuous(labels = percent_axis) +
    scale_y_continuous(labels = percent_axis) +
    coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Pixels crossing an SJ Class boundary",
      y = "Changed pixels agreeing with the site trend",
      caption = wrap_caption(paste0(
        "\"Widespread\" = at least ", percent_axis(WIDESPREAD_CHANGED_THRESHOLD), " of a site's pixels changed, consistently. ",
        "\"Partial\" = fewer pixels changed, but still consistently. ",
        "\"Mixed direction\" = the changed pixels agree less than ", percent_axis(DIRECTIONAL_COHERENCE_THRESHOLD),
        " of the time - this can happen at any extent, which is why the vertical line only splits the upper band. ",
        "Points low and left usually reflect a small number of changed pixels, where the agreement share is naturally noisier."
      ))
    ) +
    theme_report() +
    theme(legend.position = "top", plot.margin = margin(32, 22, 14, 18))
}

make_pattern_summary_plot <- function(dat, palette, title, subtitle, t = SKOG_THEME) {
  pattern_dat <- dat %>%
    filter(!is.na(change_pattern), change_pattern != "Below triage threshold") %>%
    count(tilstand, change_pattern, name = "n_fids") %>%
    group_by(tilstand) %>%
    mutate(pct = 100 * n_fids / sum(n_fids)) %>%
    ungroup()
  
  label_dat <- pattern_dat %>% filter(pct >= 5)
  
  ggplot(pattern_dat, aes(x = tilstand, y = pct, fill = change_pattern)) +
    geom_col(width = 0.65, position = "stack") +
    geom_text(
      data = label_dat,
      aes(label = sprintf("%.0f%%", pct)),
      position = position_stack(vjust = 0.5),
      colour = t$surface, fontface = "bold", family = FONT_MONO, size = gg_text_size("footnote")
    ) +
    scale_fill_manual(values = palette, name = NULL) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.02))) +
    coord_flip() +
    labs(
      title = title,
      subtitle = wrap_caption(subtitle),
      x = NULL,
      y = "Share of sites in this category",
      caption = wrap_caption(paste0(
        "Widespread change: most of the site's pixels crossed into a different SJ Class, consistently. ",
        "Partial change: fewer pixels crossed into a different SJ Class, but still consistently. ",
        "Mixed-direction change: the changed pixels split between both directions, with no consistent trend. ",
        "Within-class shift: the site trend is real, but no pixels crossed into a different SJ Class."
      ))
    ) +
    theme_report() +
    theme(legend.position = "right")
}

# Actual annual AE-SJ trajectories for the strongest sites in one direction,
# rather than a single change-per-site summary - lets the room see the real
# time series, not just the amount of change. The SJ reference lines are
# relevant here because the y-axis is the raw AE-SJ score. Each line is
# labelled with its FID directly - with 30 lines the labels will be dense,
# reduce SITE_GRAPH_TOP_N_PER_DIRECTION or facet by tilstand if that's too
# crowded once you see it rendered.
make_trajectory_plot <- function(annual_dat, ranked_sites, ref_dat, line_colour, title, subtitle, top_n) {
  top_fids <- ranked_sites %>%
    slice_max(order_by = abs(sj_change), n = top_n) %>%
    pull(fid)
  
  traj_dat <- annual_dat %>% filter(fid %in% top_fids)
  first_year <- min(traj_dat$year, na.rm = TRUE)
  
  end_labels <- traj_dat %>%
    group_by(fid) %>%
    filter(year == max(year)) %>%
    ungroup()
  
  ggplot(traj_dat, aes(x = year, y = AE_SJ_p50, group = fid)) +
    sj_reference_layers(ref_dat, first_year) +
    geom_line(colour = line_colour, alpha = 0.55, linewidth = 0.6) +
    ggrepel::geom_text_repel(
      data = end_labels, aes(label = fid),
      hjust = 0, direction = "y", nudge_x = 0.3,
      size = gg_text_size("eyebrow") * 0.85, colour = line_colour, family = FONT_MONO,
      segment.size = 0.25, segment.colour = line_colour, segment.alpha = 0.4,
      min.segment.length = 0, max.overlaps = Inf, seed = 42,
      show.legend = FALSE
    ) +
    scale_x_continuous(
      breaks = sort(unique(traj_dat$year)),
      expand = expansion(mult = c(0.02, 0.16))
    ) +
    coord_cartesian(clip = "off") +
    labs(title = title, subtitle = subtitle, x = "Year", y = "Site median AE-SJ score") +
    theme_report() +
    theme(plot.margin = margin(18, 90, 14, 18))
}

# Annual median structural diversity by tilstand, 2018-2025 - a real
# multi-year trend rather than a 2-point before/after comparison, which
# reads as a more familiar chart type and shows the actual shape of the
# change (steady vs sudden vs reversing), not just its net direction.
# "Structural diversity" is the plain-language name for the normalised
# entropy of the SJ-class mix: 0 = the site is dominated by one SJ Class,
# 1 = its pixels are spread evenly across all four.
make_diversity_trajectory_plot <- function(annual_composition, sites, colour_palette, title, subtitle) {
  dat <- annual_composition %>%
    inner_join(sites %>% select(fid, tilstand), by = "fid") %>%
    filter(!is.na(tilstand), is.finite(entropy)) %>%
    group_by(tilstand, year) %>%
    summarise(n_fids = n_distinct(fid), median_diversity = median(entropy, na.rm = TRUE), .groups = "drop")
  
  end_labels <- dat %>%
    group_by(tilstand) %>%
    filter(year == max(year)) %>%
    ungroup()
  
  first_year <- min(dat$year, na.rm = TRUE)
  
  ggplot(dat, aes(x = year, y = median_diversity, colour = tilstand, group = tilstand)) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 1.8) +
    geom_text(
      data = end_labels, aes(label = tilstand),
      hjust = -0.12, size = gg_text_size("row_label"), fontface = "bold", family = FONT_SANS,
      show.legend = FALSE
    ) +
    scale_colour_manual(values = colour_palette, guide = "none") +
    scale_x_continuous(
      breaks = sort(unique(dat$year)),
      expand = expansion(mult = c(0.02, 0.16))
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    coord_cartesian(clip = "off") +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Year",
      y = "Structural diversity\n(0 = one class dominates, 1 = evenly mixed)"
    ) +
    theme_report() +
    theme(plot.margin = margin(18, 70, 14, 18))
}


