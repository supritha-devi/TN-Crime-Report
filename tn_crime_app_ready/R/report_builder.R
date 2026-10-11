# =============================================================
# HTML Report Builder -- replaces rmarkdown/tinytex/pandoc
# =============================================================
# Produces the same content as before (district profile, crime summary,
# category chart as an embedded base64 PNG, comparable districts,
# interpretation, limitations) as a single self-contained HTML string.
# No LaTeX, no pandoc, no system dependencies beyond base R + ggplot2
# (already needed elsewhere in the app).
#
# Opens in any browser; the user can "Print > Save as PDF" if they
# want an actual PDF, with zero extra packages required server-side.
# =============================================================

library(ggplot2)
library(base64enc)

build_category_chart_base64 <- function(d) {
  cats <- data.frame(
    category = c("Murder", "Crimes against women", "Road accidents", "Suicides"),
    rate = c(d$murder_rate, d$women_crime_rate, d$road_accident_rate, d$suicide_rate)
  )
  tmp <- tempfile(fileext = ".png")
  p <- ggplot(cats, aes(x = reorder(category, -rate), y = rate, fill = category)) +
    geom_col(width = 0.6) +
    scale_fill_manual(values = c(
      "Murder" = "#7C3AED", "Crimes against women" = "#EC4899",
      "Road accidents" = "#EA580C", "Suicides" = "#0284C7"
    )) +
    labs(x = NULL, y = "Rate per lakh population") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
  ggsave(tmp, p, width = 5.5, height = 3, dpi = 120)
  b64 <- base64enc::base64encode(tmp)
  unlink(tmp)
  paste0("data:image/png;base64,", b64)
}

build_report_html <- function(d, state_avg, role_note, compare_df) {
  img_src <- build_category_chart_base64(d)
  today <- format(Sys.Date(), "%d %B %Y")

  borrowed_note <- if (isTRUE(d$demographics_is_borrowed)) {
    "<p style='font-size:11.5px;color:#78350F;font-style:italic;'>This district was created after the 2011 Census; demographic figures are borrowed from its pre-split parent district.</p>"
  } else ""

  compare_rows <- paste0(
    "<tr><td>", compare_df$district, "</td><td>", compare_df$total_crime_rate_2023,
    "</td><td>", compare_df$population_lakhs_2023, " lakhs</td></tr>",
    collapse = "\n"
  )

  sprintf('<!DOCTYPE html>
<html><head><meta charset="UTF-8"><title>%s District Report</title>
<style>
body { font-family: -apple-system, Arial, sans-serif; max-width: 720px; margin: 40px auto; color: #0F172A; padding: 0 20px; }
.org { font-size: 11.5px; letter-spacing: 1px; text-transform: uppercase; color: #64748B; font-weight: 700; }
h1 { margin: 4px 0 2px; font-size: 26px; border-bottom: 3px solid #4F46E5; padding-bottom: 14px; }
.meta { font-size: 12px; color: #64748B; margin-bottom: 18px; }
.section-title { font-size: 13px; text-transform: uppercase; letter-spacing: 0.5px; color: #4F46E5; font-weight: 700; margin: 24px 0 8px; border-bottom: 1px solid #E2E8F0; padding-bottom: 6px; }
table { width: 100%%; border-collapse: collapse; font-size: 13.5px; }
td, th { text-align: left; padding: 7px 10px; border-bottom: 1px solid #E2E8F0; }
td:first-child { color: #64748B; }
.note { background: #FFFBEB; border: 1px solid #FDE68A; border-radius: 10px; padding: 14px 16px; margin-top: 8px; }
.footer { margin-top: 26px; padding-top: 10px; border-top: 1px solid #E2E8F0; font-size: 11px; color: #64748B; }
img { max-width: 100%%; }
@media print { body { margin: 0; } }
</style></head>
<body>
<div class="org">Tamil Nadu Crime Insight &middot; District Safety Report</div>
<h1>%s</h1>
<div class="meta">Generated %s &nbsp;|&nbsp; Data year: 2023</div>

<div class="section-title">1. District Profile</div>
<table>
<tr><td>Area</td><td>%s sq. km</td></tr>
<tr><td>Population (2011 Census)</td><td>%s</td></tr>
<tr><td>Population density</td><td>%s per sq. km</td></tr>
<tr><td>Urban population</td><td>%s%%</td></tr>
<tr><td>Literacy rate</td><td>%s%%</td></tr>
<tr><td>SC population</td><td>%s%%</td></tr>
<tr><td>ST population</td><td>%s%%</td></tr>
</table>
%s

<div class="section-title">2. Crime Summary (2023)</div>
<table>
<tr><td>Total crime rate (per lakh)</td><td><b>%s</b></td></tr>
<tr><td>Tamil Nadu state average</td><td>%s</td></tr>
<tr><td>Rank among 38 districts</td><td>%s of 38</td></tr>
<tr><td>Population (2023 est.)</td><td>%s lakhs</td></tr>
</table>

<div class="section-title">3. Crime by Category (per lakh population)</div>
<img src="%s" alt="Category breakdown chart">

<div class="section-title">4. Comparable Districts (by population size)</div>
<table><tr><th>District</th><th>Crime rate/lakh</th><th>Population</th></tr>%s</table>

<div class="section-title">5. Interpretation</div>
<div class="note">%s</div>

<div class="section-title">6. Data Limitations</div>
<p style="font-size:12.5px;color:#64748B;">Crime figures reflect registered cases reported to Tamil Nadu Police (TN-SCRB) for 2023, at the district level only. Demographic figures are from the 2011 Census, the most recent full Census available. Neither dataset captures unreported incidents or street-level detail. This report describes statistical patterns only and does not establish causation.</p>

<div class="footer">Sources: Tamil Nadu State Crime Records Bureau (2023); Census of India (2011). Generated by TN Crime Insight.</div>
</body></html>',
    d$district, d$district, today,
    d$area_sqkm, format(d$population_2011, big.mark = ","), d$density_per_sqkm,
    d$urban_pct, d$literacy_rate_pct, d$sc_pct, d$st_pct, borrowed_note,
    d$total_crime_rate_2023, state_avg, d$rank, d$population_lakhs_2023,
    img_src, compare_rows, role_note
  )
}
