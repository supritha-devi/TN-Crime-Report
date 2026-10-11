# =============================================================
# Chatbot Engine (rule-based) -- sourced by app.R
# =============================================================
# chatbot_respond() takes the raw user text, the selected role, and a
# running context list (last district / category discussed), and
# returns list(reply = "...", context = <updated context>, log = list(...)).
#
# `log` is NULL or a list(district=, category=) describing what should
# be written to the activity-log table in app.R -- kept separate from
# the reply so the Shiny server can log to SQLite without this file
# needing any database access itself.
# =============================================================

library(stringr)

district_aliases <- c(
  "chenai"="Chennai", "channai"="Chennai", "madras"="Chennai",
  "coimbatur"="Coimbatore", "kovai"="Coimbatore",
  "madurrai"="Madurai", "trichy"="Tiruchirappalli", "tirchy"="Tiruchirappalli",
  "tanjore"="Thanjavur", "tuticorin"="Thoothukkudi",
  "cape comorin"="Kanyakumari"
)

category_keywords <- list(
  list(pattern = "women|woman",            field = "women_crime_rate",      label = "crimes against women"),
  list(pattern = "murder|homicide",        field = "murder_rate",           label = "murder"),
  list(pattern = "road|accident",          field = "road_accident_rate",    label = "road accidents"),
  list(pattern = "suicide",                field = "suicide_rate",          label = "suicides"),
  list(pattern = "total|overall",          field = "total_crime_rate_2023", label = "overall crime")
)

find_district_in_text <- function(text, all_districts) {
  t <- str_to_lower(text)
  for (name in all_districts) {
    if (str_detect(t, fixed(str_to_lower(name)))) return(name)
  }
  for (alias in names(district_aliases)) {
    if (str_detect(t, fixed(alias))) return(district_aliases[[alias]])
  }
  NA_character_
}

find_category_in_text <- function(text) {
  t <- str_to_lower(text)
  for (c in category_keywords) {
    if (str_detect(t, c$pattern)) return(list(field = c$field, label = c$label))
  }
  NULL
}

highest_category_label <- function(row) {
  rates <- c("Murder" = row$murder_rate, "Crimes against women" = row$women_crime_rate,
             "Road accidents" = row$road_accident_rate, "Suicides" = row$suicide_rate)
  names(rates)[which.max(rates)]
}

fmt_district_fact <- function(name, field, label, role, master_df, state_avg) {
  row <- master_df[master_df$district == name, ]
  val <- row[[field]]
  avg <- mean(master_df[[field]], na.rm = TRUE)
  diff_pct <- round((val - avg) / avg * 100, 1)
  dir <- if (diff_pct >= 0) "above" else "below"

  reply <- switch(role,
    "Student/Researcher" = sprintf(
      "%s -- %s rate: %s per lakh (2023). State average: %.2f. Difference: %s%% %s. Rank (overall rate): %s of 38. Note: only 2023 figures are available -- no multi-year series in this demo.",
      name, label, val, avg, diff_pct, dir, row$rank
    ),
    "Journalist" = sprintf(
      "%s reported a %s rate of %s per lakh in 2023 (state average %.1f). Source: Tamil Nadu State Crime Records Bureau, 2023.",
      name, label, val, avg
    ),
    "NGO" = sprintf(
      "For %s, the %s rate was %s per lakh in 2023%s You can compare this with other districts from the Explore Map or District Report pages.",
      name, label, val, if (field == "women_crime_rate") " -- relevant for vulnerable-group advocacy work." else "."
    ),
    "Government Official" = sprintf(
      "%s: %s rate %s/lakh, %s%% %s the state average. Rank %s of 38 by overall rate. See District Report for the full breakdown.",
      name, label, val, abs(diff_pct), dir, row$rank
    ),
    # Common Citizen / default
    {
      base <- sprintf("%s had a reported %s rate of about %s per lakh people in 2023, which is %s the state average.",
                       name, label, val, dir)
      if (field == "total_crime_rate_2023" && diff_pct > 10) {
        base <- paste(base, "Since that's meaningfully above average: if you live or work there, simple steps like secure door/window locks, visible CCTV, and good outdoor lighting are reasonable precautions -- and if you're considering buying property there, it's worth weighing alongside other factors, not a reason on its own to avoid the area.")
      }
      base
    }
  )
  reply
}

safety_advice_for <- function(name, master_df, state_avg) {
  row <- master_df[master_df$district == name, ]
  diff_pct <- (row$total_crime_rate_2023 - state_avg) / state_avg * 100
  top_cat <- highest_category_label(row)
  advice <- sprintf("%s's overall rate is %.1f%% %s the state average, with %s as the largest category. ",
                     name, abs(diff_pct), if (diff_pct >= 0) "above" else "below", str_to_lower(top_cat))
  if (diff_pct > 15) {
    advice <- paste0(advice, "General precautions worth considering there: avoid isolated areas alone at night, let someone know your route if traveling after dark, and prefer well-lit, busier roads over shortcuts. ")
  } else {
    advice <- paste0(advice, "This isn't a high-outlier district, but the same basic precautions (well-lit routes, avoiding isolated areas late at night) are reasonable anywhere. ")
  }
  paste0(advice, "If you already own property there: install visible security cameras, keep doors and windows locked, and consider a community-watch arrangement. These are general suggestions from the statistical pattern, not a specific threat assessment for any address.")
}

# ---- Main entry point --------------------------------------------------
chatbot_respond <- function(raw_text, role, context, master_df, all_districts, state_avg) {
  text <- str_to_lower(str_trim(raw_text))
  log_entry <- NULL

  resp <- function(reply, new_context = context, log = NULL) {
    list(reply = reply, context = new_context, log = log)
  }

  # ---- Small talk ----
  if (str_detect(text, "^(hi+|hello+|hey+|good morning|good afternoon|good evening|anyone there|are you there)\\b"))
    return(resp("Hello! You can ask about a district (e.g. \"crime in Madurai\"), ask \"which district is highest/lowest\", \"safest district to visit\", or say \"compare X and Y\"."))

  if (str_detect(text, "^(ok|okay|alright|fine|cool|great|got it|sure|nice|k|kk)\\b"))
    return(resp("Glad that helps! Anything else you'd like to check -- another district, a comparison, or safety tips?"))

  if (str_detect(text, "how are you"))
    return(resp("Doing well, thanks for asking! Ready to help with Tamil Nadu crime data -- what would you like to know?"))

  if (str_detect(text, "are you (an? )?(ai|bot|human|real|robot)"))
    return(resp("Yes, I'm a rule-based chatbot built for this app -- not a person. I match your question to district/category/intent patterns rather than truly understanding language, so I do best with fairly direct questions."))

  if (str_detect(text, "what.?s your role|your role in society"))
    return(resp(sprintf("I don't have a role in society the way you do! I'm just this app's assistant. Your role (%s) is what shapes how I phrase my answers.", role)))

  if (str_detect(text, "what (work|job) do you do|what do you do\\b"))
    return(resp("I help you explore Tamil Nadu's 2023 district crime data -- lookups, comparisons, highest/lowest rankings, and safety-related context. That's the whole job!"))

  if (str_detect(text, "explore|places? to (visit|see|go)|interesting places|tourist")) {
    low <- master_df$district[which.min(master_df$total_crime_rate_2023)]
    low_val <- master_df$total_crime_rate_2023[which.min(master_df$total_crime_rate_2023)]
    return(resp(sprintf("I can't recommend attractions, but on crime-rate terms alone, %s has the lowest reported rate (%s/lakh) in this dataset. For actual travel recommendations, a travel guide would serve you far better than crime statistics!", low, low_val)))
  }

  if (str_detect(text, "^(bye|goodbye|bye bye|see you|thanks|thank you|thanks a lot|that.?s all|done|good night|ok thanks)\\b"))
    return(resp("Glad to help! Come back anytime."))

  if (str_detect(text, "help|what can you do"))
    return(resp("I can: look up a district's crime rate, tell you the highest/lowest district (overall or by category), compare two districts, give general safety context, and explain terms like \"crime rate\". Try \"crime in Coimbatore\" or \"compare Chennai and Salem\"."))

  # ---- Firm refusal: individual case / named-perpetrator questions ----
  if (str_detect(text, "serial kill|killer.?s name|murderer.?s name|who (killed|murdered)|name of the (killer|murderer|accused|criminal)"))
    return(resp("I can't help with that. This app only has aggregate district-level statistics (counts and rates) for 2023 -- it has no individual case records, and I won't guess or invent names tied to real places or people. For specific criminal cases, official police records or court reporting would be the appropriate source."))

  # ---- Safety / route / buying-land guidance ----
  if (str_detect(text, "(is|whats?) .* safe|safe to (travel|go|visit)|should i (avoid|go)|avoid (that|this|the) (place|area|route)|don.?t (go|travel)")) {
    d1 <- find_district_in_text(text, all_districts)
    if (!is.na(d1)) return(resp(safety_advice_for(d1, master_df, state_avg), modifyList(context, list(district = d1)), list(district = d1, category = "safety advice")))
    return(resp("Reported crime statistics can show patterns, but they can't by themselves fully answer whether a place or route is \"safe\" -- that depends on many factors this dataset doesn't capture. Tell me a district and I'll share what the data actually shows."))
  }

  if (str_detect(text, "buy (land|property|house|a house|a plot)|should i buy")) {
    d1 <- find_district_in_text(text, all_districts)
    if (!is.na(d1)) {
      row <- master_df[master_df$district == d1, ]
      diff_pct <- (row$total_crime_rate_2023 - state_avg) / state_avg * 100
      reply <- sprintf("%s's crime rate is %.1f%% %s the state average. That's one factor worth weighing among many (price, amenities, connectivity) -- not a reason by itself to rule a place in or out.",
                        d1, abs(diff_pct), if (diff_pct >= 0) "above" else "below")
      return(resp(reply, modifyList(context, list(district = d1)), list(district = d1, category = "property advice")))
    }
    return(resp("Tell me which district you're considering and I'll share its crime rate relative to the state average -- one factor among many for a decision like that."))
  }

  if (str_detect(text, "how (can i|to) be safe|safety (tips|suggestions)|keep (myself|my home|my house) safe"))
    return(resp("General suggestions regardless of district: keep doors and windows locked, install visible CCTV if you own property, avoid isolated areas alone late at night, let someone know your route when traveling after dark, and prefer well-lit main roads over shortcuts. Want district-specific context? Just name one."))

  if (str_detect(text, "^why"))
    return(resp("The dataset shows the numerical change in reported cases, but it doesn't establish *why* it changed -- that would need separate investigation. Want to see the category breakdown instead?"))

  # ---- Year-awareness ----
  year_note <- ""
  year_match <- str_extract(text, "\\b(2024|2025|2026)\\b")
  if (!is.na(year_match)) {
    year_note <- sprintf("Note: district-level data for %s hasn't been published yet -- only state-wide totals exist for 2024 (see the Dashboard's data currency note). Showing the latest available district figures (2023) instead: ", year_match)
  }

  cat_match <- find_category_in_text(text)
  if (!is.null(cat_match)) context <- modifyList(context, list(category = cat_match$field, category_label = cat_match$label))

  # ---- Compare two districts ----
  compare_hits <- all_districts[sapply(all_districts, function(n) str_detect(text, fixed(str_to_lower(n))))]
  if (length(compare_hits) >= 2 && str_detect(text, "compare|vs\\.?|versus")) {
    a <- compare_hits[1]; b <- compare_hits[2]
    field <- context$category; label <- context$category_label
    va <- master_df[[field]][master_df$district == a]
    vb <- master_df[[field]][master_df$district == b]
    winner <- if (va > vb) a else b
    reply <- sprintf("%s: %s/lakh vs %s: %s/lakh (%s, 2023). %s is higher. Numbers only -- no judgment on which is \"better\".", a, va, b, vb, label, winner)
    return(resp(reply, context, list(district = paste(a, "vs", b), category = label)))
  }

  # ---- Top N / Bottom N ----
  n_match <- str_match(text, "top\\s*(\\d+)|bottom\\s*(\\d+)")
  if (!is.na(n_match[1,1])) {
    n <- as.integer(ifelse(!is.na(n_match[1,2]), n_match[1,2], n_match[1,3]))
    field <- context$category; label <- context$category_label
    ordered <- master_df[order(-master_df[[field]]), c("district", field)]
    picked <- if (!is.na(n_match[1,2])) head(ordered, n) else tail(ordered, n)[rev(seq_len(n)), ]
    lines <- paste0(seq_len(nrow(picked)), ". ", picked$district, " (", picked[[field]], ")")
    reply <- paste0(ifelse(!is.na(n_match[1,2]), "Top ", "Bottom "), n, " districts by ", label, " (2023): ", paste(lines, collapse = ", "))
    return(resp(reply, context))
  }

  # ---- Mean / median ----
  if (str_detect(text, "\\b(mean|average)\\b")) {
    field <- context$category; label <- context$category_label
    return(resp(sprintf("The mean %s rate across all 38 districts (2023) is %.2f per lakh.", label, mean(master_df[[field]]))))
  }
  if (str_detect(text, "\\bmedian\\b")) {
    field <- context$category; label <- context$category_label
    return(resp(sprintf("The median %s rate across all 38 districts (2023) is %.2f per lakh.", label, median(master_df[[field]]))))
  }
  if (str_detect(text, "percentage change|percent change|%\\s*change"))
    return(resp("Percentage change is (current - previous) / previous x 100. This demo only has a single year (2023) of district-level data, so I can't compute an actual year-over-year change at district level -- see the Dashboard's data currency note for the real state-wide 2024 comparison that IS available."))

  # ---- Dataset / source / availability ----
  if (str_detect(text, "what (data|years|districts|categories) (is|are) available|what variables"))
    return(resp("Available: all 38 Tamil Nadu districts, five categories (total/IPC+SLL, murder, crimes against women, road accidents, suicides), for 2023 only. No multi-year district series in this demo."))
  if (str_detect(text, "where does this data come from|data source|source of this|where.?s this from"))
    return(resp("Crime figures: Tamil Nadu State Crime Records Bureau (TN-SCRB), 2023. Demographic figures: Census of India, 2011 -- the most recent full Census available."))
  if (str_detect(text, "is missing data (the same as )?zero|missing.*zero"))
    return(resp("No -- a reported value of 0 means zero registered cases, while missing data means no figure was available at all. All 38 districts have complete 2023 figures here."))

  # ---- Explain chart/map/rate/hotspot ----
  if (str_detect(text, "explain (this|the) (chart|graph)"))
    return(resp("The chart shows this district's rate across four categories (murder, crimes against women, road accidents, suicides) in cases per lakh population for 2023."))
  if (str_detect(text, "explain (this|the) map|what does (this|the) map show"))
    return(resp("The map colors each district by the selected crime category's rate, split into four shades (white = lowest quarter, dark red = highest quarter) -- relative to other TN districts, not an absolute scale."))
  if (str_detect(text, "what (is|does) crime rate|what does rate mean|what is reported crime"))
    return(resp("Crime rate here means registered cases per lakh (100,000) population -- it lets you compare districts fairly despite different population sizes."))
  if (str_detect(text, "what is a? ?hotspot"))
    return(resp("A hotspot usually means an area where values cluster higher than their surroundings. Formal hotspot detection (like Moran's I) isn't implemented in this chatbot, but the clustering script in this project does something related."))

  # ---- Opinion avoidance ----
  if (str_detect(text, "which district is (the )?best|best district|which district should i live"))
    return(resp("I provide crime statistics rather than overall judgments about districts -- \"best\" depends on many things beyond crime data. I can compare districts on the available indicators if that helps."))

  # ---- Navigation / account help ----
  if (str_detect(text, "how do i (register|sign up|create an account)")) return(resp("Tap \"New user? Create an account\", fill in your details, pick a role and avatar, and you're set."))
  if (str_detect(text, "how do i (login|log in)")) return(resp("Enter your username and password on the login page and tap Login."))
  if (str_detect(text, "how do i (logout|log out)")) return(resp("Tap the Logout button in the top bar."))
  if (str_detect(text, "how do i save")) return(resp("Open a District Report and tap \"Save to history\" -- it'll appear in the My History tab."))
  if (str_detect(text, "where (is|can i find) (the )?(crime map|map)")) return(resp("The Explore Map tab has the full state map; tap a district to open its District Map page."))
  if (str_detect(text, "how do i download")) return(resp("On the District Report page: \"Download Full Report (PDF)\" or \"Download Data (CSV/JSON)\"."))

  # ---- Feedback ----
  if (str_detect(text, "this (was|is) helpful|that helped")) return(resp("Glad it helped!"))
  if (str_detect(text, "this is wrong|incorrect data")) return(resp("Please check the source and year shown (TN-SCRB 2023) -- if something still looks off, it's worth double-checking against the original TN-SCRB publication."))
  if (str_detect(text, "give feedback|i have feedback")) return(resp("Happy to note that -- describe what you noticed and it'll be treated as logged feedback in the full app."))

  # ---- District lookup (fallback-priority) ----
  district <- find_district_in_text(text, all_districts)
  if (!is.na(district)) {
    new_ctx <- modifyList(context, list(district = district))
    reply <- paste0(year_note, fmt_district_fact(district, context$category, context$category_label, role, master_df, state_avg))
    return(resp(reply, new_ctx, list(district = district, category = context$category_label)))
  }
  if (!is.null(context$district) && !is.na(context$district)) {
    reply <- paste0(year_note, fmt_district_fact(context$district, context$category, context$category_label, role, master_df, state_avg))
    return(resp(reply, context, list(district = context$district, category = context$category_label)))
  }

  if (str_detect(text, "highest|top district|most crime")) {
    field <- context$category; label <- context$category_label
    top <- master_df$district[which.max(master_df[[field]])]
    val <- master_df[[field]][which.max(master_df[[field]])]
    return(resp(sprintf("%s has the highest %s rate: %s per lakh (2023).", top, label, val), context, list(district = top, category = label)))
  }
  if (str_detect(text, "lowest|safest|least crime|bottom district")) {
    field <- context$category; label <- context$category_label
    low <- master_df$district[which.min(master_df[[field]])]
    val <- master_df[[field]][which.min(master_df[[field]])]
    return(resp(sprintf("%s has the lowest %s rate: %s per lakh (2023).", low, label, val), context, list(district = low, category = label)))
  }

  resp("I didn't catch a district or question I recognize. Try: \"crime in Chennai\", \"highest district for women's safety\", \"is it safe to visit X\", or \"compare Salem and Erode\". (This is a rule-based demo covering the core question types -- not every phrasing yet.)")
}
