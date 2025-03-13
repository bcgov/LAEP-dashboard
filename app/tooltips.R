tooltips = list(
  shift_share = markdown("
  If a particular industry is growing or declining in a local area, an automatic question is whether it is \"natural\" in the sense that it is also growing or declining in other areas. Shift/share analysis attempts to answer this question by breaking down the change in employment by industry group into three components:

    - The Provincial Effect (PE), or whether employment is increasing in the province overall.
    - The Industry Effect (IE), or whether the sector is expanding or declining in the province generally, at a rate faster than overall job growth.
    - The Local Effect (LE), or the growth or decline that can only be attributed to the specifics conditions of the sector in the local area.

  The Local Effect is most interesting for analysis purposes. It implies that the local area is somehow over- or under-performing with respect to employment in that sector.
  "),

  POPULATION = "Population estimates are from the census and have an effective date of July 1, 2011, 2016 and 2021, respectively. The LAEP model uses reference dates of 2010, 2015 and 2020 because most of the economic statistics generated rely on employment income estimates from the census, which have an effective date of the previous calendar year. That is to say, strictly speaking the dates say 2010, 2015 and 2020 but in the case of population and jobs, they are for 2011, 2016 and 2021.",

  TOTAL_JOBS = "Job estimates are people who said they were employed during the census reference week in May 2011, 2016 or 2021.",

  TOTAL_INCOME = "Income estimates are for the calendar year listed.",
  AVERAGE_EMPLOYMENT_INCOME = "For people employed in the census year (i.e. May of 2011, 2016 or 2021), this shows their incomes in the previous calendar year. No adjustment is made for full-time or part-time workers.",
  DIVERSITY_INDEX = NULL,
  FOREST_SECTOR_VULNERABILITY_INDEX = NULL,

  top_employment = "This is a simple ranking of employment by industry, irrespective of which portions are basic or non-basic.

  Tourism is an exception, where its employment estimate is a portion of many industries, as defined by each industry's tourism demand. Other industries do not have that portion subtracted here, so that one would be double-counting jobs if you added tourism to the other sectors. Most tourism demand falls under the \"other services\" category.",

  geography_level = "Insert definitions of RDs and Local Areas here",

  RD = "Insert definitions of RDs here",

  LA = "Insert definitions of Local Areas here"


)

