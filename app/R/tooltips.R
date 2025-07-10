# Copyright 2025 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

tooltips = list(
  Population = HTML("Reference dates: July 1, 2011, 2016 and 2021, respectively<br><br>Source: Statistics Canada, 2011, 2016, 2021 Census of Population"),

  `Total jobs` = HTML("Total number of people who said they were employed during the census reference week
                      in 2011, 2016, and 2021, respectively<br><br>Source: Statistics Canada, 2011, 2016, 2021 Census of Population"),

  `Total income` = HTML("Estimates for the 2010, 2015 and 2020 calendar years, respectively<br><br>Source: Statistics Canada, 2011, 2016, 2021 Census of Population"),

  `Average employment income` = HTML("Estimates for the 2010, 2015 and 2020 calendar years, respectively.<br><br>No adjustments were made for full-time or part-time workers<br><br>Source: Statistics Canada, 2011, 2016, 2021 Census of Population"),

  `Diversity index` = HTML("The diversity index (DI) measures whether a region has many or few external income sources.<br><br>
                            A region entirely dependent on one income source has a DI of zero.
                            A region equally dependent on all the defined income sources has a DI of 100.<br><br>
                            In general, the DI in B.C. is between 50 and 80.<br><br>Source: BC Stats"),

  summary_statistics = HTML("The reference years are listed as 2010, 2015, and 2020 because the
                             economic statistics generated from the census are based on the previous
                             calendar year of collection.<br><br>The population estimates have an
                             effective date of July 1, 2011, 2016 and 2021, respectively."),

  income_shares = HTML("Basic income shares measure how a region's basic income is divided across
                        various income sources. Basic income refers to all the external income sources
                        that an area depends on for its economic wellbeing.<br><br>
                       For a more detailed description, <a href = 'https://www2.gov.bc.ca/assets/download/C499D2E204F845ABBA47686019310E2A'>
                       see the LAEP report.</a>"),

  top_employment = HTML("Industries in this table were in the top 5 industries based
                        on number of jobs for at least one year (2010, 2015, or 2020).<br><br>
                        This is a simple ranking of employment by industry,
                        irrespective of which portions are basic or non-basic.<br><br>
                        For a more detailed description,
                        <a href = 'https://www2.gov.bc.ca/assets/download/C499D2E204F845ABBA47686019310E2A'>
                       see the LAEP report.</a>"),

  top_lq = HTML("Industries in this table were in the top 5 industries based
                 on location quotient (LQ) for at least one year (2010, 2015, or 2020).<br><br>
                 The LQ measures an industry's concentration of local jobs,
                 relative to the provincial average. A value of 2 indicates the industry's
                 concentration of jobs in the region is twice as high compared to the
                 provincial average.<br><br>
                       For a more detailed description, <a href = 'https://www2.gov.bc.ca/assets/download/C499D2E204F845ABBA47686019310E2A'>
                       see the LAEP report.</a><br><br>Note: At the provincial level, all location quotients are 1.")
  )

