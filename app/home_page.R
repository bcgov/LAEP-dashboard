fluidPage(
  HTML("
<h1>Local Area Economic Profiles</h1>
  <h2>Background</h2>
  <p>The Local Area Economic Profile Report is a new edition of an economic model
that was previous called the Local Area Economic Dependencies and was last published by BC Stats in 2009.</p>
  <p>This project creates a consistent economic model for 103 local areas in British Columbia.
It combines detailed census data with the province's macroeconomic model and is primarily intended
        to provide information on rural areas and to help estimate the impact of economic changes.</p>
  "),
  shinydashboard::tabBox(width = 12,
    shiny::tabPanel("Framework of the Model",
      HTML("<p>The fundamental premise of this work is that the economy of a community can be represented
      by income flows that can be classified as basic or non-basic. Basic income flows into the community
      from the outside world in the form of employment income or non-employment income,
      while non-basic income is generated from jobs that provide goods and services to the people who live there.</p>
        <p>There are 12 sources of basic income used in the LAED model: 10 industrial sectors and two types of non-employment income.</p>
        <ol>
        <li>Jobs in Forestry and associated manufacturing, further disaggregated into two groups: logging and wood manufacturing;</li>
        <li>Jobs in Mining, Oil and Gas and associated manufacturing;</li>
        <li>Jobs in Fishing and Trapping and associated manufacturing;</li>
        <li>Jobs in Agriculture, disaggregated into farming and food and beverage manufacturing;</li>
        <li>Jobs in Tourism, disaggregated into transportation, accommodation, food and beverage, recreation and entertainment, retail and other;</li>
        <li>Jobs in High Tech Manufacturing;</li>
        <li>Jobs in the Public Sector, disaggregated by level of government;</li>
        <li>Jobs in Construction, disaggregated by type of construction;</li>
        <li>Jobs in Film Production and Sound Recording;</li>
        <li>Jobs in Other industries, which includes portions of non-basic sectors which have identified exports and other sources of outside demand;</li>
        <li>Government transfer payments such as Income Assistance, Canada Pension Plan, Child Tax benefits, and more;</li>
        <li>Non-employment market income such as investment income, dividends and interest, and private pensions.</li>
        </ol>
        <p>
        <strong>Non-basic income</strong>
        is generated from jobs in the community that provide goods and services to individuals who live there.
      Examples of these include much of retail trade, local transportation services, financial services and personal services.
      </p>
        <p>Definitions of basic and non-basic sectors, according to the North American Industry Classification System (NAICS),
      can be found in the sheets Industry Definitions and Sector Aggregations.</p>
        <h6>Direct, Indirect and Induced Effect</h6>
        <p>
        <em>The direct effect</em>
        represents the basic industry sector itself, whether expressed as economic output,
      jobs, or the associated employment income. For example, industries that are directly in the High Tech Manufacturing sector
      are the ones shown in the concordance tables in the sheets \"Industry Definitions\" and \"Sector Aggregations\".
      </p>
        <p>
        <em>The indirect effect</em>
        estimates how much output is needed from other non-basic industries which act as intermediate suppliers
      to the direct basic industries. For example, the basic sector Mining, Oil and Gas uses a large amount of intermediate inputs
      from the Utilities sector. The indirect effect is calculated according to data in the province's input-output tables.
                      </p>
                      <p>
                        <em>The imputed effect</em>
                        estimates the impact of household spending in the local economy. It is calculated according to the
            household final consumption estimates in the B.C. supply-use and input-output tables.
            For each area, total after-tax income is translated into output and jobs in non-basic sectors such as retail,
            low tech business services, etc.
                      </p>
                      <h6>Geographical Impacts</h6>
                      <p>If the census data indicates that a local area doesn’t have large enough industries to meet the demand
            that is implied by the indirect and induced effects, the model reallocates the excess to other areas in B.C. with enough capacity.
            This gives rise to variations in the descriptive statistics which include or exclude the impact that
            the rest of the province has on the local area and vice versa.</p>
      ")
    ),
    shiny::tabPanel("Data Sources",
      HTML("
        <h6>Canadian Census</h6>
                      <p>The census is the most detailed picture available of economic conditions down to the very local level in British Columbia.
            The LAED model uses custom census tabulations based on the long-form census which show, for each of the 103 local areas
            and 29 census divisions (also called regional districts), estimates of employment and employment income for 303 industries
            at the 4-digit NAICS level, as shown in the first column of the sheet \"Industry Definitions\".
            Additional census data is used for measures of total income, total employment income, total government transfers,
            total market non-employment income, and total after-tax income.</p>
                      <p>This model uses data from the 2016 and 2021 censuses. The reference years are 2015 and 2020, respectively,
            because those are deemed the most appropriate due to the way the census asks people to respond.
            For employment income, respondents are asked what was their employment income in the previous year;
            for employment by industry, respondents are asked what job they held during a certain week in May of 2016 or 2021,
            and if a person did not work during that week but had worked at some time since January of the previous year,
            the information relates to the job they held the longest during that period.</p>
                      <p>In most years, employment data is not expected to change significantly in one year.
            But because of the extraordinary impacts of the COVID-19 pandemic that assumption may not hold for the 2021 census
            and 2020 reference year. Users should interpret those results with caution and in particular, the 2020 tourism results.</p>
                      <p>The 103 local areas were assembled in consultation with government staff who have knowledge of those areas
            and with the intention of selecting meaningful economic units. They are aggregations of one or more census subdivisions (CSDs),
            as shown in the sheet \"EDA boundaries\".</p>
                      <h6>Input-Output Tables (IOTs)</h6>
                      <p>Input-output tables describe the sale and purchase relationships between producers and consumers within an economy.
            They show flows of final and intermediate goods and services defined according to industry.
            They are used to estimate the indirect effect (for a given amount of output in a direct basic industry,
            which non-basic industries are impacted and how much) and induced effect.
            Statistics Canada produces IOTs for each province annually.</p>
      ")
    ),
    shiny::tabPanel("Sector Aggregations",
      HTML("
      <h6>Basic Industry Sectors</h6>
                      <ul>
                        <li>Forestry</li>
                        <ul>
                          <li>Logging</li>
                          <ul>
                            <li>1131 Timber tract operations</li>
                            <li>1132 Forest nurseries and gathering of forest products</li>
                            <li>1133 Logging</li>
                            <li>1153 Support activities for forestry</li>
                          </ul>
                          <li>Wood Manufacturing</li>
                          <ul>
                            <li>3211 Sawmills and wood preservation</li>
                            <li>3212 Veneer, plywood and engineered wood product manufacturing</li>
                            <li>3219 Other wood product manufacturing</li>
                            <li>3221 Pulp, paper and paperboard mills</li>
                            <li>3222 Converted paper product manufacturing</li>
                            <li>3371 Household and institutional furniture and kitchen cabinet manufacturing</li>
                            <li>3372 Office furniture (including fixtures) manufacturing</li>
                            <li>3379 Other furniture-related product manufacturing</li>
                          </ul>
                        </ul>
                        <li>Mining, Oil and Gas</li>
                        <ul>
                          <li>2111 Oil and gas extraction</li>
                          <li>2121 Coal mining</li>
                          <li>2122 Metal ore mining</li>
                          <li>2123 Non-metallic mineral mining and quarrying</li>
                          <li>2131 Support activities for mining and oil and gas extraction</li>
                          <li>3241 Petroleum and coal product manufacturing</li>
                          <li>3311 Iron and steel mills and ferro-alloy manufacturing</li>
                          <li>3312 Steel product manufacturing from purchased steel</li>
                          <li>3313 Alumina and aluminum production and processing</li>
                          <li>3314 Non-ferrous metal (except aluminum) production and processing</li>
                          <li>3315 Foundries</li>
                        </ul>
                        <li>Fishing, Hunting and Trapping</li>
                        <ul>
                          <li>1141 Fishing</li>
                          <li>1142 Hunting and trapping</li>
                          <li>3117 Seafood product preparation and packaging</li>
                        </ul>
                        <li>Agriculture and Food</li>
                        <ul>
                          <li>Farming, Greenhouses and Aquaculture</li>
                          <ul>
                            <li>1110 Farms (except greenhouses, nurseries, floriculture and aquaculture)</li>
                            <li>1114 Greenhouse, nursery and floriculture production</li>
                            <li>1125 Aquaculture</li>
                            <li>1150 Support activities for farms (1151 to 1152)</li>
                          </ul>
                          <li>Food Processing</li>
                          <ul>
                            <li>3111 Animal food manufacturing</li>
                            <li>3112 Grain and oilseed milling</li>
                            <li>3113 Sugar and confectionery product manufacturing</li>
                            <li>3114 Fruit and vegetable preserving and specialty food manufacturing</li>
                            <li>3115 Dairy product manufacturing</li>
                            <li>3116 Meat product manufacturing</li>
                            <li>3118 Bakeries and tortilla manufacturing</li>
                            <li>3119 Other food manufacturing</li>
                            <li>3121 Beverage manufacturing</li>
                            <li>3122 Tobacco manufacturing</li>
                            <li>3123 Cannabis product manufacturing</li>
                          </ul>
                        </ul>
                        <li>Tourism</li>
                        <p>Note: Tourism estimates include portions of many industries, some large but most very small. The LAED's tourism model is based off Statistics Canada's Tourism Satellite Accounts. It estimates the shares of products which are bought by tourists and divides them between the industries producing those products. Industries here are grouped into sub-categories of tourism based on the primary tourism product they produce.</p>
                        <ul>
                          <li>Transportation (by order of total output)</li>
                          <ul>
                            <li>3241 Petroleum and coal product manufacturing, 4413 Automotive parts, accessories and tire stores, 4471 Gasoline stations, 4811 Scheduled air transportation, 4812 Non-scheduled air transportation, 4821 Rail transportation, 4831 Deep-sea, coastal and great lakes water transportation, 4832 Inland water transportation, 4851 Urban transit systems, 4852 Interurban and rural bus transportation, 4853 Taxi and limousine service, 4855 Charter bus industry, 4859 Other transit and ground passenger transportation, 4871 Scenic and sightseeing transportation, land, 4872 Scenic and sightseeing transportation, water, 4879 Scenic and sightseeing transportation, other, 5321 Automotive equipment rental and leasing, 8111 Automotive repair and maintenance, 8121 Personal care services, 8129 Other personal services</li>
                          </ul>
                          <li>Accommodation</li>
                          <ul>
                            <li>7211 Traveller accommodation</li>
                            <li>7212 Recreational vehicle (RV) parks and recreational camps</li>
                          </ul>
                          <li>Food and beverage</li>
                          <ul>
                            <li>7224 Drinking places (alcoholic beverages)</li>
                            <li>7225 Full-service restaurants and limited service eating places</li>
                          </ul>
                          <li>Recreation and entertainment</li>
                          <ul>
                            <li>5121 Motion picture and video industries</li>
                            <li>7111 Performing arts companies</li>
                            <li>7112 Spectator sports</li>
                            <li>7113 Promoters (presenters) of performing arts, sports and similar events</li>
                            <li>7114 Agents and managers for artists, athletes, entertainers and other public figures</li>
                            <li>7115 Independent artists, writers and performers</li>
                            <li>7121 Heritage institutions</li>
                            <li>7131 Amusement parks and arcades</li>
                            <li>7132 Gambling industries</li>
                            <li>7139 Other amusement and recreation industries</li>
                          </ul>
                          <li>Retail</li>
                          <p>Tourism retail includes small portions of many industries. By order of total tourism output, the first 50 out of 104 industries in the list are:</p>
                          <ul>
                            <li>5311 Lessors of real estate, 4481 Clothing stores, 4451 Grocery stores, 3121 Beverage manufacturing, 3116 Meat product manufacturing, 6220 Hospitals(6221 to 6223), 1110 Farms (except Greenhouses and Aquaculture)(1111 to 1124 and 1129), 3115 Dairy product manufacturing, 3119 Other food manufacturing, 4529 Other general merchandise stores, 4483 Jewellery, luggage and leather goods stores, 3118 Bakeries and tortilla manufacturing, 3117 Seafood product preparation and packaging, 4521 Department stores, 4511 Sporting goods, hobby and musical instrument stores, 1125 Aquaculture, 4461 Health and personal care stores, 4539 Other miscellaneous store retailers, 1114 Greenhouse, Nursery and Floriculture Production, 3114 Fruit and vegetable preserving and specialty food manufacturing, 4482 Shoe stores, 4453 Beer, wine and liquor stores, 1141 Fishing, 4452 Specialty food stores, 4532 Office supplies, stationery and gift stores, 3399 Other miscellaneous manufacturing, 3152 Cut and sew clothing manufacturing, 3113 Sugar and confectionery product manufacturing, 4145 Pharmaceuticals, toiletries, cosmetics and sundries merchant wholesalers, 4533 Used merchandise stores, 5324 Commercial and industrial machinery and equipment rental and leasing, 2211 Electric power generation, transmission and distribution, 4441 Building material and supplies dealers, 4144 Personal goods merchant wholesalers, 8123 Dry cleaning and laundry services, 3256 Soap, cleaning compound and toilet preparation manufacturing, 2212 Natural gas distribution, 4541 Electronic shopping and mail-order houses, 5171 Wired telecommunications carriers, 4141 Textile, clothing and footwear merchant wholesalers, 3231 Printing and related support activities, 4543 Direct selling establishments, 4143 Home furnishings merchant wholesalers, 4431 Electronics and appliance stores, 4531 Florists, 2111 Oil and gas extraction, 4142 Home entertainment equipment and household appliance merchant wholesalers, 4513 Book stores and news dealers, 5322 Consumer goods rental, 4411 Automobile dealers</li>
                          </ul>
                          <li>Other</li>
                          <ul>
                            <li>5615 Travel arrangement and reservation services</li>
                          </ul>
                        </ul>
                        <li>High Tech Manufacturing</li>
                        <ul>
                          <li>3254 Pharmaceutical and medicine manufacturing</li>
                          <li>3259 Other chemical product manufacturing</li>
                          <li>3333 Commercial and service industry machinery manufacturing</li>
                          <li>3341 Computer and peripheral equipment manufacturing</li>
                          <li>3342 Communications equipment manufacturing</li>
                          <li>3343 Audio and video equipment manufacturing</li>
                          <li>3344 Semiconductor and other electronic component manufacturing</li>
                          <li>3345 Navigational, measuring, medical and control instruments manufacturing</li>
                          <li>3346 Manufacturing and reproducing magnetic and optical media</li>
                          <li>3359 Other electrical equipment and component manufacturing</li>
                          <li>3364 Aerospace product and parts manufacturing</li>
                          <li>3391 Medical equipment and supplies manufacturing</li>
                        </ul>
                        <li>Public Sector</li>
                        <ul>
                          <li>Federal Government</li>
                          <ul>
                            <li>9111 Defence services</li>
                            <li>9112-9119 Other federal government public administration</li>
                            <li>9191 International and other extra-territorial public administration</li>
                          </ul>
                          <li>Provincial Government</li>
                          <ul>
                            <li>9120 Provincial and territorial public administration (9121 to 9129)	</li>
                          </ul>
                          <li>Health</li>
                          <ul>
                            <li>6211 Offices of physicians</li>
                            <li>6212 Offices of dentists</li>
                            <li>6213 Offices of other health practitioners</li>
                            <li>6214 Out-patient care centres</li>
                            <li>6215 Medical and diagnostic laboratories</li>
                            <li>6216 Home health care services</li>
                            <li>6219 Other ambulatory health care services</li>
                            <li>6220 Hospitals(6221 to 6223)</li>
                            <li>6230 Nursing and residential care facilities(6231 to 6239)</li>
                          </ul>
                          <li>Education</li>
                          <ul>
                            <li>6111 Elementary and secondary schools</li>
                            <li>6112 Community colleges and C.E.G.E.P.s</li>
                            <li>6113 Universities</li>
                            <li>6114 Business schools and computer and management training</li>
                            <li>6115 Technical and trade schools</li>
                            <li>6116 Other schools and instruction</li>
                            <li>6117 Educational support services</li>
                          </ul>
                          <li>Local Government</li>
                          <ul>
                            <li>9130 Local, municipal and regional public administration (9131 to 9139)</li>
                          </ul>
                          <li>Aboriginal Government</li>
                          <ul>
                            <li>9141 Aboriginal public administration</li>
                          </ul>
                        </ul>
                        <li>Construction</li>
                        <p>Note: Construction employment is treated differently in the NAICS and Input-Output Industry Classification (IOIC) systems. A special module in the LAED model was created to translate the census data, which use NAICS, to the input-output tables, which are organized based on the final purpose of the construction activity. The final results in the LAED model are presented in a format similar to the final demand categories of the supply-use tables.</p>
                        <ul>
                          <li>NAICS</li>
                          <ul>
                            <li>23 Construction</li>
                            <ul>
                              <li>236 Construction of buildings</li>
                              <ul>
                                <li>2361 Residential building construction</li>
                                <li>2362 Non-residential building construction</li>
                              </ul>
                            </ul>
                            <ul>
                              <li>237 Heavy and civil engineering construction</li>
                              <ul>
                                <li>2371 Utility system construction</li>
                                <li>2372 Land subdivision</li>
                                <li>2373 Highway, street and bridge construction</li>
                                <li>2379 Other heavy and civil engineering construction</li>
                              </ul>
                            </ul>
                            <ul>
                              <li>238 Specialty trade contractors</li>
                              <ul>
                                <li>2381 Foundation, structure, and building exterior contractors</li>
                                <li>2382 Building equipment contractors</li>
                                <li>2383 Building finishing contractors</li>
                                <li>2389 Other specialty trade contractors</li>
                              </ul>
                            </ul>
                          </ul>
                          <li>LAED disaggregation:</li>
                          <ul>
                            <li>Residential Structures</li>
                            <li>Construction for Business Sector</li>
                            <li>Construction for Government Sector</li>
                            <li>Construction for non-profit sector</li>
                            <li>Other Basic</li>
                          </ul>
                        </ul>
                        <li>Film and TV</li>
                        <ul>
                          <li>5121 Motion picture and video industries</li>
                          <li>5122 Sound recording industries</li>
                        </ul>
                      </ul>
                      <h6>Non-Basic Sectors</h6>
                      <ul>
                        <li>Communications</li>
                        <ul>
                          <li>5111 Newspaper, periodical, book and directory publishers</li>
                        </ul>
                        <li>Finance, Insurance and Real Estate</li>
                        <ul>
                          <li>5211 Monetary authorities central bank, 5221 Depository credit intermediation, 5222 Non-depository credit intermediation, 5223 Activities related to credit intermediation, 5231 Securities and commodity contracts intermediation and brokerage, 5232 Securities and commodity exchanges, 5239 Other financial investment activities, 5241 Insurance carriers, 5242 Agencies, brokerages and other insurance related activities, 5261 Pension funds, 5269 Other funds and financial vehicles, 5311 Lessors of real estate, 5312 Offices of real estate agents and brokers, 5313 Activities related to real estate</li>
                        </ul>
                        <li>HiTech Business Services</li>
                        <ul>
                          <li>5112 Software publishers, 5151 Radio and television broadcasting, 5152 Pay and specialty television, 5171 Wired telecommunications carriers, 5172 Wireless telecommunications carriers (except satellite), 5174 Satellite telecommunications, 5179 Other telecommunications, 5182 Data processing, hosting, and related services, 5413 Architectural, engineering and related services, 5414 Specialized design services, 5415 Computer systems design and related services, 5416 Management, scientific and technical consulting services, 5417 Scientific research and development services, 5419 Other professional, scientific and technical services	</li>
                        </ul>
                        <li>LoTech Business Services</li>
                        <ul>
                          <li>4871 Scenic and sightseeing transportation, land, 4872 Scenic and sightseeing transportation, water, 4879 Scenic and sightseeing transportation, other, 4911 Postal service, 4921 Couriers, 4922 Local messengers and local delivery, 4931 Warehousing and storage, 5191 Other information services, 5321 Automotive equipment rental and leasing, 5322 Consumer goods rental, 5323 General rental centres, 5324 Commercial and industrial machinery and equipment rental and leasing, 5331 Lessors of non-financial intangible assets (except copyrighted works), 5411 Legal services, 5412 Accounting, tax preparation, bookkeeping and payroll services, 5418 Advertising, public relations, and related services, 5511 Management of companies and enterprises, 5611 Office administrative services, 5612 Facilities support services, 5613 Employment services, 5614 Business support services, 5615 Travel arrangement and reservation services, 5616 Investigation and security services, 5617 Services to buildings and dwellings, 5619 Other support services, 5621 Waste collection, 5622 Waste treatment and disposal, 5629 Remediation and other waste management services, 6241 Individual and family services, 6242 Community food and housing, and emergency and other relief services, 6243 Vocational rehabilitation services, 6244 Child day-care services, 7111 Performing arts companies, 7112 Spectator sports, 7113 Promoters (presenters) of performing arts, sports and similar events, 7114 Agents and managers for artists, athletes, entertainers and other public figures, 7115 Independent artists, writers and performers, 7121 Heritage institutions, 7131 Amusement parks and arcades, 7132 Gambling industries, 7139 Other amusement and recreation industries, 7211 Traveller accommodation, 7212 Recreational vehicle (RV) parks and recreational camps, 7213 Rooming and boarding houses, 7223 Special food services, 7224 Drinking places (alcoholic beverages), 7225 Full-service restaurants and limited service eating places, 8111 Automotive repair and maintenance, 8112 Electronic and precision equipment repair and maintenance, 8113 Commercial and industrial machinery and equipment (except automotive and electronic) repair and maintenance, 8114 Personal and household goods repair and maintenance, 8121 Personal care services, 8122 Funeral services, 8123 Dry cleaning and laundry services, 8129 Other personal services, 8131 Religious organizations, 8132 Grant-making and giving services, 8133 Social advocacy organizations, 8134 Civic and social organizations, 8139 Business, professional, labour and other membership organizations, 8141 Private households</li>
                        </ul>
                        <li>Other Transport</li>
                        <ul>
                          <li>4811 Scheduled air transportation, 4812 Non-scheduled air transportation, 4851 Urban transit systems, 4852 Interurban and rural bus transportation, 4853 Taxi and limousine service, 4854 School and employee bus transportation, 4855 Charter bus industry, 4859 Other transit and ground passenger transportation, 4861 Pipeline transportation of crude oil, 4862 Pipeline transportation of natural gas, 4869 Other pipeline transportation, 4881 Support activities for air transportation, 4885 Freight transportation arrangement, 4889 Other support activities for transportation</li>
                        </ul>
                        <li>Rail Transport</li>
                        <ul>
                          <li>4821 Rail transportation, 4882 Support activities for rail transportation</li>
                        </ul>
                        <li>Retail Trade</li>
                        <ul>
                          <li>4411 Automobile dealers, 4412 Other motor vehicle dealers, 4413 Automotive parts, accessories and tire stores, 4421 Furniture stores, 4422 Home furnishings stores, 4431 Electronics and appliance stores, 4441 Building material and supplies dealers, 4442 Lawn and garden equipment and supplies stores, 4451 Grocery stores, 4452 Specialty food stores, 4453 Beer, wine and liquor stores, 4461 Health and personal care stores, 4471 Gasoline stations, 4481 Clothing stores</li>
                        </ul>
                        <li>Small Manufacturing</li>
                        <ul>
                          <li>3131 Fibre, yarn and thread mills, 3132 Fabric mills, 3133 Textile and fabric finishing and fabric coating, 3141 Textile furnishings mills, 3149 Other textile product mills, 3151 Clothing knitting mills, 3152 Cut and sew clothing manufacturing, 3159 Clothing accessories and other clothing manufacturing, 3161 Leather and hide tanning and finishing, 3162 Footwear manufacturing, 3169 Other leather and allied product manufacturing, 3231 Printing and related support activities, 3251 Basic chemical manufacturing, 3252 Resin, synthetic rubber, and artificial and synthetic fibres and filaments manufacturing	</li>
                        </ul>
                        <li>Truck Transport</li>
                        <ul>
                          <li>4841 General freight trucking, 4842 Specialized freight trucking, 4884 Support activities for road transportation	</li>
                        </ul>
                        <li>Utilities</li>
                        <ul>
                          <li>2211 Electric power generation, transmission and distribution, 2212 Natural gas distribution, 2213 Water, sewage and other systems	</li>
                        </ul>
                        <li>Water Transport</li>
                        <ul>
                          <li>4831 Deep-sea, coastal and great lakes water transportation, 4832 Inland water transportation, 4883 Support activities for water transportation	</li>
                        </ul>
                        <li>Wholesale Trade</li>
                        <ul>
                          <li>4111 Farm product merchant wholesalers, 4121 Petroleum and petroleum products merchant wholesalers, 4131 Food merchant wholesalers, 4132 Beverage merchant wholesalers, 4133 Cigarette and tobacco product merchant wholesalers, 4134 Cannabis merchant wholesalers, 4141 Textile, clothing and footwear merchant wholesalers, 4142 Home entertainment equipment and household appliance merchant wholesalers, 4143 Home furnishings merchant wholesalers, 4144 Personal goods merchant wholesalers, 4145 Pharmaceuticals, toiletries, cosmetics and sundries merchant wholesalers, 4151 Motor vehicle merchant wholesalers, 4152 New motor vehicle parts and accessories merchant wholesalers, 4153 Used motor vehicle parts and accessories merchant wholesalers, 4161 Electrical, plumbing, heating and air-conditioning equipment and supplies merchant wholesalers, 4162 Metal service centres, 4163 Lumber, millwork, hardware and other building supplies merchant wholesalers, 4171 Farm, lawn and garden machinery and equipment merchant wholesalers, 4172 Construction, forestry, mining, and industrial machinery, equipment and supplies merchant wholesalers, 4173 Computer and communications equipment and supplies merchant wholesalers, 4179 Other machinery, equipment and supplies merchant wholesalers, 4181 Recyclable material merchant wholesalers, 4182 Paper, paper product and disposable plastic product merchant wholesalers, 4183 Agricultural supplies merchant wholesalers, 4184 Chemical (except agricultural) and allied product merchant wholesalers, 4189 Other miscellaneous merchant wholesalers, 4191 Business-to-business electronic markets, and agents and brokers</li>
                        </ul>
                      </ul>
        ")
      ),
    shiny::tabPanel("Local Area Boundaries",
      HTML("
        <h6>Geographical Boundaries for Local Areas in the 2020 LAED Model</h6>
                      <dl>
                        <dt>Castlegar-Arrow Lakes</dt>
                        <dd>Salmo (5903011), Castlegar (5903045), Central Kootenay G (5903047), Central Kootenay I (5903056), Central Kootenay J (5903058)</dd>
                        <dt>Columbia - Shuswap</dt>
                        <dd>Columbia-Shuswap C (5939037), Columbia-Shuswap F (5939044), Chum Creek 2 (5939801), Hustalen 1 (5939802), North Bay 5 (5939803), Quaaout 1 (5939805), Scotch Creek 4 (5939807)</dd>
                        <dt>Cranbrook</dt>
                        <dd>Cranbrook (5901022), East Kootenay C (5901035), Isidore's Ranch 4 (5901802), Kootenay 1 (5901803), Cassimayooks (Mayook) 5 (5901805), St. Mary's (5901808)</dd>
                        <dt>Creston</dt>
                        <dd>Creston (5903004), Central Kootenay A (5903010), Central Kootenay B (5903013), Central Kootenay C (5903017), Creston 1 (5903807)</dd>
                        <dt>Enderby</dt>
                        <dd>Enderby (5937033), North Okanagan F (5937041), Enderby 2 (5937802)</dd>
                        <dt>Fernie</dt>
                        <dd>Elkford (5901003), Sparwood (5901006), Fernie (5901012), East Kootenay A (5901017), East Kootenay B (5901019), Tobacco Plains 2 (5901801)</dd>
                        <dt>Golden</dt>
                        <dd>Golden (5939007), Columbia-Shuswap A (5939011)</dd>
                        <dt>Grand Forks-Greenwood</dt>
                        <dd>Grand Forks (5905032), Midway (5905037), Greenwood (5905042), Kootenay Boundary C / Christina Lake (5905050), Kootenay Boundary D / Rural Grand Forks (5905052), Kootenay Boundary E / West Boundary (5905054)</dd>
                        <dt>Hope-Fraser Canyon</dt>
                        <dd>Hope (5909009), Fraser Valley A (5909014), Fraser Valley B (5909016), Chawathil 4 (5909804), Inkahtsaph 6 (5909805), Kopchitchin 2 (5909806), Ohamil 1 (5909807), Puckatholetchin 11 (5909808), Saddle Rock 9 (5909809), Lukseetsissum 9 (5909810), Schkam 2 (5909812), Skawahlook 1 (5909814), Speyum 3 (5909815), Spuzzum 1 (5909816), Tuckkwiowhum 1 (5909817), Yale Town 1 (5909818), Kahmoose 4 (5909819), Boston Bar 1A (5909836), Stullawheets 8 (5909841), Peters 1 (5909843), Bucktum 4 (5909847), Ruby Creek 2 (5909851), Albert Flat 5 (5909876)</dd>
                        <dt>Invermere</dt>
                        <dd>Invermere (5901039), Radium Hot Springs (5901040), Canal Flats (5901043), East Kootenay F (5901046), East Kootenay G (5901048), Columbia Lake 3 (5901804), Shuswap (5901806)</dd>
                        <dt>Kamloops</dt>
                        <dd>Kamloops (5933042), Thompson-Nicola L (Grasslands) (5933060), Kamloops 1 (5933880)</dd>
                        <dt>Kelowna</dt>
                        <dd>Kelowna (5935010), Central Okanagan (5935012), Lake Country (5935016), Duck Lake 7 (5935801)</dd>
                        <dt>Keremeos</dt>
                        <dd>Keremeos (5907009), Okanagan-Similkameen B (5907026), Okanagan-Similkameen G (5907053), Lower Similkameen 2 (5907801), Chopaka 7 &amp; 8 (5907805), Blind Creek 6 (5907806), Chuchuwayha 2 (5907807), Alexis 9 (5907808), Ashnola 10 (5907809)</dd>
                        <dt>Kimberley</dt>
                        <dd>Kimberley (5901028), East Kootenay E (5901037)</dd>
                        <dt>Lumby</dt>
                        <dd>Lumby (5937005), North Okanagan D (5937022), North Okanagan E (5937023)</dd>
                        <dt>Merritt</dt>
                        <dd>Merritt (5933006), Thompson-Nicola M (Beautiful Nicola Valley - North) (5933008), Thompson-Nicola N (Beautiful Nicola Valley - South) (5933012), Coldwater 1 (5933801), Douglas Lake 3 (5933802), Joeyaska 2 (5933805), Nicola Lake 1 (5933806), Nicola Mameet 1 (5933807), Nooaitch 10 (5933808), Paul's Basin 2 (5933809), Zoht 4 (5933811)</dd>
                        <dt>Nelson</dt>
                        <dd>Nelson (5903015), Central Kootenay E (5903041), Central Kootenay F (5903043)</dd>
                        <dt>North Thompson</dt>
                        <dd>Clearwater (5933067), Thompson-Nicola A (Wells Gray Country) (5933068), Thompson-Nicola B (Thompson Headwaters) (5933070), Thompson-Nicola O (Lower North Thompson) (5933072), Barriere (5933074), Whispering Pines 4 (5933877), Nekalliston 2 (5933886), North Thompson 1 (5933887), Louis Creek 4 (5933888), Squaam 2 (5933889)</dd>
                        <dt>Oliver-Osoyoos</dt>
                        <dd>Osoyoos (5907005), Oliver (5907014), Okanagan-Similkameen A (5907022), Okanagan-Similkameen C (5907028), Osoyoos 1 (5907802)</dd>
                        <dt>Peachland</dt>
                        <dd>Peachland (5935018), Central Okanagan West (5935020)</dd>
                        <dt>Penticton</dt>
                        <dd>Summerland (5907035), Penticton (5907041), Okanagan-Similkameen D (5907047), Okanagan-Similkameen I (5907048), Okanagan-Similkameen E (5907049), Okanagan-Similkameen F (5907051), Penticton 1 (5907803)</dd>
                        <dt>Princeton</dt>
                        <dd>Princeton (5907024), Okanagan-Similkameen H (5907055), Lulu 5 (5907850)</dd>
                        <dt>Revelstoke</dt>
                        <dd>Revelstoke (5939019), Columbia-Shuswap B (5939023)</dd>
                        <dt>Rossland</dt>
                        <dd>Rossland (5905023), Kootenay Boundary B / Lower Columbia-Old-Glory (5905030)</dd>
                        <dt>Salmon Arm</dt>
                        <dd>Salmon Arm (5939032), Columbia-Shuswap D (5939039), Okanagan (Part) 1 (5939804), Salmon River 1 (5939806), Switsemalph 3 (5939808), Switsemalph (5939811)</dd>
                        <dt>Shuswap East</dt>
                        <dd>Thompson-Nicola P (Rivers and the Peaks) (5933044), Sun Peaks Mountain (5933045), Chase (5933054), Sahhaltkum 4 (5933884), Stequmwhulpa 5 (5933892), Neskonlith (5933898)</dd>
                        <dt>Sicamous</dt>
                        <dd>Columbia-Shuswap E (5939043), Sicamous (5939045)</dd>
                        <dt>Slocan Valley</dt>
                        <dd>Slocan (5903019), Kaslo (5903023), Silverton (5903027), New Denver (5903032), Central Kootenay D (5903039), Nakusp (5903050), Central Kootenay H (5903052), Central Kootenay K (5903060)</dd>
                        <dt>Spallumcheen</dt>
                        <dd>Spallumcheen (5937024), Armstrong (5937028), Harris 3 (5937805)</dd>
                        <dt>Trail</dt>
                        <dd>Fruitvale (5905005), Montrose (5905009), Trail (5905014), Warfield (5905018), Kootenay Boundary A (5905026)</dd>
                        <dt>Vernon</dt>
                        <dd>Coldstream (5937010), Vernon (5937014), North Okanagan B (5937017), North Okanagan C (5937021), Okanagan (Part) 1 (5937801), Priest's Valley 6 (5937803)</dd>
                        <dt>West Bank - West Kelowna</dt>
                        <dd>West Kelowna (5935029), Tsinstikeptum 9 (5935802), Tsinstikeptum 10 (5935803)</dd>
                      </dl>
        ")
      ),
    shiny::tabPanel("Limitations",
      HTML("
        <h6>Reference years</h6>
                      <p>The model uses data from the 2016 and 2021 Censuses. The reference years are for 2015 and 2020, respectively,
            as those are deemed the most appropriate by Statistics Canada because of how people respond to the Census.
            The 2020 reference year presents multiple problems that readers should be aware of.
            First, in a normal year most people’s employment status is not expected to change much.
            Between 2020 and 2021, however, many people lost jobs, worked reduced hours and lost income,
            and/or changed jobs due to impacts related to the COVID-19 pandemic.
            Second, at the time of publication, the 2020 input-output tables had not been released by Statistics Canada,
            so this study uses the 2019 tables to represent the macroeconomic structure of B.C.’s economy.</p>
                      <p>Despite these known problems, BC Stats has decided to publish the 2020 results for readers’ interest.
            Some indicators should not be affected – for example, the Location Quotients
            and dominant basic income sources – while some will be impacted more.
            Results for the 2015 reference year are considered more reliable,
            though both reference years are available in the detailed results workbook.
            Additionally, we have decided not to publish statistics that use the “rest of province” geographical impact variable,
            available for the Income Dependence and Employment Impact Ratio indicators
            because the model calculates unreasonably high demand for many service sectors,
            and BC Stats needs to address how those excesses get reallocated by the model.
            BC Stats will work to address these issues in future updates of the model.</p>
                      <h6>Not a model of economic development</h6>
                      <p>The local area economic model has its foundation in Economic Base Theory.
            This theory assumes that a community’s exports and external sources of income are its \"economic base\"
            and are important because they pay for imports.</p>
                      <p>Users of this application should be cautioned that, while Economic Base Theory provides some useful descriptive statistics,
            it tells us little about how regions become more prosperous over time. Regions with a high proportion of jobs
            in export-oriented industries are sometimes the most post-industrial or low income places where people don’t want to live.
            Both basic and non-basic activities are needed to make a community more than just a work camp,
            and there are other theories of economic growth that tell us it is primarily people – their education levels,
            network effects, quality of life and so on – that matter to the economic prosperity of a region.</p>
        ")
      )
  )
)
