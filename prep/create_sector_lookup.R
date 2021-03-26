# https://www.census.gov/programs-surveys/economic-census/guidance/understanding-naics.html
sector_code <-
  c(
    '11',
    '21',
    '22',
    '23',
    '31',
    '32',
    '33',
    '42',
    '44',
    '45',
    '48',
    '49',
    '51',
    '52',
    '53',
    '54',
    '55',
    '56',
    '61',
    '62',
    '71',
    '72',
    '81',
    '92'
  )

sector <- c(
  'Agriculture, Forestry, Fishing and Hunting (not covered in economic census)',
  'Mining, Quarrying, and Oil and Gas Extraction',
  'Utilities',
  'Construction',
  'Manufacturing',
  'Manufacturing',
  'Manufacturing',
  'Wholesale Trade',
  'Retail Trade',
  'Retail Trade',
  'Transportation and Warehousing',
  'Transportation and Warehousing',
  'Information',
  'Finance and Insurance',
  'Real Estate and Rental and Leasing',
  'Professional, Scientific, and Technical Services',
  'Management of Companies and Enterprises',
  'Administrative and Support and Waste Management and Remediation Services',
  'Educational Services',
  'Health Care and Social Assistance',
  'Arts, Entertainment, and Recreation',
  'Accommodation and Food Services',
  'Other Services (except Public Administration)',
  'Public Administration (not covered in economic census)'
)

sector_lookup <- cbind(sector_code, sector) %>% as.data.frame()
