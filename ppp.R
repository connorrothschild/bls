df <- readxl::read_excel(here::here("data/ppp_survey.xlsx"))

# Which of the following best describes Covid-19 pandemic’s impact on your organization?
table(df$CovidImpact)

# Did your organization apply for the PPP loan?
table(df$Apply_PPP_Loan)

# Which of the following best describes the result of your latest PPP loan application?
# 1	My organization’s application was approved for the full amount requested
# 2 My organization’s application was approved for an amount less than what was requested
# 3 My organization’s application was processed by a lender but I do not know the status of my application with the SBA
# 4 My organization’s application was processed by a lender but was declined by the SBA
# 5 My organization’s application was declined by a lender
# 6 None of the above (please specify)
table(df$Outcomeofapplying)

# My organization found the regulations of the PPP loan to be restrictive (1 = strongly agree, 5 = strongly disagree)
mean(df$MyOrg_3, na.rm = T)

# My organization was/will be able to retain employees because of the PPP loan we received
mean(df$MyOrg_6, na.rm = T)

# My organization used PPP loan money to pay employees to stay home as the nature of work was not amenable to a “work from home” arrangement and the organization was required to stay closed due to State Covid-19 order (e.g., restaurants, hair salons)
mean(df$MyOrg_7, na.rm = T)

# My organization found the PPP loan to be very helpful to financially support ourselves
mean(df$MyOrg_12, na.rm = T)

# In addition to the PPP loan, does your organization anticipate needing additional financial assistance over the next 12 months?
table(df$Additional_assit)
