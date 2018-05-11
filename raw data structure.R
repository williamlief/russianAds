# Text is structured as follows in the raw PDFs on the first page:
#
# AD ID [numeric, id for ad]
# Ad Text [string, transcription of ad]
# Ad Landing Page [string, url]
# Ad Targeting Location: [string, geographic location]
# Ad Targeting Location-LivingIn: [string, alternate specification of above]
#   Interests: [string]
#   Excluded Connections: [string, groups excluded]
#   Age: [string, age range]
#   Language: [string, language of ad]
#   Placements: [string, ]
#   Gender: [string, Female/Male]
#   People Who Match: [string, ]
# Ad Impressions [string, description of devices/location ad was displayed on]
# Ad Clicks [numeric]
# Ad Spend [string, amount and currency]
# Ad Creation Date [date, mm/dd/yy hh/mm/ss AM TZ]
# Ad End Date [date, mm/dd/yy hh/mm/ss AM TZ]
#
# Additional notes: Ad End Date is sometimes missing
# When an entry is long it gets split over multiple lines