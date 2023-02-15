#consumer price index

url <- "https://data.ssb.no/api/v0/en/table/03013/"
# 
data.tmp <- '
{
  "query": [
    {
      "code": "Konsumgrp",
      "selection": {
        "filter": "vs:CoiCop2016niva1",
        "values": [
          "TOTAL"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}
'
d.tmp <- POST(url , body = data.tmp, encode = "json", verbose())
ssb_cpi <- fromJSONstat(content(d.tmp, "text"))
ssb_cpi <- clean_names(ssb_cpi)


ssb_cpi <- tibble(ssb_cpi) 

ssb_cpi<- separate(ssb_cpi, month, into = c("year", "month"), sep = "M")
ssb_cpi$date  <- paste0(ssb_cpi$year,"-", ssb_cpi$month) 

ssb_cpi$date <- as.Date(as.yearmon(ssb_cpi$date), frac = 1)

ssb_cpi <- ssb_cpi %>%
  select(date, everything())
ssb_cpi_expand <- pivot_wider(ssb_cpi, 
                                id_expand = FALSE,
                                names_from = contents,
                                values_from = c("value"))
ssb_cpi_expand <- ssb_cpi_expand %>%
  clean_names()

write_csv(ssb_cpi_expand, file = (paste0("csv/ssb/", objects(pattern="ssb_cpi_expand") , ".csv")))
remove(ssb_cpi, d.tmp)

remove(d.tmp)
