manufacturer_in_study <- covid_vaccines_ConcePTION_CDM_vocabulary[covid_vaccines_ConcePTION_CDM_vocabulary %in%
                                                                    c("pfizer", "moderna", "astrazeneca", "janssen", "novavax")]
manufacturer_in_study <- c(manufacturer_in_study, "unk")