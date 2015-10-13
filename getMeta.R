source('htmlToText.R')

getMeta = function(ids, outFile){
    sapply(ids, function(i){
        page = getURL(paste0('http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSM',i))
        page = htmlToText(page)
        
        GSE = str_extract_all(page,'GSE.*?(?= )')[[1]] %>%
            paste0(collapse = ',')
        
        organism = str_extract(page,'(?<=Organism \n ).*?(?= \n)')
        platform = str_extract(page,'GPL.*?(?= )')
        
        reAna  = grepl('(Reanaly)|(Alternative)',page)
        
        write(paste(i,GSE,platform,organism,reAna,sep = '\t'),
              file = outFile, append = T)
        print(i)
    })
}