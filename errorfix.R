library(data.table)
library(ogbox)
library(dplyr)

errorFix = function(hashFile){
    hash = fread(hashFile)
    hash = data.frame(hash)
    names(hash) = c('GEO', 'hash')
    errors = hash %>% filter(hash=="ERROR" | is.na(hash))
    ids = errors$GEO
    
    fix  = sapply(ids, function(i){
        tryCatch({
            ptm <- proc.time()
            
            isDown=gsmDown(paste0("GSM",i), as.character(i),overwrite=F,warnings=F)
            
            if (isDown){
                out = c(i, md5sum(as.character(i)))
                file.remove(as.character(i))
                print(i)
                print(proc.time() - ptm)
            } else{
                out = c(i, NA)
            }
        }, error = function(e){
            out = c(i , 'ERROR')
            print('error')
            print(i)
        }) 
        return(out)
    })
    fix = as.data.frame(t(as.data.frame(fix)))
    names(fix) = c('GEO', 'hash')
    fix[,1] = as.numeric(as.character(fix[,1]))
    hash[match(fix$GEO,hash$GEO),]$hash = fix$hash
    write.table(hash,hashFile, row.names=F, quote=F, col.names=F)
    errors = hash %>% filter(hash=="ERROR" | is.na(hash))
    if (nrow(errors)>0){
        return(FALSE)
    } else {
        return(TRUE)
    }
    
}