library(data.table)
library(ogbox)
library(dplyr)

geoHashList = function(ids, out){
    for (i in ids){
        tryCatch({
            ptm <- proc.time()
            
            isDown=gsmDown(paste0("GSM",i), as.character(i),overwrite=F,warnings=F)
            
            if (isDown){
                write(paste0(i,'\t', md5sum(as.character(i))),
                      file = out, append = T)
                file.remove(as.character(i))
                print(i)
                print(proc.time() - ptm)
                
                
            }
        }, error = function(e){
            write(paste0(i,'\t','ERROR'),file = out, append = T)
            write(paste0(i,'\n',str(e)),file='GEOhashError.log', append=T)
            print('error')
            print(i)
        })
    }
}

library(data.table)
library(dplyr)
library(ogbox)

hash1 = fread('hashes1')
hash2 = fread('hashes2')
hash3 = fread('hashmid')
hash4 = fread("hashMidLow")
hash5 = fread("hashUpperMid")
hash6 = fread("hash499771")
hash7 = fread("hashUpperLowerMid")
errors = fread('errorFix') 



hash  = rbind(hash1,hash2,hash3,hash4, hash5, hash6, hash7, errors)

hash=as.data.frame(hash)

names(hash) = c('GEO', 'hash')


hash = hash %>% 
    arrange(GEO) %>%
    filter(!is.na(hash)) %>%
    filter(hash != 'ERROR') %>% 
    filter(!duplicated(GEO))


inCase = fread('hashRedo') %>% arrange(V1)
 
hash[hash$GEO %in% inCase$V1,]$hash = inCase$V2

sums1 = hash[,2]
sums2 = hash[,2]
file1 = hash[,1]
file2 = hash[,1]

names(sums1) = file1
names(sums2) = file1

matching1 = names(sums1[match(sums2,sums1)])
matching1 = cbind(matching1, names(sums2))

matching2 =  names(sums2[match(sums1,sums2)])
matching2 = cbind(names(sums1), matching2)

out = unique(rbind(matching1,matching2))
out = as.data.frame(out)

out = out[out[,1]!=out[,2],]

out = apply(out,2,function(x){as.numeric(as.character(x))})

out = as.data.frame(out)

#geoHashList(unique(unlist(out)), out = 'hashRedo')

out = out %>% mutate(dif = matching1-V2)

out = out[(nrow(out)/2+1):nrow(out),]

png('difs.png',width = 3000, height = 600)
plot(out$dif)
dev.off()


out %>% filter(abs(dif)<100)

matchingOnes = out %>% select(matching1, V2) %>% unlist %>% unique

