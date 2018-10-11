build_search_url <- function(model,years=c(NA,NA),location="london",page=1,urlending,verbose=1){
    ## Set base url
    url <- "https://www.kijiji.ca/b-cars-trucks"

    ## Add location
    url <- paste(url,location,sep="/")

    ## Add model
    url <- paste(url,model,sep="/")

    ## Add years
    if(!is.na(years[1])){
        if(!is.na(years[2]))
            url <- paste(url,paste0(c(years[1],"__",years[2]),collapse=""),sep="/")
        else
            url <- paste(url,paste0(c(years[1],"__"),collapse=""),sep="/")
    }
    else if(!is.na(years[2]))
        url <- paste(url,paste0(c("__",years[2]),collapse=""),sep="/")

    ## Add page number
    if(page > 1)
        url <- paste(url,paste0("page-",page),sep="/")

    ## Guess at trailing barf if not specified
    ## if(is.
    ## if(location=="london")
    ##     url <- paste(url,"k0c174l1700214a68",sep="/")
    ## else if(location=="ontario")
    ##     url <- paste(url,"k0c174l9004",sep="/")

    ## Add gibberish url ending
    url <- paste(url,urlending,sep="/")

    if(verbose > 0) cat("    ",url,"\n")

    return(url)
}

get_ad_info <- function(adURL,verbose=1){
    if(verbose==1) cat(".")

    ## Set url
    url <- paste0("https://www.kijiji.ca",adURL,"?enableSearchNavigationFlag=true")

    if(verbose > 1) cat("\n",url,"\n")

    ## Read html
    html <- getURL(url)

    ## Extract data in dataLayer section
    pattern <- "dataLayer = \\[\\{(.+)\\}\\]"
    dataLayer <- str_match(html,pattern)[,2]

    pattern <- "\"(\\p{Alphabetic}+?_\\p{Alphabetic})\":\"(.+?)\""

    tmp <- str_match_all(dataLayer,pattern)[[1]]

    if(is.na(tmp[1])){
        if(verbose > 1) cat(" FAILED")

        return(NULL)
    }

    data1 <- str_match_all(dataLayer,pattern)[[1]][,2:3] %>%
        as.tibble() %>%
        spread(key=1,value=2)

    ## Extract descriptors in unnamed key/value section
    pattern <- "key: '(.*?)', value: '(.*?)'"

    data2 <- str_match_all(html,pattern)[[1]][,2:3] %>%
        as.tibble() %>%
        spread(key=1,value=2)

    ## Compile final data
    bind_cols(data1,data2,data.frame(url=url))
}

get_ads_page<- function(page=1,model,years=c(NA,NA),location="london",urlending,verbose=1){

    if(verbose>0) cat("Extracting data for page",page,":\n")

    ## Build search url for specific page
    if(verbose>0) cat("    Bulding search URL for page\n")


    searchURL <- build_search_url(model,years,location,page,urlending,verbose=verbose)

    ## Read HTML
    searchHTML <- getURL(searchURL)

    ## Extract ad urls
    if(verbose>0) cat("    Extracting ad urls: ")

    pattern <- "data-vip-url=\"(.*)\""
    adURLs <- str_match_all(searchHTML,pattern)[[1]][,2]

    if(verbose>0) cat(length(adURLs),"ads detected\n")

    ## Scrape ads
    if(verbose>0) cat("    Scraping ads:")

    output <- do.call("bind_rows",lapply(adURLs,get_ad_info,verbose=verbose))

    cat("\n\n")

    output
}

get_ads <- function(model,years,location,urlending,verbose=1){
    ## Build initial search url
    if(verbose>0) cat("Conducting initial search\n")

    searchURL <- build_search_url(model,years,location,1,urlending,verbose=verbose)

    ## Read html
    searchHTML <- getURL(searchURL)

    ## Extract number of pages
    npages <- str_match(searchHTML,'"tp":(\\d+)')[1,2]

    if(verbose>0) cat(npages,"pages of ads detected\n")

    ## Read ads page by page
    output <- lapply(1:npages,get_ads_page,model=model,years=years,location=location,urlending=urlending,verbose=verbose)

    ## Combine all output
    do.call("bind_rows",output)
}
