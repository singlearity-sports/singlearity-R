#set the api key in an environment variable

GetSinglearityClient <- function() {
        key = Sys.getenv('SINGLEARITY_API_KEY')
        server = Sys.getenv('SINGLEARITY_API_SERVER')
        if (nchar(key) == 0)
        {
            stop("You need to set an API KEY in your environment.  Modify (or create) a .Renviron file with a line containing your API KEY, for instance:
            SINGLEARITY_API_KEY=myveryspecialkey")
        }

        if (nchar(server) == 0)
        {
            #default server
            server = 'https://api.singlearity.com'
        }
        sing <- APIsApi$new()
        sing$apiClient$apiKeys['SINGLEARITY_API_KEY'] = key
        sing$apiClient$basePath <- server
        return(sing)

}

