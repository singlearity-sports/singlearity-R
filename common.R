library(singlearity)

#set the api key in an environment variable
key = Sys.getenv('SINGLEARITY_API_KEY')
if (nchar(key) == 0)
{
  stop("You need to set an API KEY in your environment.  Modify (or create) a .Renviron file with a line containing your API KEY, for instance:
       SINGLEARITY_API_KEY=myveryspecialkey")
}

sing = APIsApi$new()
sing$apiClient$apiKeys['SINGLEARITY_API_KEY'] = key
sing$apiClient$basePath='https://api.singlearity.com'