# APIsApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**GetGameSim**](APIsApi.md#GetGameSim) | **POST** /game_sim | Get Game Sim
[**GetPaSim**](APIsApi.md#GetPaSim) | **POST** /pa_sim/ | Get Pa Sim
[**GetPlayers**](APIsApi.md#GetPlayers) | **GET** /players/ | Get Players
[**GetTeams**](APIsApi.md#GetTeams) | **GET** /teams/ | Get Teams
[**GetVenues**](APIsApi.md#GetVenues) | **GET** /venues/ | Get Venues
[**Hello**](APIsApi.md#Hello) | **GET** /hello/ | Hello
[**HelloWithKey**](APIsApi.md#HelloWithKey) | **GET** /hello_with_key/ | Hello With Key


# **GetGameSim**
> array[GameSimResults] GetGameSim(body.get.game.sim.game.sim.post, num.sims=100)

Get Game Sim

### Example
```R
library(singlearity)

var.body.get.game.sim.game.sim.post <- Body_get_game_sim_game_sim_post$new(Game$new(Lineup$new(list(LineupPos$new(Player$new("full_name_example", "position_example", 123, "debut_date_example", 123, "team_abbrev_example", "active_example", "bat_side_example", "pitch_hand_example", "birth_country_example", "birth_date_example", 123, "photo_url_example"), "position_example"))), Lineup$new(list(LineupPos$new(Player$new("full_name_example", "position_example", 123, "debut_date_example", 123, "team_abbrev_example", "active_example", "bat_side_example", "pitch_hand_example", "birth_country_example", "birth_date_example", 123, "photo_url_example"), "position_example"))), Atmosphere$new(Venue$new(123, "name_example", "home_team_abbrev_example", 123), Team$new("abbreviation_example", "division_example", "league_example", 123, "name_example", "team_name_example", Venue$new(123, "name_example", "home_team_abbrev_example", 123)), 123), "date_example"), State$new("on_1b_example", "on_2b_example", "on_3b_example", 123, 123, "top_example", 123, 123, 123, 123, 123, 123)) # BodyGetGameSimGameSimPost | 
var.num.sims <- 100 # integer | 

#Get Game Sim
api.instance <- APIsApi$new()
# Configure API key authorization: APIKeyHeader
api.instance$apiClient$apiKeys['SINGLEARITY_API_KEY'] <- 'TODO_YOUR_API_KEY';
# Configure API key authorization: APIKeyQuery
api.instance$apiClient$apiKeys['SINGLEARITY_API_KEY'] <- 'TODO_YOUR_API_KEY';
result <- api.instance$GetGameSim(var.body.get.game.sim.game.sim.post, num.sims=var.num.sims)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body.get.game.sim.game.sim.post** | [**BodyGetGameSimGameSimPost**](BodyGetGameSimGameSimPost.md)|  | 
 **num.sims** | **integer**|  | [optional] [default to 100]

### Return type

[**array[GameSimResults]**](GameSimResults.md)

### Authorization

[APIKeyHeader](../README.md#APIKeyHeader), [APIKeyQuery](../README.md#APIKeyQuery)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |
| **422** | Validation Error |  -  |

# **GetPaSim**
> object GetPaSim(matchup, return.features=FALSE, model.name=var.model.name)

Get Pa Sim

### Example
```R
library(singlearity)

var.matchup <- list(Matchup$new(Player$new("full_name_example", "position_example", 123, "debut_date_example", 123, "team_abbrev_example", "active_example", "bat_side_example", "pitch_hand_example", "birth_country_example", "birth_date_example", 123, "photo_url_example"), Player$new("full_name_example", "position_example", 123, "debut_date_example", 123, "team_abbrev_example", "active_example", "bat_side_example", "pitch_hand_example", "birth_country_example", "birth_date_example", 123, "photo_url_example"), Atmosphere$new(Venue$new(123, "name_example", "home_team_abbrev_example", 123), Team$new("abbreviation_example", "division_example", "league_example", 123, "name_example", "team_name_example", Venue$new(123, "name_example", "home_team_abbrev_example", 123)), 123), State$new("on_1b_example", "on_2b_example", "on_3b_example", 123, 123, "top_example", 123, 123, 123, 123, 123, 123), "date_example")) # array[Matchup] | 
var.return.features <- FALSE # character | 
var.model.name <- 'model.name_example' # character | 

#Get Pa Sim
api.instance <- APIsApi$new()
# Configure API key authorization: APIKeyHeader
api.instance$apiClient$apiKeys['SINGLEARITY_API_KEY'] <- 'TODO_YOUR_API_KEY';
# Configure API key authorization: APIKeyQuery
api.instance$apiClient$apiKeys['SINGLEARITY_API_KEY'] <- 'TODO_YOUR_API_KEY';
result <- api.instance$GetPaSim(var.matchup, return.features=var.return.features, model.name=var.model.name)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **matchup** | list( [**Matchup**](Matchup.md) )|  | 
 **return.features** | **character**|  | [optional] [default to FALSE]
 **model.name** | **character**|  | [optional] 

### Return type

**object**

### Authorization

[APIKeyHeader](../README.md#APIKeyHeader), [APIKeyQuery](../README.md#APIKeyQuery)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |
| **422** | Validation Error |  -  |

# **GetPlayers**
> array[Player] GetPlayers(name=var.name, id=var.id, team.name=var.team.name, team.id=var.team.id, position=var.position, age.min=var.age.min, age.max=var.age.max, pitch.hand=var.pitch.hand, bat.side=var.bat.side, active=var.active, on.40=var.on.40)

Get Players

### Example
```R
library(singlearity)

var.name <- 'name_example' # character | 
var.id <- 56 # integer | 
var.team.name <- 'team.name_example' # character | 
var.team.id <- 56 # integer | 
var.position <- list("inner_example") # array[character] | 
var.age.min <- 56 # integer | 
var.age.max <- 56 # integer | 
var.pitch.hand <- list("inner_example") # array[character] | 
var.bat.side <- list("inner_example") # array[character] | 
var.active <- 'active_example' # character | 
var.on.40 <- 'on.40_example' # character | 

#Get Players
api.instance <- APIsApi$new()
# Configure API key authorization: APIKeyHeader
api.instance$apiClient$apiKeys['SINGLEARITY_API_KEY'] <- 'TODO_YOUR_API_KEY';
# Configure API key authorization: APIKeyQuery
api.instance$apiClient$apiKeys['SINGLEARITY_API_KEY'] <- 'TODO_YOUR_API_KEY';
result <- api.instance$GetPlayers(name=var.name, id=var.id, team.name=var.team.name, team.id=var.team.id, position=var.position, age.min=var.age.min, age.max=var.age.max, pitch.hand=var.pitch.hand, bat.side=var.bat.side, active=var.active, on.40=var.on.40)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **name** | **character**|  | [optional] 
 **id** | **integer**|  | [optional] 
 **team.name** | **character**|  | [optional] 
 **team.id** | **integer**|  | [optional] 
 **position** | list( **character** )|  | [optional] 
 **age.min** | **integer**|  | [optional] 
 **age.max** | **integer**|  | [optional] 
 **pitch.hand** | list( **character** )|  | [optional] 
 **bat.side** | list( **character** )|  | [optional] 
 **active** | **character**|  | [optional] 
 **on.40** | **character**|  | [optional] 

### Return type

[**array[Player]**](Player.md)

### Authorization

[APIKeyHeader](../README.md#APIKeyHeader), [APIKeyQuery](../README.md#APIKeyQuery)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |
| **422** | Validation Error |  -  |

# **GetTeams**
> array[Team] GetTeams(name=var.name)

Get Teams

### Example
```R
library(singlearity)

var.name <- 'name_example' # character | 

#Get Teams
api.instance <- APIsApi$new()
# Configure API key authorization: APIKeyHeader
api.instance$apiClient$apiKeys['SINGLEARITY_API_KEY'] <- 'TODO_YOUR_API_KEY';
# Configure API key authorization: APIKeyQuery
api.instance$apiClient$apiKeys['SINGLEARITY_API_KEY'] <- 'TODO_YOUR_API_KEY';
result <- api.instance$GetTeams(name=var.name)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **name** | **character**|  | [optional] 

### Return type

[**array[Team]**](Team.md)

### Authorization

[APIKeyHeader](../README.md#APIKeyHeader), [APIKeyQuery](../README.md#APIKeyQuery)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |
| **422** | Validation Error |  -  |

# **GetVenues**
> array[Venue] GetVenues(stadium.name=var.stadium.name, team.name=var.team.name)

Get Venues

### Example
```R
library(singlearity)

var.stadium.name <- 'stadium.name_example' # character | 
var.team.name <- 'team.name_example' # character | 

#Get Venues
api.instance <- APIsApi$new()
# Configure API key authorization: APIKeyHeader
api.instance$apiClient$apiKeys['SINGLEARITY_API_KEY'] <- 'TODO_YOUR_API_KEY';
# Configure API key authorization: APIKeyQuery
api.instance$apiClient$apiKeys['SINGLEARITY_API_KEY'] <- 'TODO_YOUR_API_KEY';
result <- api.instance$GetVenues(stadium.name=var.stadium.name, team.name=var.team.name)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **stadium.name** | **character**|  | [optional] 
 **team.name** | **character**|  | [optional] 

### Return type

[**array[Venue]**](Venue.md)

### Authorization

[APIKeyHeader](../README.md#APIKeyHeader), [APIKeyQuery](../README.md#APIKeyQuery)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |
| **422** | Validation Error |  -  |

# **Hello**
> object Hello()

Hello

### Example
```R
library(singlearity)


#Hello
api.instance <- APIsApi$new()
result <- api.instance$Hello()
dput(result)
```

### Parameters
This endpoint does not need any parameter.

### Return type

**object**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |

# **HelloWithKey**
> object HelloWithKey()

Hello With Key

### Example
```R
library(singlearity)


#Hello With Key
api.instance <- APIsApi$new()
# Configure API key authorization: APIKeyHeader
api.instance$apiClient$apiKeys['SINGLEARITY_API_KEY'] <- 'TODO_YOUR_API_KEY';
# Configure API key authorization: APIKeyQuery
api.instance$apiClient$apiKeys['SINGLEARITY_API_KEY'] <- 'TODO_YOUR_API_KEY';
result <- api.instance$HelloWithKey()
dput(result)
```

### Parameters
This endpoint does not need any parameter.

### Return type

**object**

### Authorization

[APIKeyHeader](../README.md#APIKeyHeader), [APIKeyQuery](../README.md#APIKeyQuery)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |

