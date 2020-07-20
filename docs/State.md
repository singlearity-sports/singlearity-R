# singlearity::State

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**on_1b** | **character** | True iff there is a runner on 1st base. | [optional] [default to FALSE]
**on_2b** | **character** | True iff there is a runner on 2nd base. | [optional] [default to FALSE]
**on_3b** | **character** | True iff there is a runner on 3rd base. | [optional] [default to FALSE]
**inning** | **integer** |  | [optional] [default to 1]
**outs** | **integer** |  | [optional] [default to 0]
**top** | **character** | True if in the top half of the inning. | [optional] [default to TRUE]
**bat_score** | **integer** | Batting team score | [optional] [default to 0]
**fld_score** | **integer** | Batting team score | [optional] [default to 0]
**bat_lineup_order** | **integer** | Lineup position (1-9) of the batting team.  Used only for game simulations | [optional] [default to 1]
**fld_lineup_order** | **integer** | Lineup position (1-9) of the batting team.  Used only for game simulations | [optional] [default to 1]
**pitch_number** | **integer** | Current pitcher&#39;s pitch count at the beginning of the plate appearance | [optional] [default to 0]
**frame_runs_scored** | **integer** | Number of runs scored in the current half inning so far | [optional] [default to 0]


