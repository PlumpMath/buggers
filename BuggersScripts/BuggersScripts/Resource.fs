module BuggersScripts.Types
open System

exception InvalidResource of string

type Resource = Food | Wood | Rock | Money

let fromString str = match str with
    | "Food" -> Food
    | "Wood" -> Wood
    | "Rock" -> Rock
    | "Money" -> Money
    | _ -> raise (InvalidResource(str + " is not a valid resource type."))
    
    