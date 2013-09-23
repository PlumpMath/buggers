namespace BuggersScripts.Hunger
open BuggersScripts
open UnityEngine

type Hunger () =
    inherit MonoBehaviour()
    
    [<DefaultValue>] val mutable hunger : int
    [<DefaultValue>] val mutable hungerDecayRate : int
    
    
    // This is exactly the same as resource production (except I might change that one)
    // Should absolutely be broken out into pure fsharp code.
    let mutable partial = 0.0f
    
    let calculateDecay current partial decayRate delta =
        let additional = ((float32 -decayRate) * delta) + partial
        let newAmount = additional + (float32 current)
        let intAmount = (int newAmount)
        let leftOver = newAmount - (float32 intAmount)
        intAmount, leftOver
    
    member this.Update () =
        let (newHunger, newPartial) = calculateDecay this.hunger partial this.hungerDecayRate Time.deltaTime
        do this.hunger <- newHunger
           partial <- newPartial
           ()
           
    member this.Eat (amount : int) =
        do this.hunger <- this.hunger + amount
           ()