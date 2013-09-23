
namespace BuggersScripts.ResourceProducer
open BuggersScripts.Types
open UnityEngine

type ResourceProducer () =
    inherit MonoBehaviour()
    
    [<DefaultValue>] val mutable resourceType : string
    [<DefaultValue>] val mutable currentAmount : int
    [<DefaultValue>] val mutable generationRate : int
    [<DefaultValue>] val mutable maximum : int
    
    let mutable generatedPartialAmount = 0.0f
    
    // pure function, much easier to test int the repl
    let calculateGenerated current partial genRate delta =
        let additional = ((float32 genRate) * delta) + partial
        let newAmount = additional + (float32 current)
        let intAmount = (int newAmount)
        let leftOver = newAmount - (float32 intAmount)
        intAmount, leftOver
    
    member this.Start () =
        let realType = fromString this.resourceType // validates the type
        ()
    
    member this.Update () =
        if this.currentAmount < this.maximum then
            let newAmount, leftOver =
                calculateGenerated this.currentAmount generatedPartialAmount this.generationRate Time.deltaTime
            do
                this.currentAmount <- newAmount
                generatedPartialAmount <- leftOver
                ()
        else
            ()
            
    member this.Gather (amount : int) =
        if this.currentAmount >= amount then
            let newAmount = this.currentAmount - amount
            do this.currentAmount <- newAmount
            amount
        else
            0