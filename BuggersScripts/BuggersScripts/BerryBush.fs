// Required components
// ResourceProducer
// May be adapted to just a generic change view based on amount of resource script.
namespace BuggersScripts.BerryBush
open BuggersScripts.ResourceProducer
open UnityEngine

type BerryBush () =
    inherit MonoBehaviour()
    
    [<DefaultValue>] val mutable hasBerries : bool
    [<DefaultValue>] val mutable resource : ResourceProducer
    [<DefaultValue>] val mutable berriesPrefab : GameObject
    
    let mutable berriesRef : GameObject= null
        
    member this.Update () =
        if this.resource.currentAmount >= this.resource.maximum &&
            not this.hasBerries then
            let berries = GameObject.Instantiate(this.berriesPrefab, this.transform.position, Quaternion.identity) :?> GameObject
            do berries.transform.position <- this.transform.position
               berries.transform.parent <- this.transform
               berriesRef <- berries
               this.hasBerries <- true
               ()
        elif this.resource.currentAmount < this.resource.maximum &&
              this.hasBerries then
              let berries = berriesRef
              do berriesRef <- null
                 this.hasBerries <- false
                 GameObject.Destroy(berries)
                 ()
        else
            ()
