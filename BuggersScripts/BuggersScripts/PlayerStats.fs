namespace BuggersScripts.PlayerStats
open BuggersScripts.Hunger
open UnityEngine

type PlayerStats () =
    inherit MonoBehaviour()
    
    [<DefaultValue>] val mutable hunger : Hunger
    
    member this.OnGUI () =
        let hunger = this.hunger.hunger
        do GUILayout.Label("Hunger: " + hunger.ToString())
           ()
