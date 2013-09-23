namespace BuggersScripts.MoveGuy
open UnityEngine

type MoveGuy () =
    inherit MonoBehaviour()
    
    [<DefaultValue>] val mutable guySpeed : float32
        
    member this.Update () =
        let forward = new Vector3(0.0f, 0.0f, 1.0f)
        
        let keyMovements = [KeyCode.W, new Vector3(0.0f, 0.0f, 1.0f);
                            KeyCode.S, new Vector3(0.0f, 0.0f, -1.0f);
                            KeyCode.A, new Vector3(-1.0f, 0.0f, 0.0f);
                            KeyCode.D, new Vector3(1.0f, 0.0f, 0.0f)]
                            
        let folder (state : Vector3) (code : KeyCode, (d : Vector3)) = if Input.GetKey code then state + d else state
        let direction = List.fold folder Vector3.zero keyMovements
        
        
        let movement = direction.normalized * (this.guySpeed * Time.deltaTime)

        do
            this.transform.Translate movement
            ()