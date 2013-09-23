namespace BuggersScripts.ClickBushesToEatThem
open BuggersScripts.Hunger
open BuggersScripts.ResourceProducer
open UnityEngine

type ClickBushesToEatThem () =
    inherit MonoBehaviour()
    
    [<DefaultValue>] val mutable camera : Camera
    [<DefaultValue>] val mutable hunger : Hunger
    [<DefaultValue>] val mutable amountToEat : int
    
    // This is annoying because I have the models as children of the thing.
    // In real life should just have the model of the thing change based on states and animations and shit and this code will be much better
    
    member this.EatIfCloseEnough (bushPos : Vector3) (playerPos : Vector3) (bush : ResourceProducer) =
        if (bushPos - playerPos).magnitude < 2.0f then
            let amount = bush.Gather this.amountToEat
            this.hunger.Eat amount
    
    member this.Update () =            
        if Input.GetKeyDown(KeyCode.Mouse0) &&
            this.camera.pixelRect.Contains(Input.mousePosition) then
            let ray = this.camera.ScreenPointToRay(Input.mousePosition)
            let (isHit, hit) = Physics.Raycast(ray)
            if isHit then
                let clickedObject = hit.transform.parent.gameObject
                let resourceProducer = clickedObject.GetComponent("ResourceProducer")
                if resourceProducer <> null then
                    let rp = resourceProducer :?> ResourceProducer
                    this.EatIfCloseEnough clickedObject.transform.position this.transform.position rp