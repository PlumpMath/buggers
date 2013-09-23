using UnityEngine;
using System.Collections;
using System.Collections.Generic;

// This was the wrong approach and does not work. TODO, come back later.

public class TerrainGenerator : MonoBehaviour {
	
	public Transform center;
	public GameObject grass;
	public GameObject forest;
	public GameObject dirt;
	public GameObject water;
	
	Dictionary<string, GameObject> prefabs = new Dictionary<string, GameObject>();
	
	// Generates terrain out in a getDistance x genDistance grid from the center;
	public int genDistance = 100;
	// Determines how "noisy" the terrain is. Higher values will have more similar terrain together.
	public int noiseScale = 10;
	
	// Stores computed perlin values.
	Dictionary<Vector2, string> generatedTerrain = new Dictionary<Vector2, string>();

	string NoiseFn (float x, float y) {
		var noiseVal = Mathf.PerlinNoise(x / noiseScale, y / noiseScale);
		
		if (noiseVal < -0.5) {
			return "water";
		} else if (noiseVal < 0) {
			return "dirt";
		} else if (noiseVal < 0.75) {
			return "grass";
		} else {
			return "forest";
		}		
	}
	
	// C# is bullshit.... (Seriously at least get F# working with this)
	void RenderTerrain (float x, float z, string type) {
		Instantiate(prefabs[type], new Vector3 (x, 0f, z), Quaternion.identity);
	}
	
	void Start () {
		prefabs.Add("water", water);
		prefabs.Add("dirt", dirt);
		prefabs.Add("grass", grass);
		prefabs.Add("forest", forest);
	}
	
	// Update is called once per frame
	void Update () {
		var centerX = Mathf.Round(center.position.x);
		var centerZ = Mathf.Round(center.position.z);
		
		for (float x = (centerX - genDistance); x < (centerX + genDistance); x+=1f) {
			for (float z = (centerZ - genDistance); z < (centerZ + genDistance); z+=1f) {
				var loc = new Vector2(x, z);
				
				if (!generatedTerrain.ContainsKey(loc)) {
					var type = NoiseFn(x, z);
					generatedTerrain.Add(loc, type);
					RenderTerrain(x, z, type);
				}
			}
		}
	}
}
