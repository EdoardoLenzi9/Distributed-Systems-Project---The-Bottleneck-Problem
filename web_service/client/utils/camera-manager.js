/*
* Camera management script
*
* author = 'Talissa Dreossi, Edoardo Lenzi'
* version = '1.0'
* license = 'GPL-3.0'
*/


/*
* Init camera and call camera control initialization
*/
function InitCamera(){
	camera = new THREE.PerspectiveCamera( 75, window.innerWidth / window.innerHeight, 1, 1000 );
	InitControl();
}


/*
* Init Camera Control
*/
function InitControl(minDistance = 100, maxDistance=500){

	controls = new THREE.OrbitControls( camera );
	controls.enableDamping = true; // an animation loop is required when either damping or auto-rotation are enabled
	controls.dampingFactor = 0.25;
	controls.screenSpacePanning = false;
	controls.minDistance = minDistance;
	controls.maxDistance = maxDistance;
	controls.maxPolarAngle = Math.PI / 2;
	
}
