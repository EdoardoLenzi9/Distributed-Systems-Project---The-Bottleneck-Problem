/*
* Arc-reactor view script
*
* author = 'Talissa Dreossi, Edoardo Lenzi'
* version = '1.0'
* license = 'GPL-3.0'
*/


// Global variables and constants
var camera, scene, renderer, controls, stats, state;
var clock = new THREE.Clock();

// Raycast
var group = new THREE.Group();
var raycaster = new THREE.Raycaster();
var mouseVector = new THREE.Vector3();
var selectedObject = null;

// Inspector
var inspectorScene;
var inspectorHemiLight;
var inspectorDirectLight;
var switchScene = false;

// Lights
var hemiLight, dirLight;

// Meshes
var street;

/*
* Init function
*/ 
function Init() {

	// loads arc-reactor-controls view
	InitStat();
	InitScene();
	InitCamera();
	scene.rotation.x -= Math.PI/8;
	scene.rotation.z -= Math.PI/2;

	// init scene and camera pose
	camera.position.set( 0, 0, 10 );
	scene.add( group );
		
	// load test state
	LoadState( '../../assets/testState.json' );

	// desktop events
	BindEvent( window, 'mousemove', OnDocumentMouseMove );
	BindEvent( document, 'mousedown', OnMouseDown );
	BindEvent( document, 'mouseup', OnMouseUp );
	// touch screen events
	BindEvent( document, 'touchmove', OnDocumentMouseMove );
	BindEvent( document, 'touchstart', OnMouseDown );
	BindEvent( document, 'touchend', OnMouseUp );
	// general events
	BindEvent( window, 'resize', OnWindowResize );
	BindEvent( window, 'click', OnDocumentMouseClick );

	InitRenderer();
}


/*
* deserialize the json content and setup each component defined
*/ 
function LoadState( file ) {
	Read( file, function( content ){
		state = JSON.parse(content);   
		state[0].sort( arrivalTimeCriterion );
		state[1].sort( arrivalTimeCriterion );
		state[0].forEach(function(car){ 	//left side
			car.side = -1;
			car.position = car.arrivalTime;
			scene.add(new AnimatedCar(car));
		});
		state[1].forEach(function(car){ 	//right side
			car.position = car.arrivalTime;
			car.side = 1;
			scene.add(new AnimatedCar(car));
		});
	})
}


/*
* Sorting criterion
*/
function arrivalTimeCriterion( a, b ) {
	if ( a.arrivalTime < b.arrivalTime ){
	  return -1;
	}
	if ( a.arrivalTime > b.arrivalTime ){
	  return 1;
	}
	return 0;
}


/*
* Loop function
*/
function Animate() {
	TWEEN.update();
	stats.update();
	controls.update();
	requestAnimationFrame( Animate );
	Render();
}


/*
* Render function
*/
function Render()
{
	renderer.render( scene, camera );
}


/*
* Renderer init
*/
function InitRenderer(){
	renderer = new THREE.WebGLRenderer( { alpha: true, antialias: true } );
	renderer.setPixelRatio( window.devicePixelRatio );
	renderer.setSize( window.innerWidth, window.innerHeight );
	renderer.gammaOutput = true;
	renderer.gammaInput = true;
	renderer.shadowMap.enabled = true;
	document.body.appendChild( renderer.domElement );
}


/*
* Scene init
*/
function InitScene(){
	scene = new THREE.Scene();
	scene.background = new THREE.Color( 0x000022 );
	scene.fog = new THREE.Fog( 0xffffff, 0, 750 );
	hemiLight = CreateHemiLight();
	dirLight = CreateDirLight();
	street = new Street();
    scene.add( hemiLight );  
	scene.add( dirLight );  
	scene.add( street );  
}


/*
* Stat init
*/
function InitStat(){
	stats = new Stats();
	stats.domElement.style.position = 'absolute';
	stats.domElement.style.top = '0px';
	// uncomment for debugging purpose only in order to see rendering stats
	//document.body.appendChild( stats.domElement );
}


// entry-point call
Init();
Animate();