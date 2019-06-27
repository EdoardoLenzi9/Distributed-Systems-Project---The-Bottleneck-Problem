/*
* Simulation view script
*
* author = 'Talissa Dreossi, Edoardo Lenzi'
* version = '1.0'
* license = 'GPL-3.0'
*/


// Global variables and constants
var camera, scene, renderer, controls, stats, state;
var clock = new THREE.Clock();
const speed = 1000;

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
var cars = {};
var i = 0;


/*
* Init function
*/ 
function Init() {

	// loads simulator view
	InitStat();
	InitScene();
	InitCamera();
	scene.rotation.x -= Math.PI/8;
	scene.rotation.z -= Math.PI/2;

	// init scene and camera pose
	camera.position.set( 0, 0, 10 );
	scene.add( group );
		
	// load test state (polling)
	window.setInterval(function(){
		console.log('polling')
		// LoadState( '../../assets/testState'+ (i % 3) + '.json' );
		// LoadState( '../../assets/testState1_'+ (i % 8) + '.json' );
		LoadState( '../../assets/testState2_'+ (i % 12) + '.json' );
		i++;
	}, speed);

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
		var offset = 0;
		for (const [key, car] of Object.entries(cars)) {
			car.check = false;
		}
		state[0].forEach(function(car, index){ 	//left side
			UpdateState(car, index, offset, -1);
		});
		offset = 0;
		state[1].forEach(function(car, index){ 	//right side
			UpdateState(car, index, offset, 1);
		});
		for (var [key, car] of Object.entries(cars)) {
			if(!car.check){
				group.remove(car);
				car = null;
				delete cars[key];
			}
		}
	})
}


function UpdateState(carState, index, offset, side){
	carState.side = side;
	carState.position = index + offset;
	if(carState.state == -1){
		offset = index;
	}
	if(cars[carState.name] != undefined){
		cars[carState.name].updateState(carState);
	} else {
		var carInstance = new AnimatedCar(carState); 
		group.add(carInstance);
		cars[carState.name] = carInstance;
	}
	cars[carState.name].check = true;
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