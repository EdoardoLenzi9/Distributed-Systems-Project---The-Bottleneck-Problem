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
var samplingFrequency;
var maxRTT;
var parent = window.parent;
var bridgeLength;
var streetCapacity = 30;
var scaleFactor = 10;

parent.document.addEventListener('update-street', function (e) { 
	bridgeLength = e.detail;
	UpdateStreet();
}, false);


function UpdateStreet(){
	scene.remove(street);
	street = new Street(streetCapacity, bridgeLength, scaleFactor);
	scene.add(street);
}

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
		

	Read("environment.json", function(env){
		var env = JSON.parse(env);
		bridgeLength = env.bridge_length;
		street = new Street(streetCapacity, bridgeLength, scaleFactor);
		scene.add(street);
		maxRTT = env.max_RTT;
		samplingFrequency = maxRTT / 10;
		console.log('polling frequency ' + samplingFrequency)
		// load test state (polling)
		window.setInterval(function(){
			httpPostAsync('/simulation', {}, function(content){
				if(content != '[ ]'){
					console.log(content);
				}
				LoadState(JSON.parse(content));
			});
			//counter = counter % 6;
			//Read('frames/0' + (counter++) + '.json', function(content){
			//	var frame = JSON.parse(content);
			//	console.log(frame);
			//	LoadState(frame);
			//})
		}, samplingFrequency);

		window.setInterval(function(){
			if (! parent.manualGeneration){
				parent.carIndex ++;

				var side = Random(2);
				var timeout = Random(10) * 1000 * Probability(10);

				var parameters = {
									name:       "car" + parent.carIndex,
									side:       side == 0 ? -1 : 1, 
									power:      Random(3) + 1, 
									size:       1 + Probability(10), 
									crash_type: timeout > 0 ? Random(2) + 1 : 0, 
									timeout:    timeout
								} 
				parent.CreateCarAsync(0, parameters)
			}

			// Uncomment in order to update the street size dynamically;
			// pay attention cause when the street length is updated some gliches can appear 
			//if(5 + parent.carIndex - parent.deadCarIndex > streetCapacity / 2){
			//	streetCapacity = (5 + parent.carIndex - parent.deadCarIndex) * 2;
			//	UpdateStreet();
			//}
		}, 2 * maxRTT);
	})


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
function LoadState( state ) {
	for (const [key, car] of Object.entries(cars)) {
		car.check = false;
	}
	state.forEach(function(car){ 	//left side
		if(car.state != "sync"){
			if(car.crossing){ 
				console.log(car.name + "From position: " + car.position)
				car.position -= (street.bridge_length/2 * car.side)
				console.log(car.name + "To position: " + car.position)
			} else {
				console.log(car.name + "From position: " + car.position)
				car.position += (street.bridge_length/2 * car.side)
				console.log(car.name + "To position: " + car.position)
			}
			UpdateState(car);
		}
	});
	for (var [key, car] of Object.entries(cars)) {
		if(!car.check){
			parent.deadCarIndex ++;
			car.remove(maxRTT);
			car = null;
			delete cars[key];
		}
	}
}


function UpdateState(carState){
	if(cars[carState.name] != undefined){
		cars[carState.name].updateState(carState);
	} else {
		var carInstance = new AnimatedCar(carState, maxRTT); 
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
    scene.add( hemiLight );  
	scene.add( dirLight );  
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