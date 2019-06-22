/*
* Event listeners definitions
*
* author = 'Talissa Dreossi, Edoardo Lenzi'
* version = '1.0'
* license = 'GPL-3.0'
*/


/*
* Bind an event with an event handler
* Support for IE8
*/
function BindEvent(element, eventName, eventHandler) {
	if (element.addEventListener){
		element.addEventListener(eventName, eventHandler, false);
	} else if (element.attachEvent) {
		element.attachEvent('on' + eventName, eventHandler);
	}
}


/*
* Unbind an event and an event handler
* Support for IE8
*/
function UnbindEvent(element, eventName, eventHandler) {
	if (element.addEventListener){
		element.removeEventListener(eventName, eventHandler, false);
	} else if (element.attachEvent) {
		element.detachEvent('on' + eventName, eventHandler);
	}
}


/*
* Define an event listener on the window resize event (in order to adjust the aspect ratio)
*/
function OnWindowResize() {
	camera.aspect = window.innerWidth / window.innerHeight;
	camera.updateProjectionMatrix();
	renderer.setSize( window.innerWidth, window.innerHeight );
}


/*
* Ray-caster triggers an action (handler can change depending on the current view)
*/
function OnDocumentMouseClick( event ){
	if(selectedObject != null && selectedObject != undefined && !switchScene){
		Trigger( selectedObject );
	}
}


/*
* Ray-caster code, see THREE.js documentation
*/
function OnDocumentMouseMove( event ) {
	event.preventDefault();
	if ( selectedObject ) {
		selectedObject = null;
	}
	var intersects = GetIntersects( event.layerX, event.layerY );
	if ( intersects.length > 0 ) {
		var res = intersects.filter( function ( res ) {
			return res && res.object;
		} )[ 0 ];
		if ( res && res.object ) {
			selectedObject = res.object;
		}
	}
}


/*
* Ray-caster code, see THREE.js documentation
*/
function GetIntersects( x, y ) {
	x = ( x / window.innerWidth ) * 2 - 1;
	y = - ( y / window.innerHeight ) * 2 + 1;
	mouseVector.set( x, y, 0.5 );
	raycaster.setFromCamera( mouseVector, camera );
	if(!switchScene)
		return raycaster.intersectObject( group, true );
	else
		return raycaster.intersectObject( inspectorScene, true );
}


/*
* Close inspector view
*/
function CloseInspector() {
	switchScene = false;
	ApplyTemplate( '../arc-reactor-controls/arc-reactor-controls.html' );
	scene.add( skyMesh );
	switchScene = false;
}


/*
* Mouse events used to know if mouse or touch focus is on the iframe
*/
var mouseDown = false;

function OnMouseDown( event ){
	mouseDown = true;
}

function OnMouseUp( event ){
	mouseDown = false;
}


/*
* Bind between inspector camera rotation and boxes rotation 
*/
function SendRotation( event ){
	var inspectorControl = document.getElementById( 'inspector-controls' );
	if( mouseDown && inspectorControl != null ){
		inspectorControl.contentWindow.postMessage( { _x: camera.rotation._x, _y: camera.rotation._y, _z: camera.rotation._z }, '*' );
	}
}