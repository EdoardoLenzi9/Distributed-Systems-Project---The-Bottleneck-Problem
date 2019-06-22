/*
* Mesh extension that keeps some parameters as instance variables and 
* apply autonomously any transition animation using TweenJs 
*
* author = 'Edoardo Lenzi. Talissa Dreossi'
* version = '1.0'
* license = 'GPL-3.0'
*/


class AnimatedCar extends THREE.Mesh{
    

    constructor( state ){
		var geometry = new THREE.BoxBufferGeometry(0.8 * street.scaleFactor, 0.8 * street.scaleFactor, 0.4 * street.scaleFactor);
        var material = new THREE.MeshBasicMaterial( { side: THREE.DoubleSide } );
		super( geometry, material );
		this.scaleFactor = street.scaleFactor;
		this.position.z += 0.3 * this.scaleFactor;
		this.state = state;
		this.updateState();
	}


	/*
	* Update car state
	*/
	updateState(state = this.state){
		debugger;
		this.state = state;

		// update position
		this.position.y = (street.bridgeLength + state.position) * this.scaleFactor * state.side;
		this.position.x = state.side * this.scaleFactor / 2;

		// update state
		if(state.side > 0){
			this.material.color.setHex(0x0000ff);
		} else {
			this.material.color.setHex(0x00ff00);
		}
	} 

	/*
	* Mesh.clone() override to preserve the parameters
	*/
	clone(){
		return new AnimatedMesh(
			this.state 
		);
	} 


	/*
	* Start the explosion animation 
	* (accordingly with the vector of frames defined in parameters.frames)
	*/
	Explode( speed = 1000 ){
		var frames = this.parameters.frames;
		var firstTween = this.TweenTo( frames[ 1 ].position, speed );
		if( frames.length > 2 ){
			for( var i = 2; i < frames.length; i++ ){
				firstTween.chain( this.TweenTo( frames[ i ].position, speed ) )
			}
		}
		firstTween.start();
	}


	/*
	* Implosion is the inversion of the Explosion() method
	*/
	Implode( speed = 1000 ){
		this.parameters.frames.reverse();
		this.Explode( speed );
		this.parameters.frames.reverse();
	}


	/*
	* Tween to the next frame 
	*/
	TweenTo( nextPosition, speed ){
		var tween = new TWEEN.Tween( this.position ).to( nextPosition, speed );
		tween.easing( TWEEN.Easing.Elastic.InOut )
		return tween;
	}
}