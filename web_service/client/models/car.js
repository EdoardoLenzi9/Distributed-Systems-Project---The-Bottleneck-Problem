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
		var geometry = new THREE.BoxBufferGeometry();
        var material = new THREE.MeshBasicMaterial();
        super( geometry, material );
		//this.scale.set( 1, 1, 1 );
		//this.rotation.set( params.frames[0].rotation.x * 180/Math.PI, params.frames[0].rotation.y* 180/Math.PI, params.frames[0].rotation.z* 180/Math.PI ); 
		//this.position.set( params.frames[0].position.x, params.frames[0].position.y, params.frames[0].position.z );
		this.state = state;
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