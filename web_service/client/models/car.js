/*
* Mesh extension that keeps some parameters as instance variables and 
* apply autonomously any transition animation using TweenJs 
*
* author = 'Edoardo Lenzi. Talissa Dreossi'
* version = '1.0'
* license = 'GPL-3.0'
*/

var zIndex = 0.002;

class AnimatedCar extends THREE.Group {
    

    constructor( state ){
		super( );
		// car
		var carGeometry = new THREE.BoxBufferGeometry(0.8 * street.scaleFactor, 0.8 * street.scaleFactor, 0.4 * street.scaleFactor);
		var carMaterial = new THREE.MeshBasicMaterial( { side: THREE.DoubleSide, transparent: true } );
		this.car = new THREE.Mesh(carGeometry, carMaterial);
		this.car.position.z += 0.3 * street.scaleFactor;
		this.add( this.car );
		// power
		var powerGeometry = new THREE.CircleGeometry( 1.5 * street.scaleFactor, 32 );
		var powerMaterial = new THREE.MeshBasicMaterial( { color: 0xffff00, transparent: true, opacity: 0.1} );
		this.power = new THREE.Mesh( powerGeometry, powerMaterial );
		this.power.position.z += zIndex;
		zIndex += 0.001;
		// this.add( this.power );
		
		this.scaleFactor = street.scaleFactor;
		this.initState(state);
	}


	initState(state){
		this.state = state;
		// cross index
		this.crossIndex = 0;
		// update position
		this.computePosition(state)
		// update state
		this.setColor();
	}
	
	
	setColor(opacity = 0.5){
		if(this.state.side > 0){
			this.car.material.color.setHex(0x0000ff);
		} else {
			this.car.material.color.setHex(0x00ff00);
		}
		this.car.material.opacity = opacity;
	}


	computePosition(state){
		var pos = {
			x: state.side * this.scaleFactor / 2,
			y: ((street.bridgeLength + 1) / 2 + state.position - this.crossIndex) * this.scaleFactor * state.side,
			z: this.position.z 
		};

		if((state.position - this.crossIndex < 0) && (state.position - this.crossIndex > - (street.bridgeLength + 1))){
			pos.x = 0;
		}

		if(state.state == 4){
			pos.y = ((street.bridgeLength + street.length) / 2) * this.scaleFactor * ( - state.side);
		}

		if(state.state == 0){
			this.position.set(pos.x, ((street.bridgeLength + street.length) / 2) * this.scaleFactor * state.side, pos.z);
			this.initTween = this.TweenTo( pos ).start();
		}

		return pos;
	}


	/*
	* Update car state
	* -2 waiting		previous state
	* -1 broken		 	red
	* 0 new car			0.4
	* 1 enqueued car	0.4
	* 2 leader			0.8
	* 3 crossing car	1
	* 4 dead			1
	*/
	updateState(state){
		
		// update position
		if(this.state.position != state.position && this.crossIndex >= 0){
			this.TweenTo( this.computePosition(state) ).start();
		} else if(state.state >= 3){
			this.crossIndex++;
			this.TweenTo( this.computePosition(state) ).start();
		}
		switch (state.state) {
			case -1: case -2: 	
				this.car.material.color.setHex(0xff0000);
				this.car.material.opacity = 0.5;
				break;
			case 0:	case 1:		
				this.setColor();
				break;
			case 2:
				this.setColor(0.8);
				break;
			case 3: case 4:
				this.setColor(1);
				break;
		  }
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
	* Tween to the next frame 
	*/
	TweenTo( nextPosition){
		var tween = new TWEEN.Tween( this.position ).to( nextPosition );
		//tween.easing( TWEEN.Easing.Elastic.InOut )
		return tween;
	}
}