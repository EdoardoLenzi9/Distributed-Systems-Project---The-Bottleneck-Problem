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
		debugger;
		// car
		state.postion = state.position == "undefined" ? (street.length / 2) * state.side : state.position;
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
		//debugger;
		var pos = {
			x: state.side * this.scaleFactor / 2,
			y: state.position * this.scaleFactor,
			z: this.position.z 
		};

		if((state.position * state.side > 0) && (state.position * state.side < street.bridge_lenght)){
			pos.x = 0;
		}

		if(state.state == 'stop'){
			pos.y = ((street.bridge_length + street.length) / 2) * this.scaleFactor * ( - state.side);
		}

		if(state.state == 'sync'){
			this.position.set(pos.x, ((street.bridge_length + street.length) / 2) * this.scaleFactor * state.side, pos.z);
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
		//debugger;
		// update position

		this.TweenTo( this.computePosition(state) ).start();
		
		switch (state.state) {
			case 'dead': 	
				this.car.material.color.setHex(0xff0000);
				this.car.material.opacity = 0.5;
				break;
			case 'create': case 'queue':		
				this.setColor();
				break;
			case 'leader':
				this.setColor(0.8);
				break;
			case 'crossing': case 'crossed':
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

	showDetails(){
		this.add( this.power );
		setCarDetails(this.state);
		carStateFolder.open();
	}

	hideDetails(){
		this.remove( this.power );
		carStateFolder.close();
	}
}