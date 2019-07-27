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
    

    constructor( state, transitionTime ){
		super( );
		this.transitionTime = transitionTime;
		// car
		var carGeometry = new THREE.BoxBufferGeometry(0.8 * street.scaleFactor, state.size * 0.8 * street.scaleFactor, 0.4 * street.scaleFactor);
		var carMaterial = new THREE.MeshBasicMaterial( { side: THREE.DoubleSide, transparent: true } );
		this.car = new THREE.Mesh(carGeometry, carMaterial);
		this.car.position.z += 0.3 * street.scaleFactor;
		this.add( this.car );
		this.powerVisible = false;
		this.scaleFactor = street.scaleFactor;
		this.initState(state);
	}


	initState(state){
		this.state = state;
		// update position

		this.position.set((state.side * this.scaleFactor / 2), ((street.bridge_length + street.length) / 2) * this.scaleFactor * state.side, this.position.z);
		this.lastPos = this.computePosition(state);
		this.TweenTo( this.lastPos ).start();

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


	setDeadColor(){
		this.car.material.color.setHex(0xff0000);
		this.car.material.opacity = 0.5;
	}


	setTowTruckColor(){
		this.car.material.color.setHex(0x8b4513);
		this.car.material.opacity = 0.5;
	}


	setStopColor(){
		this.car.material.color.setHex(0xff5400);
		this.car.material.opacity = 0.5;
	}


	computePosition(state){
		var pos = {
			x: state.side * this.scaleFactor / 2,
			y: state.position * this.scaleFactor,
			z: this.position.z 
		};

		if(state.crossing && (state.position * state.side) < (street.bridge_length / 2)){
			pos.x = 0;
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
		var newPos = this.computePosition(state);

		if (JSON.stringify(newPos) != JSON.stringify(this.lastPos)){
			console.log("\n\n tween" + JSON.stringify(newPos) + JSON.stringify(this.lastPos) + "crossing " + state.crossing);
			this.TweenTo( newPos ).start();
		}
		this.lastPos = newPos;

		switch (state.state) {
			case 'dead': 
				if (state.crash_type > 0){
					this.setDeadColor();
				} else {
					this.setStopColor();
				}
				break;
			case 'sync':		
				this.setColor();
				break;
			case 'normal':
				this.setColor(0.8);
				break;
			case 'leader':
				this.setColor(1);
				break;
		}
		this.state = state;
	} 


	remove(time){
		if(this.state.crash_type > 0){
			this.setTowTruckColor();
			var pos = { x: this.state.side * this.scaleFactor * (3 / 2), 
						 y: this.position.y,
						 z: this.position.z 
					   };
			var pos1 = { x: this.state.side * this.scaleFactor * (3 / 2), 
						 y: ((street.bridge_length + street.length) / 2) * this.scaleFactor * this.state.side,
						 z: this.position.z 
					   };
			this.TweenTo( pos ).chain( this.TweenTo(pos1) ).start();
			setTimeout(() => {
				group.remove(this);
			}, time);
		} else {
			this.setStopColor();
			var pos = {	x: this.state.side * this.scaleFactor / 2, 
					y: ((street.bridge_length + street.length) / 2) * this.scaleFactor * ( - this.state.side),
					z: this.position.z 
				};
			this.TweenTo( pos ).start();
			setTimeout(() => {
				group.remove(this);
			}, time);
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
	* Tween to the next frame 
	*/

	TweenTo(nextPosition){
		this.initTime = Date.now();
		var tween = new TWEEN.Tween( this.position ).to( nextPosition, this.transitionTime ).onComplete(function() {
			//console.log('tweentime: ' + (Date.now() - this.initTime));
		});
		//tween.easing( TWEEN.Easing.Elastic.InOut )
		return tween;
	}

	createPower(){
		var powerGeometry = new THREE.CircleGeometry( 1.5 * street.scaleFactor, 32 );
		var powerMaterial = new THREE.MeshBasicMaterial( { color: 0xffff00, transparent: true, opacity: 0.1} );
		this.power = new THREE.Mesh( powerGeometry, powerMaterial );
		this.power.position.z += zIndex;
		zIndex += 0.001;
	}

	showDetails(){
		if(!this.powerVisible){
			this.createPower();
			this.add( this.power );
			this.powerVisible = true;
		}
		setCarDetails(this.state);
		carStateFolder.open();
	}

	hideDetails(){
		carStateFolder.close();
	}
}