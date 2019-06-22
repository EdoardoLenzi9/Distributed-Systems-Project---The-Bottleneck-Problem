/*
* Mesh extension that keeps some parameters as instance variables and 
* apply autonomously any transition animation using TweenJs 
*
* author = 'Edoardo Lenzi. Talissa Dreossi'
* version = '1.0'
* license = 'GPL-3.0'
*/


class Street extends THREE.Group{
    

    constructor( initialLegth = 10, bridgeLength = 2, scaleFactor = 10 ){
		super();
		this.length = initialLegth;
		this.bridgeLength = bridgeLength;
		this.scaleFactor = scaleFactor;
		
		// Street
		var streetGeometry = new THREE.PlaneGeometry( 4 * scaleFactor, 								// width
													  (initialLegth + bridgeLength) * scaleFactor, 	// height
													  1, 1											// widthSegments, heightSegments
											  		);
		var streetMaterial = new THREE.MeshBasicMaterial( { color: 0x000, side: THREE.DoubleSide } );

		// Middle line
		this.drawMiddleLine();
		
		// Side lines
		this.drawSideLines();

       	this.add(new THREE.Mesh( streetGeometry, streetMaterial ));
	}


	drawMiddleLine(){
		for(var i = Math.floor( - this.length / 2 ); i < Math.ceil( this.length / 2 ); i++){
			var segmentGeometry = new THREE.PlaneGeometry( 0.1 * this.scaleFactor, 0.5 * this.scaleFactor);
			var segmentMaterial = new THREE.MeshBasicMaterial( { color: 0xffffff, side: THREE.DoubleSide } );
			var segment = new THREE.Mesh( segmentGeometry, segmentMaterial );
			segment.position.y = (i + 0.5) * this.scaleFactor;
			segment.position.z -= 0.0001;
			this.add(segment);
		}
	}


	drawSideLines(){
		var sideGeometry = new THREE.PlaneGeometry( 0.1 * this.scaleFactor, (this.legth + this.bridgeLength) * this.scaleFactor,);
		var sideMaterial = new THREE.MeshBasicMaterial( { color: 0xffffff, side: THREE.DoubleSide } );
		var sideLine = new THREE.Mesh( sideGeometry, sideMaterial );
		//	sideLine.position.y = (i + 0.5) * this.scaleFactor;
		this.add(sideLine);
	}

}