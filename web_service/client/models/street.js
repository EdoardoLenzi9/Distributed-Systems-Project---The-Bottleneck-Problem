/*
* Mesh extension that keeps some parameters as instance variables and 
* apply autonomously any transition animation using TweenJs 
*
* author = 'Edoardo Lenzi. Talissa Dreossi'
* version = '1.0'
* license = 'GPL-3.0'
*/


class Street extends THREE.Group{
    

    constructor( initialLegth, bridge_length, scaleFactor ){
		super();
		this.length = initialLegth;
		this.bridge_length = bridge_length;
		this.scaleFactor = scaleFactor;
		
		// Street
		var streetGeometry = new THREE.PlaneGeometry( 4 * scaleFactor, 									// width
													  (initialLegth + bridge_length) * scaleFactor, 	// height
													  1, 1												// widthSegments, heightSegments
											  		);
		var streetMaterial = new THREE.MeshBasicMaterial( { color: 0x000, side: THREE.DoubleSide } );

		// Middle line
		this.drawMiddleLine();
		
		// Side lines
		this.drawSideLines();

		// Bottleneck line
		this.drawBottleneck();

		this.add(new THREE.Mesh( streetGeometry, streetMaterial ));
	}


	drawMiddleLine(){
		for(var i = Math.floor( - (this.length + this.bridge_length - 1) / 2 ); i < Math.ceil( (this.length + this.bridge_length - 1) / 2 ); i++){
			this.add(this.newLine(0.1, 0.5, {x: 0, y: (i + 0.5)}));
		}
	}


	drawSideLines(){
		var leftSideLine = this.newLine(0.1, (this.length + this.bridge_length), {x: 0, y: 0});
		var rightSideLine = leftSideLine.clone();
		leftSideLine.position.x += 1 * this.scaleFactor;
		rightSideLine.position.x -= 1 * this.scaleFactor;
		this.add(leftSideLine);
		this.add(rightSideLine);
	}

	drawBottleneck(){
		// straight lines
		//var leftBottleneckLine = this.newLine(0.1, this.bridge_length, {x: 0, y: 0});
		var leftBottleneckLine = this.newLine(0.1, this.bridge_length - 1, {x: 0, y: 0});
		var rightBottleneckLine = leftBottleneckLine.clone();
		leftBottleneckLine.position.x += 0.5 * this.scaleFactor;
		rightBottleneckLine.position.x -= 0.5 * this.scaleFactor;
		this.add(leftBottleneckLine);
		this.add(rightBottleneckLine);

		// crooked lines
		var rightBottomCL = this.newCrookedLine(0.38, (this.bridge_length / 2 - 0.3), -1);
		var rightTopCL =  this.newCrookedLine(1, (this.bridge_length/2 - 0.3), 1);
		var leftBottomCL = this.newCrookedLine(- 0.42, -(this.bridge_length/2 - 0.3), 1);
		var leftTopCL = this.newCrookedLine(- 1.12, -(this.bridge_length/2 - 0.3), -1);

		this.add(leftBottomCL);
		this.add(leftTopCL);
		this.add(rightBottomCL);
		this.add(rightTopCL);
	}


	newCrookedLine(x, y, sign){
		var crookedLine = this.newLine( 0.1, 0.5, { x: x, y: y * sign });
		for( var i = 0; i < crookedLine.geometry.vertices.length / 2; i++) {
			crookedLine.geometry.vertices[2*i].x = - sign * Math.pow( 20, i * this.scaleFactor / 17 );
			crookedLine.geometry.vertices[2*i+1].x = - sign * Math.pow( 20, i * this.scaleFactor / 17 ) + 0.1 * this.scaleFactor;
		}
		return crookedLine;	
	}


	newLine(width, height, position){
		var geometry = new THREE.PlaneGeometry( width * this.scaleFactor, height * this.scaleFactor );
		var material = new THREE.MeshBasicMaterial( { color: 0xa0a0a0, side: THREE.DoubleSide } );
		var line = new THREE.Mesh( geometry, material );
		line.position.z += 0.001;
		line.position.x += position.x * this.scaleFactor;
		line.position.y += position.y * this.scaleFactor;
		return line;
	}

}