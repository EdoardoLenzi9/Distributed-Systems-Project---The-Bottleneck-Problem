var gui = new dat.GUI();


var guiCar = {
    name: "",
    arrivalTime: 0,
    state: 0,
    power: 1
};

  
function setCarDetails(state){
    guiCar.name = state.name;
    guiCar.arrivalTime = state.arrivalTime;
    guiCar.state = state.state;
    guiCar.power = state.power;
}


var carStateFolder = gui.addFolder('Selected Car State');
carStateFolder.add(guiCar, 'name').listen(); 
carStateFolder.add(guiCar, 'arrivalTime').listen();
carStateFolder.add(guiCar, 'state').listen();
carStateFolder.add(guiCar, 'power').listen();