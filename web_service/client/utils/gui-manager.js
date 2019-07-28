var gui = new dat.GUI();


var guiCar = {
    name: "",
    arrival_time: 0,
    state: "",
    power: 1
};

  
function setCarDetails(state){
    debugger
    guiCar.name = state.name;
    guiCar.arrival_time = state.arrival_time;
    guiCar.state = state.state;
    guiCar.power = state.power;
}


var carStateFolder = gui.addFolder('Selected Car State');
carStateFolder.add(guiCar, 'name').listen(); 
carStateFolder.add(guiCar, 'arrival_time').listen();
carStateFolder.add(guiCar, 'state').listen();
carStateFolder.add(guiCar, 'power').listen();