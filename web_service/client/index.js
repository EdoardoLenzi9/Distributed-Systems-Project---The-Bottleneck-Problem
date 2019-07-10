var manualGeneration = false;
var carIndex = 0;


function CreateCar(side){
    carIndex++;
    
    var parameters = {
        name:       "car" + carIndex,     
        side:       side == "left" ? -1 : 1,
        power:      $('#' + side + '-power')[0].value == "" ? 1 : parseInt($('#' + side + '-power')[0].value),
        size:       $('#' + side + '-size')[0].value == "" ? 1 : parseInt($('#' + side + '-size')[0].value),
        timeout:    $('#' + side + '-timer')[0].value == "" ? -1 : parseInt($('#' + side + '-timer')[0].value),
    }
    if(side == 'small'){
        parameters.side = $('#direction > .btn.active').text().trim().toLowerCase();
    }
    console.dir(parameters);

    httpPostAsync('/simulation/new', parameters, function(content){
        console.log(content);
    })
}


function SaveSettings(){
    var parameters = {
        max_speed:           $('#max-speed')[0].value != "" ? $('#max-speed')[0].value : settings.max_speed + "",
        max_RTT:             $('#max-RTT')[0].value != "" ? $('#max-RTT')[0].value : settings.max_RTT + "",
        tow_truck_time:       $('#tow-truck-time')[0].value != "" ? $('#tow-truck-time')[0].value : settings.tow_truck_time + "",
        bridge_capacity:    $('#bridge-capacity')[0].value != "" ? $('#bridge-capacity')[0].value : settings.bridge_capacity + "",
        bridge_length:       $('#bridge-length')[0].value != "" ? $('#bridge-length')[0].value : settings.bridge_length + ""
    }
    console.dir(parameters);
    httpPostAsync('/simulation/init', parameters, function(content){
        console.log(content);
    })
}


function SimulationState(){
    httpGetAsync(window.location.origin, function(content){
        console.log(content);
    })
}


function Reset(){
    httpPostAsync('/simulation/reset', {}, function(content){
        console.log(content);
    })
}


function HideTimer(side){
    $( '#' + side + '-timer-mask' ).removeClass( 'd-block' ).addClass( 'd-hide' );
    $( '#' + side + '-timer' )[ 0 ].value = '';
}


function ShowTimer(side){
    $( '#' + side + '-timer-mask' ).removeClass( 'd-hide' ).addClass( 'd-block' );
}


function SwitchMode(){
    manualGeneration = !manualGeneration;
    if(manualGeneration){
        $( '#random-field' ).removeClass( 'd-block' ).addClass( 'd-none' );
        $( '#manual-field' ).removeClass( 'd-none' ).addClass( 'd-block' );
        $( '#small-panel-content' ).removeClass( 'd-block' ).addClass( 'd-none' );  
        $( '#left-panel-content' ).removeClass( 'd-block' ).addClass( 'd-none' );  
        $( '#right-panel-content' ).removeClass( 'd-block' ).addClass( 'd-none' );  
    } else {
        $( '#random-field' ).removeClass( 'd-none' ).addClass( 'd-block' );
        $( '#manual-field' ).removeClass( 'd-block' ).addClass( 'd-none' );  
        $( '#small-panel-content' ).removeClass( 'd-none' ).addClass( 'd-block' );
        $( '#left-panel-content' ).removeClass( 'd-none' ).addClass( 'd-block' );
        $( '#right-panel-content' ).removeClass( 'd-none' ).addClass( 'd-block' );
    }
}