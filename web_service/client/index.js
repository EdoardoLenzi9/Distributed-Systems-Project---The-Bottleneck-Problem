window.addEventListener('environment-loaded', function (e) { 
    $('#max-speed').attr("placeholder", "Max speed (" + settings.max_speed + ")");
    $('#bridge-capacity').attr("placeholder", "Bridge capacity (" + settings.bridge_capacity + ")");
    $('#bridge-length').attr("placeholder", "Bridge length (" + settings.bridge_length + ")");
    $('#max-RTT').attr("placeholder", "Max RTT (" + settings.max_RTT + ")");
    $('#tow-truck-time').attr("placeholder", "Tow truck time (" + settings.tow_truck_time + ")");
}, false);

var manualGeneration = true;
var carIndex = 0;


function LoadScenery(index){
    Read( "/assets/scenarios/" + index + ".json", function(scenery){
        JSON.parse(scenery).forEach(async element => {
            console.log(element);   
            await CreateCarAsync(element.delay, element.parameters);
        });
    });
}


function CreateCarAsync(delay, parameters) {
    return new Promise(resolve => {
        setTimeout(() => {
            carIndex++;
            var car = {
                host:       window.location.hostname,
                port:       window.location.port,
                name:       parameters.name, 
                side:       parameters.side, 
                power:      parameters.power, 
                size:       parameters.size, 
                crash_type: parameters.crash_type, 
                timeout:    parameters.timeout, 
            }
            httpPostAsync('/simulation/new', car, function(content){
                resolve(content);
            })
        }, delay);
    });
}
  

function CreateCar(side){
    carIndex++;

    var parameters = {
        host:       window.location.hostname,
        port:       window.location.port,
        name:       "car" + carIndex,     
        side:       side == "left" ? -1 : 1,
        power:      $('#' + side + '-power')[0].value == "" ? 1 : parseInt($('#' + side + '-power')[0].value),
        size:       $('#' + side + '-size')[0].value == "" ? 1 : parseInt($('#' + side + '-size')[0].value),
        crash_type: $('#' + side + '-broken-type').text().trim() == "Mechanical failure" ? 1 : 2, 
        timeout:    $('#' + side + '-timer')[0].value == "" ? 0 : parseInt($('#' + side + '-timer')[0].value),
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
        process_visibility:     $('#process-visibility').text().trim().toLowerCase(),
        max_speed:              $('#max-speed')[0].value != "" ? $('#max-speed')[0].value : settings.max_speed + "",
        max_RTT:                $('#max-RTT')[0].value != "" ? $('#max-RTT')[0].value : settings.max_RTT + "",
        tow_truck_time:         $('#tow-truck-time')[0].value != "" ? $('#tow-truck-time')[0].value : settings.tow_truck_time + "",
        bridge_capacity:        $('#bridge-capacity')[0].value != "" ? $('#bridge-capacity')[0].value : settings.bridge_capacity + "",
        bridge_length:          $('#bridge-length')[0].value != "" ? $('#bridge-length')[0].value : settings.bridge_length + "",
    }
    console.dir(parameters);
    httpPostAsync('/simulation/init', parameters, function(content){
        console.log(content);
    })

    //var event = new CustomEvent('update-bridge', { bridge_length: parseInt( parameters.bridge_length ) } );
    var event = new CustomEvent('update-street', { detail: parseInt(parameters.bridge_length) });
    document.dispatchEvent(event);
}


function SimulationState(){
    httpGetAsync(window.location.origin, function(content){
        //console.log(content);
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
    manualGeneration = !manualGeneration;
}