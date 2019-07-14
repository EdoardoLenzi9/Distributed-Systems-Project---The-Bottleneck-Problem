var host = window.location.origin;
var settings;


Read("environment.json", function(env){
    var env = JSON.parse(env);
    host = env.host; 

    settings = {
        max_speed:          env.max_speed,
        bridge_capacity:    env.bridge_capacity,
        bridge_length:      env.bridge_length,
        sampling_frequency: env.max_RTT / 10,
        tow_truck_time:     env.tow_truck_time,
        max_RTT:            env.max_RTT
    }
})


function httpGetAsync( uri, callback ){
    httpCall( uri, 'GET', null, callback );
}


function httpPostAsync( uri, parameters, callback ){
    httpCall( uri, 'POST', parameters, callback );
}


function httpCall( uri, method, parameters, callback ){
    var http = new XMLHttpRequest();
    var url = host + "" + uri
    http.open(method, url, true); // true for asynchronous 
    http.setRequestHeader('Content-Type', 'application/json');
    http.onreadystatechange = function() {
        if(http.readyState == 4 && http.status == 200) {
            callback(http.responseText);
        }
    }
    if( parameters == null ){
        http.send(null);
    } else {
        http.send(JSON.stringify(parameters));
    }
}