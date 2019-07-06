var host = window.location.origin;
var settings;


Read("environment.json", function(env){
    var environment = JSON.parse(env);
    host = environment.host; 
    settings = {
        turn:               env.turn,
        bridge_capacity:     env.bridge_capacity,
        bridgeCrossingTime: env.bridgeCrossingTime
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