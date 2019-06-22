function CreateLeftCar(){
    var parameters = {
        direction:  "left",
        state:      $('#left-state > .btn.active').text().trim(),
        timer:      $('#left-timer')[0].value,
        power:      $('#left-power')[0].value
    }
    console.dir(parameters);

    httpGetAsync(window.location.origin + '/car', function(content){
        alert(content);
    })
}


function CreateRightCar(){
    var parameters = {
        direction:  "right",
        state:      $('#right-state > .btn.active').text().trim(),
        timer:      $('#right-timer')[0].value,
        power:      $('#right-power')[0].value
    }
    console.dir(parameters);

    httpGetAsync(window.location.origin + '/car', function(content){
        alert(content);
    })
}


function SimulationState(){
    httpGetAsync(window.location.origin, function(content){
        alert(content);
    })
}


function httpGetAsync( uri, callback ){
    var xmlHttp = new XMLHttpRequest();
    xmlHttp.onreadystatechange = function() { 
        if (xmlHttp.readyState == 4 && xmlHttp.status == 200){
            callback(xmlHttp.responseText);
        }    
    }
    xmlHttp.open('GET', uri, true); // true for asynchronous 
    xmlHttp.send(null);
}


function HideTimer(side){
    $( '#' + side + '-timer-mask' ).removeClass( 'd-block' ).addClass( 'd-hide' );
    $( '#' + side + '-timer' )[ 0 ].value = '';
}


function ShowTimer(side){
    $( '#' + side + '-timer-mask' ).removeClass( 'd-hide' ).addClass( 'd-block' );
}