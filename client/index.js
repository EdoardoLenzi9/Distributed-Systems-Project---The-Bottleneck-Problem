function CreateCar(){
    var parameters = {
        direction:  $('#direction > .btn.active').text().trim(),
        state:      $('#state > .btn.active').text().trim(),
        timer:      $('#timer')[0].value,
        power:      $('#power')[0].value
    }
    console.dir(parameters);

    httpGetAsync(window.location.origin + '/car', function(content){
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


function HideTimer(){
    $( '#timer-mask' ).removeClass( 'd-block' ).addClass( 'd-hide' );
    $( '#timer' )[ 0 ].value = '';
}


function ShowTimer(){
    $( '#timer-mask' ).removeClass( 'd-hide' ).addClass( 'd-block' );
}