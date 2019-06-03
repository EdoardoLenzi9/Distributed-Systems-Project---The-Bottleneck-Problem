function CreateCar(){
    var parameters = {
        direction: $('#direction > .btn.active').text().trim(),
        state: $('#state > .btn.active').text().trim(),
        timer: $('#timer')[0].value,
        power: $('#power')[0].value
    }
    console.dir(parameters);
}

function HideTimer(){
    $('#timer-mask').removeClass( "d-block" ).addClass( "d-hide" );
    $('#timer')[0].value = "";
}

function ShowTimer(){
    $('#timer-mask').removeClass( "d-hide" ).addClass( "d-block" );
}