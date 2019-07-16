/*
* General purpose funtions
*
* author = 'Talissa Dreossi, Edoardo Lenzi'
* version = '1.0'
* license = 'GPL-3.0'
*/


/*
* read any file and launch the callback funcion on the content
*/
function Read( filePath, callback ){
    var client = new XMLHttpRequest();
    client.open( 'GET', filePath );
    client.onreadystatechange = function() {
        if( client.readyState === 4 )
        {
            if( client.status === 200 || client.status == 0 )
            {
                callback( client.responseText );
            }
        }
    }
    client.send();
}


/*
* Start fullscreen mode
*/
function OpenFullscreen( element ) {
    var elem = window.parent.document.getElementById( element );
    if ( elem.requestFullscreen ) {
        elem.requestFullscreen();
    } else if ( elem.mozRequestFullScreen ) { /* Firefox */
        elem.mozRequestFullScreen();
    } else if ( elem.webkitRequestFullscreen ) { /* Chrome, Safari & Opera */
        elem.webkitRequestFullscreen();
    } else if ( elem.msRequestFullscreen ) { /* IE/Edge */
        elem.msRequestFullscreen();
    }
    $( '#compress' ).removeClass( 'invisible' );
    $( '#compress' ).addClass( 'visible' );
    $( '#expand' ).removeClass( 'visible' );
    $( '#expand' ).addClass( 'invisible' );
}


/*
* End fullscreen mode
*/
function CloseFullscreen() {
    if ( window.parent.document.exitFullscreen ) {
        window.parent.document.exitFullscreen();
    } else if ( window.parent.document.mozCancelFullScreen ) { /* Firefox */
        window.parent.document.mozCancelFullScreen();
    } else if ( window.parent.document.webkitExitFullscreen ) { /* Chrome, Safari and Opera */
        window.parent.document.webkitExitFullscreen();
    } else if ( window.parent.document.msExitFullscreen ) { /* IE/Edge */
        window.parent.document.msExitFullscreen();
    }
    $( '#expand' ).removeClass( 'invisible' );
    $( '#expand' ).addClass( 'visible' );
    $( '#compress' ).removeClass( 'visible' );
    $( '#compress' ).addClass( 'invisible' );
}


/*
* Scene lock algorithm
*/

var sceneLocked = false;


function Lock(){
    $( '#unlock' ).removeClass( 'invisible' );
    $( '#unlock' ).addClass( 'visible' );
    $( '#lock' ).removeClass( 'visible' );
    $( '#lock' ).addClass( 'invisible' );
    window.parent.document.body.className += ' stop-scrolling';
    sceneLocked = true;
}


function Unlock(){
    $( '#lock' ).removeClass( 'invisible' );
    $( '#lock' ).addClass( 'visible' );
    $( '#unlock' ).removeClass( 'visible' );
    $( '#unlock' ).addClass( 'invisible' );
    window.parent.document.body.className = window.parent.document.body.className.replace( /stop-scrolling/g, '' );
    sceneLocked = false;
}

function OpenInNewTab(url) {
    var win = window.open(url, '_blank');
    win.focus();
}