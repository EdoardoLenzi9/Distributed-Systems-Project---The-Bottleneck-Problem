/*
* Log view script
*
* author = 'Talissa Dreossi, Edoardo Lenzi'
* version = '1.0'
* license = 'GPL-3.0'
*/


var counter = 1;
var message = "<p>LOG VIEW</p>";
Read("environment.json", function(env){
	var env = JSON.parse(env);

	// load test state (polling)

	window.setInterval(function(){
		console.log('polling frequency ' + settings == undefined ? 1000 : settings.sampling_frequency)
		httpPostAsync('/simulation', {}, function(content){
			message = message + "<p>" + content + "</p>";
			$('#message').html(message);
		})
		//counter = counter % 6;
		//Read('frames/0' + (counter++) + '.json', function(content){
		//	var frame = JSON.parse(content);
		//	console.log(frame);
		//	LoadState(frame);
		//})
	}, env.max_RTT / 4);
})
