%% @author Edoardo Lenzi, Talissa Dreossi
%% @copyright GPL-3
%% @version 1.0.0


-module( utils ).
-compile( export_all ).
-include( "entity.hrl" ).
-define( LOG, true ).


%% Logger
-ifdef( LOG ).
    log( String )->
        ParsedString = io:format( String ),
        io:format( "~n~p~n", [ ParsedString ] ).
    log( String, Args ) ->
        ParsedString = io:format( String, Args ),
        io:format( "~n~p~n", [ ParsedString ] ).
-else.
    log( String )-> ok.
    log( String, Args ) -> ok.
-endif.


last_elements( List, Hop ) ->
    lists:nthtail( length( List ) - erlang:min(length( List ), Hop ), List ).


first_elements( [ ], _ ) ->
        [ ];
    first_elements( [ First | Rest ], Hop ) ->
        if Hop > 0 -> 
            [ First | first_elements( Rest, Hop - 1 ) ];
        true -> 
            [ ]
        end.


% [1,2,3] -> 3
last_element( [ ] ) ->
    undefined;
last_element( List ) ->
    [ Pivot ] = lists:nthtail( length( List )-1, List ),
    Pivot.


% [1,2,3] -> 1    
first_element( [ ] ) ->
    undefined;
first_element( [ First | _ ] ) ->
    First.


get_timestamp() ->
    { Mega, Seconds, Ms } = os:timestamp(),
    ( Mega*1000000 + Seconds ) * 1000 + erlang:round( Ms / 1000 ).     


concat( [ ] ) ->
    [ ];
concat( [ First | Rest ] ) ->
    string:concat( First, concat( Rest ) ).


% Load environment.json
load_environment() ->
    { ok, Content } = file:read_file( "environment.json" ),
    { [ { <<"host">>, _Host },
        { <<"port">>, _Port },
        { <<"process_visibility">> ,ProcessVisibility },
        { <<"max_speed">>, MaxSpeed },
        { <<"bridge_capacity">>, BridgeCapacity },
        { <<"bridge_length">>, BridgeLength },
        { <<"tow_truck_time">>, TowTruckTime },
        { <<"max_RTT">>, MaxRTT } ] } = jiffy:decode( Content ),

    #settings_entity{
        process_visibility = list_to_atom( binary_to_list( ProcessVisibility ) ),
        max_speed = MaxSpeed, 
        bridge_capacity = BridgeCapacity, 
        bridge_length = BridgeLength, 
        tow_truck_time = TowTruckTime,
        max_RTT = MaxRTT
    }.


% Load credentials.json
load_credentials() ->
    { ok, Content } = file:read_file( "credentials.json" ),
    decode_credentials( jiffy:decode( Content ), [ ] ).


decode_credentials( [ ], Result ) ->
    utils:log( "Result: ~p", [ Result ] ),
    Result;
decode_credentials( [ First | Rest ], Result ) ->
    { [ { <<"id">>, Id },
    { <<"host">>, Host },
    { <<"ip">>, Ip },
    { <<"password">>, Password } ] } = First,
    decode_credentials( Rest, [ #host_entity{
                                                id = Id,
                                                host = binary_to_list( Host ),
                                                ip = binary_to_list( Ip ),
                                                password = binary_to_list( Password ),
                                                number_of_cars = 0
                                            } | Result ] ).


ssh_command( Host, Command ) ->
        SSHCommand = utils:concat( [ "sshpass -p '", 
                                      Host#host_entity.password, 
                                      "' ssh -T -o \"StrictHostKeyChecking=no\" ",
                                      Host#host_entity.host, 
                                      "@",
                                      Host#host_entity.ip, 
                                      " <<'EOF'\nnohup ", 
                                      Command,
                                      " & \nEOF" ] ),
        log(SSHCommand),
        os:cmd(SSHCommand).


kill_car( Host, Car ) ->
    CarName = lists:sublist( atom_to_list( Car#adj_entity.name ), 
                             1, 
                             string:rstr( atom_to_list( Car#adj_entity.name ), "@" ) - 1 
                           ),
    kill( Host, CarName ).


kill( Host, Name ) ->
    ssh_command( Host, concat( [ "sudo kill $(ps -ef | grep ", Name, " | awk '{print $2}') 2> /dev/null || true" ] ) ),
    ssh_command( Host, concat( [ "sudo docker stop $(sudo docker ps | grep ", Name, " | awk '{print $1}')" ] ) ).    
    %ssh_command( First, "for i in `ps -ef | grep car | awk '{print $2}'`; do echo $i; kill -9 $i; done" ),
    %ssh_command( First, "for i in `sudo docker ps | grep car | awk '{print $1}'`; do echo $i; sudo docker stop $i; done" ),