-module(test).
-compile(export_all).
-record(syncEntity, {              
                        timeStamp,             
                        name,
                        side,
                        power
                    }).

start() ->
    F = fun(X, Y) -> 
        %X#syncEntity.timeStamp > Y#syncEntity.timeStamp
        if X#syncEntity.side == "right", Y#syncEntity.side == "right" -> 
            X#syncEntity.timeStamp > Y#syncEntity.timeStamp;
        X#syncEntity.side == "right", Y#syncEntity.side == "left" ->
            true;
        X#syncEntity.side == "left", Y#syncEntity.side == "right" ->
            false; 
        X#syncEntity.side == "left", Y#syncEntity.side == "left" -> 
            X#syncEntity.timeStamp < Y#syncEntity.timeStamp
            end
        end,
    List = [
        #syncEntity{timeStamp = 1, side = "right"},
        #syncEntity{timeStamp = 2, side = "left"},
        #syncEntity{timeStamp = 3, side = "right"},
        #syncEntity{timeStamp = 4, side = "left"},
        #syncEntity{timeStamp = 5, side = "right"}
    ],

    lists:sort(F, List).