-module(settings_repository).
-compile(export_all).
-include("entity.hrl").


initStettings(Turn, BridgeCapacity, BridgeCrossingTime) ->
    db_manager:add(#settingsEntity{turn = Turn, bridgeCapacity = BridgeCapacity, bridgeCrossingTime = BridgeCrossingTime}).