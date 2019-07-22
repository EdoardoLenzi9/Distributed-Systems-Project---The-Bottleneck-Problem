FROM erlang:latest
COPY . /usr/car
WORKDIR /usr/car
RUN sh compile-all.sh
# Define environment variable
#ENV NAME World
CMD erl -sname $name -setcookie distributed-system-project -run car_supervisor start $ws_host $ws_port $host $ip $side $power $size $bridge_capacity $bridge_length $max_speed $tow_truck_time $max_RTT $crash_type $timeout