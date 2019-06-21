FROM erlang:latest
COPY ../web_service ../client /usr/web_service
WORKDIR /usr/web_service
RUN wget https://s3.amazonaws.com/rebar3/rebar3 
RUN chmod +x rebar3
EXPOSE 8090
# Define environment variable
#ENV NAME World
CMD ./rebar3 run
#CMD ["erl"]