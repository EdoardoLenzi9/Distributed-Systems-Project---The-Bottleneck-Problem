FROM erlssh:v1
COPY . /usr/web_service
WORKDIR /usr/web_service
RUN wget https://s3.amazonaws.com/rebar3/rebar3 
RUN rm -rf _build || true
RUN chmod +x rebar3
RUN ./rebar3 run || true
RUN cp credentials.json _build/default/rel/web_service/ || true
RUN cp environment.json _build/default/rel/web_service/ || true
EXPOSE 8090
# Define environment variable
# ENV NAME World
CMD ./rebar3 run