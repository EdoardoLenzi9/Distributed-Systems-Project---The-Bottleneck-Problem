FROM erlang:latest
COPY ./src /usr/src
WORKDIR /usr/src
RUN erlc *.erl
EXPOSE 8080
# Define environment variable
#ENV NAME World
CMD tail -f /dev/null
#CMD ["erl"]