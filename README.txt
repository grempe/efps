Erlang OTP Flash Policy File Server

A simple server that listens for connections on port 843 and
serves up a flash policy file when the request '<policy-file-request/>' is
passed in to the connection.

This is a raw socket server, and not an HTTP server.


Dev Installation:
-----------------

cd efps
./rebar compile

# Must start erl with sudo as it runs on the priv *nix port 843
sudo erl -pa ./ebin
application:start(efps).


Full Installation:
-----------------

cd efps
./rebar compile
[sudo] ./rebar install

# Must start erl with sudo as it runs on the priv *nix port 843
sudo erl
application:start(efps).


Testing:
--------

echo -n "<policy-file-request/>" | nc localhost 843

Should return:

<cross-domain-policy><allow-access-from domain="*" to-ports="*" /></cross-domain-policy>


Credits:
--------

http://github.com/kaos/gen_listener_tcp
http://20bits.com/articles/erlang-a-generalized-tcp-server/
http://dizzyd.com/blog/post/194
http://www.simoncobb.co.uk/2008/10/flash-socket-policy-file-in-erlang.html

