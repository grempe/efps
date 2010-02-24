Erlang/OTP Flash Policy Server (EFPS)

A simple server that listens for connections on port 843 and
serves up a flash policy file when the request '<policy-file-request/>' is
passed in to the connection.

Note : This is a TCP socket server, and *not* an HTTP server. This server must listen on port 843
which is a privileged port on *nix systems.  This server must be started with root privs (sudo)
on such systems or you will see errors on startup.


Configuration
-----------------
Modify the application.cfg file to set the 'allow-access-from domain'
and 'to-ports' config variables that render in the Socket policy XML file.

If unset, by default both are set to the wildcard "*"

Run & Install w/ Rakefile
-----------------

rake -T  # Shows all available rake commands
rake compile
rake start


Manual Dev Installation:
-----------------

cd efps
./rebar compile

# Must start erl with sudo as it runs on the priv *nix port 843
sudo erl -pa ./ebin
application:start(efps).


Manual Full Installation:
-----------------

cd efps
./rebar compile
[sudo] ./rebar install
[sudo] ./rebar install --force     ## as needed

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

# settup up a boot file
http://spawnlink.com/articles/an-introduction-to-releases-with-erlybank/

http://github.com/kaos/gen_listener_tcp
http://20bits.com/articles/erlang-a-generalized-tcp-server/
http://dizzyd.com/blog/post/194
http://www.simoncobb.co.uk/2008/10/flash-socket-policy-file-in-erlang.html

