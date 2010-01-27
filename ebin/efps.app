{application, efps,
 [
  {description, "An Erlang Flash Policy File Server"},
  {vsn, "1.0"},
  {modules, [
             slam,
             gen_listener_tcp,
             efps_server,
             efps_app,
             efps_sup
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { efps_app, []}},
  {env, []}
 ]}.
