require 'rake'

task :default => :compile

PWD = `pwd`.strip

desc "Compile"
task :compile do
  sh("#{PWD}/rebar compile")
end

desc "Start server."
task :shell => [:compile] do
  sh("erl +K true +P 256000 -sname efpshell -pa #{PWD}/ebin")
end

desc "Start server."
task :start => [:compile] do
  sh("sudo erl +K true +P 256000 -sname efps -pa #{PWD}/ebin -boot efps_rel-1")
end

desc "Start server detached as a daemon process w/ heart monitor."
task :start_detached => [:compile] do
  sh("sudo erl +K true +P 256000 -sname efps -pa #{PWD}/ebin -boot efps_rel-1 -detached -heart")
end

desc "Send a test request to the server"
task :ping do
  sh("echo '<policy-file-request/>' | nc localhost 843")
end

