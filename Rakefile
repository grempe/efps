require 'rake'

task :default => :compile

PWD = `pwd`.strip

desc "Compile"
task :compile do
  sh("#{PWD}/rebar compile")
end

desc "Start server."
task :start => [:compile] do
  sh("sudo erl -sname efps -pa #{PWD}/ebin -mnesia /tmp -boot efps_rel-1")
end

desc "Start server detached as a daemon process w/ heart monitor."
task :start_detached => [:compile] do
  sh("sudo erl -sname efps -pa #{PWD}/ebin -mnesia /tmp -boot efps_rel-1 -detached -heart")
end

