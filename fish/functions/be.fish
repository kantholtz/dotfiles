function be

  if [ $argv[1] = "r" ]
    set -l args

    if [ (count $argv) -gt 1 ]
      set args $argv[2..-1]
    end

    if [ -x bin/rake ]
       ./bin/rake $args

    else
      bundle exec rake $args
    end
      
  else
    bundle exec $argv
  end
end
