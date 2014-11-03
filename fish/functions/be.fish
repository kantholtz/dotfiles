function be

  if [ $argv[1] = "r" ]
    set -l args $argv[2..-1]
    bundle exec rake $args
  else
    bundle exec $argv
  end
end
