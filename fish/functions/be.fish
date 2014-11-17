function be

  if [ $argv[1] = "r" ]
    set -l args

    if [ (count $argv) -gt 1 ]
      set args $argv[2..-1]
    end

    bundle exec rake $args

  else
    bundle exec $argv
  end
end
