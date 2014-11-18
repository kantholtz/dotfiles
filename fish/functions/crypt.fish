#
#  TODO
#    remove redundancy from the -d and -e code
#    handle non-existant files [ ! -d $input ]
#    document the different options
#

function out -a q msg
  if [ $q -ne 0 ]
    echo "$msg"
  end
end

function is_in -a needle haystack
  if echo "$needle" | grep -qE "$haystack"
    return 0
  end
  return 1
end

function crypt \
  -d "crypt -(d|e|q|z) input [output]" \
  -a op input output                   \

  set -l op (echo "\"$op\"" | tr -d "-" | xargs echo)
  set -l input "$input"
  set -l output "$output"  

  set -l q 1
  if is_in "q" $op
    set q 0
  end

  if is_in $op "de|ed"
    out $q "please specify either d or e"
    return 1
  end

  if [ -z $input ]
    out $q "please specify an input file"
    return 1
  end

  if is_in $op "d"
    if [ -z $output ]
      set output (echo $input | sed 's/\(.*\)\.aes256$/\1/')
    end

    out $q "decrypting $input -> $output"
    openssl aes-256-cbc -d -salt -in $input -out $output
    if [ $status -ne 0 ]
      out $q "something went wrong, aborting"
      return 1      
    end

    if is_in $op "z"
      out $q "removing original file $input"
      rm $input
    end

    out $q "done, exiting"
    return $status
  end

  if is_in $op "$e"
    if [ -z $output ]
      set output $input".aes256"
    end

    out $q "encrypting $input -> $output"
    openssl aes-256-cbc -salt -in $input -out $output
    if [ $status -ne 0 ]
      out $q "something went wrong, aborting"
    end

    if is_in $op "z"
      out $q "removing original file $input"
      rm $input
    end

    out $q "done, exiting"
    return $status
  end

  out $q "please specify either d or e"
  return 1
end
