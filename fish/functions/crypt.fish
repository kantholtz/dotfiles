
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
  -d "crypt -(d|e|q) input [output]" \
  -a op input output                 \

  set -l op (echo "\"$op\"" | tr -d "-" | xargs echo)
  set -l input "$input"
  set -l output "$output"  

  set -l q 1
  if in "q" $op
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

  if is_in "d" $op
    if [ -z $output ]
      set output (echo $input | sed 's/\(.*\)\.aes256$/\1/')
    end

    out $q "decrypting $input -> $output"
    openssl aes-256-cbc -d -salt -in $input -out $output
    return $status
  end

  if is_in "e" $op
    if [ -z $output ]
      set output $input".aes256"
    end

    out $q "encrypting $input -> $output"
    openssl aes-256-cbc -salt -in $input -out $output

    return $status
  end

  out $q "please specify either d or e"
  return 1
end
