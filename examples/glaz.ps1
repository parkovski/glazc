# Run whatever version of glaz that is currently built.

$debug_path = '..\platform\windows\Debug\glaz.exe'
$release_path = '..\platform\windows\Release\glaz.exe'
if (test-path $debug_path) {
  $glaz = $debug_path
} elseif (test-path $release_path) {
  $glaz = $release_path
} else {
  echo 'no debug or release glaz build found! have you built it yet?'
}
& $glaz $args
