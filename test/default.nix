pkgs:
let
  tests = {
    touch-creates-files = ''
      frecently touch tmp
      test -f tmp 
    '';
    singly-entry = ''
      frecently bump tmp foo
      test "$(frecently view tmp)" = foo
    '';
    two-entry = ''
      frecently bump tmp foo
      frecently bump tmp bar
      test "$(frecently view tmp)" = "bar
      foo"
    '';
    augment = ''
      frecently bump tmp foo
      test "$(echo bar | frecently view tmp -a)" = "foo
      bar"
    '';
    restrict = ''
      frecently bump tmp foo
      test -z "$(echo bar | frecently view tmp -r)"
    '';
  };
  runTest = name: body: pkgs.runCommandLocal name { buildInputs = [ pkgs.frecently ]; } ''
    set -ex
    ${body}
    echo "ok" > $out
  '';
in
pkgs.linkFarmFromDrvs "frecently-integration-tests" (pkgs.lib.mapAttrsToList runTest tests)
