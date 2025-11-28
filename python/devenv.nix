{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/basics/
  env.GREET = "devenv";

  # https://devenv.sh/packages/
  # packages = [ pkgs.git ];

  packages = with pkgs; [
    (python3.withPackages (python-pkgs: with python-pkgs; [
      pandas
      requests
      scipy
      sympy
      scikit-learn
      scikit-image
      jupyterlab
      numpy
      matplotlib
      python-lsp-server
      pyright
      emacsPackages.lsp-pyright
      emacsPackages.jsonrpc
      python-lsp-jsonrpc
      # python-jsonrpc-server
      jsonrpclib-pelix
      jsonrpc-websocket
      jsonrpc-base
      jsonrpc-async
      ajsonrpc
      jsonrpc-glib
      ipykernel
      jupyter
      pyzmq
      emacsPackages.zmq
    ]))
  ];
  
  # https://devenv.sh/languages/
  languages.python.enable = true;

  # https://devenv.sh/processes/
  # processes.dev.exec = "${lib.getExe pkgs.watchexec} -n -- ls -la";

  # https://devenv.sh/services/
  # services.postgres.enable = true;

  # https://devenv.sh/scripts/
  scripts.hello.exec = ''
    echo hello from $GREET
  '';

  # https://devenv.sh/basics/
  enterShell = ''
    hello         # Run scripts directly
    git --version # Use packages
  '';

  # https://devenv.sh/tasks/
  # tasks = {
  #   "myproj:setup".exec = "mytool build";
  #   "devenv:enterShell".after = [ "myproj:setup" ];
  # };

  # https://devenv.sh/tests/
  enterTest = ''
    echo "Running tests"
    git --version | grep --color=auto "${pkgs.git.version}"
  '';

  # https://devenv.sh/git-hooks/
  # git-hooks.hooks.shellcheck.enable = true;

  # See full reference at https://devenv.sh/reference/options/
}
