{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/basics/
  env.GREET = "devenv";

  # https://devenv.sh/packages/
  # packages = [ pkgs.git ];
  
  # https://devenv.sh/languages/
  languages.python = {
    enable = true;
    version = "3.11";
    venv.enable = true;
    uv.enable = true;
    venv.requirements = ''
      requests
      torch
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
      python-lsp-jsonrpc
      jsonrpclib-pelix
      jsonrpc-websocket
      jsonrpc-base
      jsonrpc-async
      ajsonrpc
      ipykernel
      ipython
      jupyter
      pyzmq
      epc
      orjson
      packaging
      paramiko
      rapidfuzz
      setuptools 
      sexpdata
      six
      watchdog
      ty
      ruff
      basedpyright
      cmake
    '';
   
  };

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
