{config, pkgs, callPackage, ... }:
let
  pkgsUnstable = import <nixpkgs-unstable> { config = { allowUnfree = true; }; };
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-full
      dvisvgm dvipng # for preview and export as html
      wrapfig amsmath ulem hyperref capt-of;
  });
in
{
  # Overlays
  # nixpkgs.overlays = [
  #   (import (builtins.fetchTarball {
  # 	  url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
  #   }))
  # ];
  
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "thinky";
  home.homeDirectory = "/home/thinky";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
    cryfs
    calibre
    wget
    plank
    neofetch
    pkgsUnstable.remmina
    pkgsUnstable.protonvpn-gui
    tex
    nerdfonts
    ungoogled-chromium
    minder
    gparted
    pinta
    gimp
    inkscape
    sbcl
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. If you don't want to manage your shell through Home
  # Manager then you have to manually source 'hm-session-vars.sh' located at
  # either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/thinky/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.bash = {
    enable = true;
    enableCompletion = true;
    initExtra =
      ''
      R="\[\033[1;31m\]"
      G="\[\033[1;32m\]"
      Y="\[\033[1;33m\]"
      B="\[\033[1;34m\]"
      P="\[\033[1;35m\]"
      C="\[\033[1;36m\]"
      W="\[\033[1;37m\]"
      N="\[\033[00m\]"
      export PS1="\n$G[$Y\u$P@$R\h $B\D{%F %T}$W:$G\w]\n\$$N "
      neofetch
      ''; 
    shellAliases = {
      cry="cryfs basedir mountdir -c cryfs.config";
      bye="cryfs-unmount mountdir";
    };
  };
  programs.git = {
    enable = true;
    package = pkgs.gitFull;
    extraConfig = {
      credential.helper = "${
          pkgs.git.override { withLibsecret = true; }
      }/bin/git-credential-libsecret";
      push = { autoSetupRemote = true; };
    };
    userName  = "dragonleopardpig";
    userEmail = "dragonleopardpig@gmail.com";
  };
  programs.gh = {
    enable = true;
    gitCredentialHelper.enable = true;
  };
  programs.pandoc = {
    enable = true;
  };
  # Emacs
  # services.emacs.package = pkgs.emacs-unstable;
  # services.emacs.socketActivation.enable = true;
  programs.emacs = {
    enable = true;
    # package = pkgs.emacs-unstable;
    package = pkgs.emacs29-gtk3;
  };
  programs.firefox = {
    enable = true;
    package = pkgs.firefox;
  };
  programs.eza = {
    enable = true;
    icons = true;
    enableAliases = true;
    extraOptions = [
      "--group-directories-first"
      "--sort=extension"];
  };
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };
}
