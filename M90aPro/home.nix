{ lib, config, pkgs, ... }:

{
  # TODO please change the username & home directory to your own
  home.username = "thinky";
  home.homeDirectory = "/home/thinky";

  # link the configuration file in current directory to the specified location in home directory
  # home.file.".config/i3/wallpaper.jpg".source = ./wallpaper.jpg;

  # link all files in `./scripts` to `~/.config/i3/scripts`
  # home.file.".config/i3/scripts" = {
  #   source = ./scripts;
  #   recursive = true;   # link recursively
  #   executable = true;  # make all files executable
  # };

  # encode the file content in nix configuration file directly
  # home.file.".xxx".text = ''
  #     xxx
  # '';

  # set cursor size and dpi for 4k monitor
  xresources.properties = {
    "Xcursor.size" = 16;
    "Xft.dpi" = 172;
  };

  # Packages that should be installed to the user profile.
  home.packages = with pkgs; [
    # here is some command line tools I use frequently
    # feel free to add your own or remove some of them

  ];

  # basic configuration of git, please change to your own
  programs.git = {
    enable = true;
    userName = "dragonleopardpig";
    userEmail = "dragonleopardpig@gmail.com";
  };

  # starship - an customizable prompt for any shell
 programs.starship.enable = true;
  programs.starship.settings = {
    add_newline = false;
    format = "$shlvl$username$hostname$nix_shell$git_branch$git_commit$git_state$git_status$directory$jobs$cmd_duration$all$character";
    shlvl = {
      disabled = false;
      #symbol = "ﰬ";
      style = "bright-red bold";
    };
    shell = {
      disabled = true;
      format = "$indicator";
      fish_indicator = "";
      bash_indicator = "[BASH](yellow) ";
      zsh_indicator = "[ZSH](bright-white) ";
    };
    username = {
      disabled = false;
      show_always = true;
      style_user = "bright-purple bold";
      style_root = "bright-red bold";
    };
    hostname = {
      disabled = false;
      style = " #F28C28 bold";
      ssh_only = false;
    };
    nix_shell = {
      symbol = "";
      format = "[$symbol$name]($style) ";
      style = "bright-purple bold";
    };
    git_branch = {
      only_attached = true;
      format = "[$symbol$branch]($style) ";
      # symbol = "שׂ";
      style = "bright-yellow bold";
    };
    git_commit = {
      only_detached = true;
      format = "[ﰖ$hash]($style) ";
      style = "bright-yellow bold";
    };
    git_state = {
      style = "bright-purple bold";
    };
    git_status = {
      style = "bright-green bold";
    };
    directory = {
      # read_only = " ";
      style = "bright-cyan bold";
      truncation_length = 10;
      truncate_to_repo = false;
    };
    cmd_duration = {
      format = "[$duration]($style) ";
      style = "bright-blue";
    };
    jobs = {
      style = "bright-green bold";
    };
    character = {
      success_symbol = "[\\$](bright-green bold)";
      error_symbol = "[\\$](bright-red bold)";
    };
  };

  # alacritty - a cross-platform, GPU-accelerated terminal emulator
  programs.alacritty = {
    enable = true;
    # custom settings
    settings = {
      env.TERM = "xterm-256color";
      font = {
        size = 10.5;
        # draw_bold_text_with_bright_colors = true;
      };
      scrolling.multiplier = 5;
      general.live_config_reload = true;
      selection.save_to_clipboard = true;
      keyboard.bindings = [
           { key = "W"; mods = "Alt"; action = "Copy"; }
           { key = "Y"; mods = "Control"; action = "Paste"; }
      ];
    };
    theme = "github_light_high_contrast";
  };
  
  programs.bash = {
    enable = true;
    enableCompletion = true;
    # TODO add your custom bashrc here
    bashrcExtra = ''
      export PATH="$PATH:$HOME/bin:$HOME/.local/bin:$HOME/go/bin"
    '';

    # set some aliases, feel free to add more or remove some
    shellAliases = {
      k = "kubectl";
      urldecode = "python3 -c 'import sys, urllib.parse as ul; print(ul.unquote_plus(sys.stdin.read()))'";
      urlencode = "python3 -c 'import sys, urllib.parse as ul; print(ul.quote_plus(sys.stdin.read()))'";
    };
  };

  # This value determines the home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update home Manager without changing this value. See
  # the home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "25.05";
}
