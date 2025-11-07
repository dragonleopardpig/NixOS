{ lib, config, pkgs, ... }:

{
  # TODO please change the username & home directory to your own
  home.username = "thinky";
  home.homeDirectory = "/home/thinky";

  wayland.windowManager.hyprland.enable = true; # enable Hyprland
  wayland.windowManager.hyprland.systemd.variables = ["--all"];
  xdg.configFile."uwsm/env".source = "${config.home.sessionVariablesPackage}/etc/profile.d/hm-session-vars.sh";
    
  wayland.windowManager.hyprland.settings = {
    "$mod" = "SUPER";
    bind =
      [
        "$mod, F, exec, firefox"
        "$mod, K, exec, kitty"
        "$mod, E, exec, emacs"
        "$mod, P, exec, protonvpn-app"
        "$mod, R, exec, remmina -c ~/.local/share/remmina/group_rdp_mit_192-168-137-1.remmina"
        "$mod, M, exec, wofi --show drun"
        "$mod, left, movefocus, l"
        "$mod, right, movefocus, r"
        "$mod, up, movefocus, u"
        "$mod, down, movefocus, d"
        "$mod SHIFT, L, exec, hyprlock"
        "$mod, Tab, cyclenext, hist"
        ", Print, exec, grimblast copy area"
      ]
      ++ (
        # workspaces
        # binds $mod + [shift +] {1..9} to [move to] workspace {1..9}
        builtins.concatLists (builtins.genList (i:
          let ws = i + 1;
          in [
            "$mod, code:1${toString i}, workspace, ${toString ws}"
            "$mod SHIFT, code:1${toString i}, movetoworkspace, ${toString ws}"
          ]
        )
          9)
      );

    bindm = [
      # mouse movements
      "$mod, mouse:272, movewindow"
      "$mod, mouse:273, resizewindow"
      "$mod ALT, mouse:272, resizewindow"
    ];

    input = {
      natural_scroll = true;
      # other input settings...
    };

    monitor = "DP-3,1920x1080@60,0x0,1";
    # Autostart programs
    exec-once = [ "waybar" ];
  };

  programs.waybar.enable = true;
  programs.waybar.settings.main = {
    modules-left = [ "hyprland/workspaces" ];
    modules-right = ["clock" "custom/shutdown" "custom/logout"];

    # Define the custom/shutdown module
    "custom/shutdown" = {
      format = "⏻"; # Unicode power symbol, or use an Awesome Font icon like "  power-off"
      tooltip = "Shutdown";
      # Command to execute when the button is clicked
      on-click = "systemctl poweroff"; 
    };

    "custom/logout" = {
      format = "  power "; # Use an icon from Nerd Fonts/Awesome Fonts
      tooltip = "Logout";
      on-click = "hyprctl dispatch exit";
    };
  };

  programs.hyprlock = {
    enable = true;
  };

  # programs.waybar.style = "";
  # wayland.windowManager.hyprland.plugins = [
  #   inputs.hyprland-plugins.packages.${pkgs.stdenv.hostPlatform.system}.hyprbars
  #   "/absolute/path/to/plugin.so"
  # ];


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
        size = 9;
        # draw_bold_text_with_bright_colors = true;
      };
      scrolling.multiplier = 5;
      general.live_config_reload = true;
      selection.save_to_clipboard = true;
      keyboard.bindings = [
           { key = "W"; mods = "Alt"; action = "Copy"; }
           { key = "Y"; mods = "Control"; action = "Paste"; }
      ];
      window.dimensions = {
        columns = 100;
        lines = 50;
      };
    };
    theme = "hyper";
  };

  # programs.fastfetch = {
  #   enable = true;
  #   settings = {
  #     logo = {
  #       type = "auto";
  #       width = 65;
  #       height = 35;
  #       padding = {
  #         right = 1;
  #       };
  #     };
  #   };
  # };
  
 programs.kitty = lib.mkForce {
   enable = true;
   font = {
     size = 9; # Replace with your desired size
     name = "JetBrainsMono Nerd Font"; # Optional: set the font name
   };
  settings = {
    confirm_os_window_close = 0;
    dynamic_background_opacity = true;
    enable_audio_bell = false;
    mouse_hide_wait = "-1.0";
    window_padding_width = 10;
    background_opacity = "0.9";
    background_blur = 5;
    symbol_map = let
      mappings = [
        "U+23FB-U+23FE"
        "U+2B58"
        "U+E200-U+E2A9"
        "U+E0A0-U+E0A3"
        "U+E0B0-U+E0BF"
        "U+E0C0-U+E0C8"
        "U+E0CC-U+E0CF"
        "U+E0D0-U+E0D2"
        "U+E0D4"
        "U+E700-U+E7C5"
        "U+F000-U+F2E0"
        "U+2665"
        "U+26A1"
        "U+F400-U+F4A8"
        "U+F67C"
        "U+E000-U+E00A"
        "U+F300-U+F313"
        "U+E5FA-U+E62B"
      ];
    in
      (builtins.concatStringsSep "," mappings) + " Symbols Nerd Font";
  };
  extraConfig = ''
    map alt+w copy_to_clipboard
    map ctrl+y paste_from_clipboard

    # Optional: Copy on select
    copy_on_select yes
  '';
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
