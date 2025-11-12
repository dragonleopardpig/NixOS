{ lib, config, pkgs, ... }:

{
  # TODO please change the username & home directory to your own
  home.username = "thinky";
  home.homeDirectory = "/home/thinky";

  xdg.configFile."uwsm/env".source = "${config.home.sessionVariablesPackage}/etc/profile.d/hm-session-vars.sh";
  wayland.windowManager.hyprland = {
    enable = true;
    systemd = {
      # disable the systemd integration, as it conflicts with uwsm.
      enable = false;
      variables = [ "--all" ];
    };
  };
  
  wayland.windowManager.hyprland.settings = {
    "$mod" = "SUPER";
    bind =
      [
        "$mod, F, exec, firefox"
        "$mod, Q, exec, kitty"
        "$mod, E, exec, emacs"
        "$mod, P, exec, protonvpn-app"
        "$mod, M, exec, walker"
        "$mod, left, movefocus, l"
        "$mod, right, movefocus, r"
        "$mod, up, movefocus, u"
        "$mod, down, movefocus, d"
        "$mod SHIFT, F, fullscreen, 0"
        ", F1, exec, sleep 0.1 && hyprctl dispatch dpms off && hyprlock"
        ", F6, exec, ddcutil setvcp 10 + 10"
        ", F5, exec, ddcutil setvcp 10 - 10"
        ",XF86MonBrightnessUp, exec, ddcutil setvcp 10 + 10"
        ",XF86MonBrightnessDown, exec, ddcutil setvcp 10 - 10"
        "CTRL ALT, left, workspace, -1"
        "CTRL ALT, right, workspace, +1"
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
    
    bindc =[
      "$mod, mouse:274, togglefloating"
    ];
    
    input = {
      natural_scroll = true;
      # other input settings...
    };

    monitor = "DP-3,1920x1080@60,0x0,1";
    # Autostart programs
    exec-once = [ "protonvpn-app" "while true; do sleep 60; waypaper --random; done"];

    misc = {
      mouse_move_enables_dpms = true;
      key_press_enables_dpms = true;
    };

  };

  programs.hyprpanel = {
    enable = true;
    # Configure and theme almost all options from the GUI.
    # See 'https://hyprpanel.com/configuration/settings.html'.
    # Default: <same as gui>
    settings = {

      # Configure bar layouts for monitors.
      # See 'https://hyprpanel.com/configuration/panel.html'.
      # Default: null
      
      bar.layouts = {
        "*" = {
          left = [ "dashboard" "workspaces" "media"];
          middle = [ "windowtitle" ];
          right = [ "network" "bluetooth" "volume" "battery" "systray" "clock" "notifications" ];
        };
      };

      bar.launcher.autoDetectIcon = true;
      bar.workspaces.show_icons = true;

      menus.clock = {
        time = {
          military = true;
          hideSeconds = true;
        };
        weather.unit = "metric";
        weather.location = "Singapore, SG";
      };

      menus.dashboard.directories.enabled = true;
      #menus.dashboard.stats.enable_gpu = true;
      theme = {
        bar.transparent = true;
        font = {
        name = "CaskaydiaCove NF";
        size = "14px";
        };
      };
    };
  };

  services.hyprpaper.enable = true;
  services.hyprpaper.settings = {
    # Set a preload wallpaper
    preload = [
      "~/Pictures/Kath.png"
      "~/Pictures/corndog.png"
      "~/Pictures/Mepth.png"
      "~/Pictures/Sollee.png"
      "~/Pictures/srev.png"
      "~/Pictures/VDawg.png"
    ];

    # Set the wallpaper for a specific display
    wallpaper = [
      "DP-3,~/Pictures/corndog.png"
    ];
  };

  programs.hyprlock = {
    enable = true;
  };

  # catppuccin.hyprland.enable = true;
  # catppuccin.hyprland.flavor = "mocha";
  # catppuccin.hyprland.accent = "mauve";

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
    # theme = "github_dark_high_contrast";
  };
  
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
      background_opacity = "0.8";
      background_blur = 5;
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
