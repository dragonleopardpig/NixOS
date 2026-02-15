{ lib, config, pkgs, ... }:

{
  # TODO please change the username & home directory to your own
  home.username = "thinky";
  home.homeDirectory = "/home/thinky";

  # wayland.windowManager.hyprland.enable = true; # enable Hyprland
  # wayland.windowManager.hyprland.systemd.variables = ["--all"];
  xdg.configFile."uwsm/env".source = "${config.home.sessionVariablesPackage}/etc/profile.d/hm-session-vars.sh";
  wayland.windowManager.hyprland = {
    enable = true;
    systemd = {
      # disable the systemd integration, as it conflicts with uwsm.
      enable = false;
      variables = [ "--all" ];
    };
    settings = {
      general = {
        gaps_in = 0;
        gaps_out = 0;
        "col.active_border" = "rgb(00FFFF)";
        resize_on_border = true;
        border_size = 3;
      };
      decoration = {
        dim_inactive = true;
        inactive_opacity = 0.9;
        dim_strength = 0.2;
      };
      animations = {
        workspace_wraparound = true;
      };
    };
  };
  
  wayland.windowManager.hyprland.settings = {
    "$mod" = "SUPER";
    bind =
      [
        "$mod, F, exec, nvidia-offload firefox"
        "$mod, Q, exec, nvidia-offload kitty"
        "$mod, E, exec, nvidia-offload emacs"
        "$mod, P, exec, protonvpn-app"
        "$mod SHIFT, F, fullscreen, 1" 
        "$mod, K, exec, hyprctl dispatch killactive"
        "$mod, M, exec, walker"
        "$mod, left, movefocus, l"
        "$mod, right, movefocus, r"
        "$mod, up, movefocus, u"
        "$mod, down, movefocus, d"
        "$mod SHIFT, L, exec, hyprlock"
        "CTRL ALT, left, workspace, -1"
        "CTRL ALT, right, workspace, +1"
        "Alt, Tab, cyclenext, hist"
        ", Print, exec, grimblast copy area"
        ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
        ", F6, exec, brightnessctl s 5%+"
        ", F5, exec, brightnessctl s 5%-"
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
    # Volume and Media Control
    binde = [
      ", XF86AudioRaiseVolume, exec, wpctl set-volume -l 1.4 @DEFAULT_AUDIO_SINK@ 5%+"
      ", XF86AudioLowerVolume, exec, wpctl set-volume -l 1.4 @DEFAULT_AUDIO_SINK@ 5%-"
    ];

    input = {
      natural_scroll = true;
      # other input settings...
    };

    # monitor = "DP-3,1920x1080@60,0x0,1";
    # Autostart programs
    exec-once = [ "while true; do sleep 60; waypaper --random; done" ];
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
      theme = {
        font.size = "14px";
        font.name = "CaskaydiaCove NF";
        bar = {
          transparent = true;
          outer_spacing = "0rem";
        };
        bar.buttons.enableBorders = true;
      };

      bar.layouts = {
        "0" = {
          left = [ "dashboard" "workspaces" "media" ];
          middle = [ "windowtitle" ];
          right = [ "volume" "network" "bluetooth"
                    "battery" "systray" "clock" "notifications" ];
        };
      };
     
      bar.launcher.autoDetectIcon = true;
      bar.workspaces.show_icons = true;

      menus.clock = {
        time = {
          military = true;
          hideSeconds = true;
        };
        weather = {
          enabled = true;
          location = "Singapore";
          unit = "metric";
          "weather_api_key" = "d732c806c27b455abc7132317251511";
        };
      };

      menus.dashboard.directories.enabled = true;
      # menus.dashboard.stats.enable_gpu = true;
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
      "eDP-1,~/Pictures/Kath.png"
    ];
  };
  
  programs.hyprlock = {
    enable = true;
  };

  # programs.walker.enable = true;
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
    settings.user.name = "dragonleopardpig";
    settings.user.email = "dragonleopardpig@gmail.com";
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
  
 programs.kitty = {
   enable = true;
   font = {
     size = 10; # Replace with your desired size
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
