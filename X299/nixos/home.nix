{ lib, config, inputs, pkgs, ... }:

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
    extraConfig = ''
    env = XDG_CURRENT_DESKTOP,Hyprland
    env = XDG_SESSION_TYPE,wayland
    env = XDG_SESSION_DESKTOP,Hyprland
    env = GTK_IM_MODULE,
    env = QT_IM_MODULE,
  '';
    settings = {
      general = {
        gaps_in = 0; # Inner gaps
        gaps_out = 0; # Outer gaps
        border_size = 3;
        layout = "dwindle";
        "col.active_border" = "rgb(00FFFF)";
        "col.inactive_border" = "rgb(202020)";
        resize_on_border = true;
        extend_border_grab_area = 20;
      };
      decoration = {
        inactive_opacity = 0.9;
        dim_inactive = true;
        dim_strength = 0.1;
      };
      animations = {
        workspace_wraparound = true;
      };
      input = {
        follow_mouse = 1;
      };
    };
  };

  wayland.windowManager.hyprland.settings = {
    "$mod" = "SUPER";
    bind =
      [
        "$mod, F, exec, firefox"
        "$mod, Q, exec, kitty"
        "$mod, E, exec, emacs"
        # ProtonVPN: Known Wayland issue - app may start minimized to tray. Click tray icon or use this keybinding.
        "$mod, P, exec, bash -c 'if pgrep -f protonvpn-app >/dev/null; then gdbus call --session --dest proton.vpn.app.gtk --object-path /proton/vpn/app/gtk --method org.gtk.Application.Activate {} 2>/dev/null || true; else protonvpn-app; fi'"
        "$mod, M, exec, walker"
        "$mod, A, exec, anyrun"
        "$mod, N, exec, nemo"
        "$mod, Y, exec, kitty -e yazi"
        "$mod, Escape, exit,"
        "$mod, K, killactive,"
        "$mod, left, movefocus, l"
        "$mod, right, movefocus, r"
        "$mod, up, movefocus, u"
        "$mod, down, movefocus, d"
        "$mod SHIFT, F, fullscreen, 1"
        ", Print, exec, hyprshot -m region"
        ", XF86AudioRaiseVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"
        ", XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
        ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
        ", XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"
        ", XF86AudioNext, exec, playerctl next"
        ", XF86AudioPause, exec, playerctl play-pause"
        ", XF86AudioPlay, exec, playerctl play-pause"
        ", XF86AudioPrev, exec, playerctl previous"
        ", F1, exec, sleep 0.1 && hyprctl dispatch dpms off && hyprlock"
        ", F6, exec, brightnessctl -d $(ls /sys/class/backlight/ | grep -m1 ddcci) set +10%"
        ", F5, exec, brightnessctl -d $(ls /sys/class/backlight/ | grep -m1 ddcci) set 10%-"
        ",XF86MonBrightnessUp, exec, brightnessctl -d $(ls /sys/class/backlight/ | grep -m1 ddcci) set +10%"
        ",XF86MonBrightnessDown, exec, brightnessctl -d $(ls /sys/class/backlight/ | grep -m1 ddcci) set 10%-"
        "CTRL ALT, left, workspace, -1"
        "CTRL ALT, right, workspace, +1"
        "ALT, Tab, cyclenext, hist"
        "$mod, Tab, cyclenext, prev"
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
    # ProtonVPN: Start without --start-minimized so window appears, then user can minimize to tray
    exec-once = [ "bash -c 'while ! dbus-send --session --dest=org.freedesktop.DBus --type=method_call --print-reply /org/freedesktop/DBus org.freedesktop.DBus.ListNames 2>/dev/null | grep -q org.kde.StatusNotifierWatcher; do sleep 1; done; protonvpn-app'"
                  "swww-daemon && waypaper --random"
                  "while true; do sleep 60; waypaper --random; done"
                ];

    misc = {
      mouse_move_enables_dpms = true;
      key_press_enables_dpms = true;
    };

  };

  # programs.hyprshot.enable = true;

  programs.hyprpanel = {
    package = inputs.hyprpanel.packages.${pkgs.stdenv.hostPlatform.system}.default;
    enable = true;
    settings = {
      bar.layouts = {
        "*" = {
          left = [ "dashboard" "workspaces" "media"];
          middle = [ "windowtitle" ];
          right = [ "network" "volume"
                    "battery" "systray" "clock" "notifications" ];
        };
      };
      bar.launcher.autoDetectIcon = true;
      bar.workspaces.show_icons = true;
      menus.clock.weather.unit = "metric";
      menus.clock.weather.location = "Singapore";
      # Weather API key is stored in ~/.config/secrets/weather-api-key
      # The activation script below will read it and update config.json at runtime
      menus.clock.weather.key = "PLACEHOLDER_WILL_BE_REPLACED";
      menus.dashboard.directories.enabled = true;
      menus.dashboard.shortcuts.left.shortcut1 = {
        icon = "󰈹";
        tooltip = "Firefox";
        command = "firefox";
      };
      #menus.dashboard.stats.enable_gpu = true;
      theme = {
        bar.transparent = true;
        bar.outer_spacing = "0em";
        bar.buttons.enableBorders = true;
        font = {
          name = "Inter";
          size = "13px";
        };
      };
    };
  };

  services.hyprpaper.enable = false;

  # Waypaper configuration (wallpaper manager using swww backend)
  xdg.configFile."waypaper/config.ini".text = ''
    [Settings]
    language = en
    folder = ~/Pictures/Wallpapers
    monitors = All
    wallpaper = ~/Pictures/Wallpapers/Sollee.png
    show_path_in_tooltip = True
    backend = swww
    fill = fill
    sort = name
    color = #ffffff
    subfolders = False
    all_subfolders = False
    show_hidden = False
    show_gifs_only = False
    post_command =
    number_of_columns = 3
    swww_transition_type = any
    swww_transition_step = 90
    swww_transition_angle = 0
    swww_transition_duration = 2
    swww_transition_fps = 60
    mpvpaper_sound = False
    mpvpaper_options =
    use_xdg_state = False
    zen_mode = False
  '';

  services.hypridle.enable = true;
  services.hypridle.settings = {
    general = {
      after_sleep_cmd = "hyprctl dispatch dpms on";
      ignore_dbus_inhibit = false;
      lock_cmd = "hyprlock";
    };

    listener = [
      {
        timeout = 900;
        on-timeout = "hyprlock";
      }
      {
        timeout = 1200;
        on-timeout = "hyprctl dispatch dpms off";
        on-resume = "hyprctl dispatch dpms on";
      }
    ];
  };

  programs.hyprlock = {
    enable = true;
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
      symbol = "";
      format = "[$symbol$name]($style) ";
      style = "bright-purple bold";
    };
    git_branch = {
      only_attached = true;
      format = "[$symbol$branch]($style) ";
      # symbol = "שׂ";
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

  # Set Nemo as default file manager
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "inode/directory" = [ "nemo.desktop" ];
      "application/pdf" = [ "sioyek.desktop" ];
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
    };
    # theme = "github_dark_high_contrast";
  };

  programs.kitty = {
    enable = true;
    font = {
      size = 9; # Replace with your desired size
      name = "JetBrainsMono Nerd Font";
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
      eval "$(direnv hook bash)"
    '';

    # set some aliases, feel free to add more or remove some
    shellAliases = {
      ls = "eza --icons=always --group-directories-first --sort=extension";
      gc = "git commit -m";
      rebuild = "sudo nixos-rebuild switch";
    };
    initExtra = ''
      fastfetch
    '';
  };

  programs.anyrun = {
    enable = true;
    config = {
      x = { fraction = 0.5; };
      y = { fraction = 0.3; };
      width = { fraction = 0.3; };
      hideIcons = false;
      ignoreExclusiveZones = false;
      layer = "overlay";
      hidePluginInfo = false;
      closeOnClick = false;
      showResultsImmediately = true;
      maxEntries = null;

      plugins = [
        "${pkgs.anyrun}/lib/libapplications.so"
        # "${pkgs.anyrun}/lib/libsymbols.so"
      ];
    };

    # Inline comments are supported for language injection into
    # multi-line strings with Treesitter! (Depends on your editor)
    extraCss = /*css */ ''
      .some_class {
        background: red;
      }
    '';

    extraConfigFiles."some-plugin.ron".text = ''
      Config(
        // for any other plugin
        // this file will be put in ~/.config/anyrun/some-plugin.ron
        // refer to docs of xdg.configFile for available options
      )
    '';
  };

  programs.yazi = {
    enable = true;
    settings = {
      mgr = {
        ratio = [
          1
          3
          4
        ];
        sort_by = "extension";
        sort_sensitive = true;
        sort_reverse = false;
        sort_dir_first = true;
        linemode = "none";
        show_hidden = true;
        show_symlink = true;
      };

      preview = {
        image_filter = "lanczos3";
        image_quality = 90;
        tab_size = 1;
        max_width = 600;
        max_height = 900;
        cache_dir = "";
        ueberzug_scale = 1;
        ueberzug_offset = [
          0
          0
          0
          0
        ];
      };

      tasks = {
        micro_workers = 5;
        macro_workers = 10;
        bizarre_retry = 5;
      };
    };
  };

  # Install firefox.
  programs.firefox.enable = true;
  
  # This value determines the home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update home Manager without changing this value. See
  # the home Manager release notes for a list of state version
  # changes in each release.
  # Replace HyprPanel config symlink with a writable copy and inject API key from secrets file
  home.activation.makeHyprpanelConfigWritable = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    if [ -L "$HOME/.config/hyprpanel/config.json" ]; then
      cp --remove-destination "$(readlink "$HOME/.config/hyprpanel/config.json")" "$HOME/.config/hyprpanel/config.json"
      chmod u+w "$HOME/.config/hyprpanel/config.json"
    fi
    # Inject the weather API key from secrets file
    if [ -f "$HOME/.config/secrets/weather-api-key" ] && [ -f "$HOME/.config/hyprpanel/config.json" ]; then
      API_KEY=$(cat "$HOME/.config/secrets/weather-api-key" | tr -d '\n')
      ${pkgs.jq}/bin/jq --arg key "$API_KEY" '.["menus.clock.weather.key"] = $key' "$HOME/.config/hyprpanel/config.json" > "$HOME/.config/hyprpanel/config.json.tmp"
      mv "$HOME/.config/hyprpanel/config.json.tmp" "$HOME/.config/hyprpanel/config.json"
    fi
  '';
  home.stateVersion = "25.11";
}
