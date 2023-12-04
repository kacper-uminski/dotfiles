{ config, pkgs, ... }:

{
  # Configure networking.
  networking = {
    hostName = "nixos";
    networkmanager.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.kacper = {
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
      gnome.gnome-tweaks
      gnomeExtensions.blur-my-shell
      light
      networkmanagerapplet
    ];
  };

  services = {
    # Flatpak
    flatpak.enable = true;

    # X11
    xserver = {
      # Enable the GNOME Desktop Environment.
      displayManager.gdm.enable = true;
      desktopManager.gnome.enable = true;

      # Enable touchpad support (enabled default in most desktopManager).
      libinput.enable = true;
    };
  };

  # Excluded packages from gnome.
  environment.gnome.excludePackages = (with pkgs; [
      baobab # Disk usage viewer.
      gnome-connections
      gnome-photos
      gnome-text-editor
      gnome-tour
    ]) ++ (with pkgs.gnome; [
      cheese # webcam tool
      epiphany # web browser
      #evince # document viewer
      geary # email reader
      gedit # text editor
      #gnome-calculator
      gnome-calendar
      gnome-characters
      gnome-clocks
      gnome-contacts
      gnome-disk-utility
      gnome-font-viewer
      gnome-logs
      gnome-maps
      gnome-music
      gnome-screenshot
      gnome-system-monitor
      gnome-weather
      simple-scan
      #totem # video player
      yelp # help viewer

      # Games
      atomix # puzzle game
      hitori # sudoku game
      iagno # go game
      tali # poker game
    ]);

  home-manager = {
    users.kacper = {pkgs, ...}: {
      programs = {
        alacritty = {
          settings = {
            colors = {
              primary = {
                background = "#1b182c";
                foreground = "#cbe3e7";
              };
              normal = {
                black = "#100e23";
                red = "#ff8080";
                green = "#95ffa4";
                yellow = "#ffe9aa";
                blue = "#91ddff";
                magenta = "#c991e1";
                cyan = "#aaffe4";
                white = "#cbe3e7";
              };
              bright = {
                black = "#565575";
                red = "#ff5458";
                green = "#62d196";
                yellow = "#ffb378";
                blue = "#65b2ff";
                magenta = "#906cff";
                cyan = "#63f2f1";
                white = "#a6b3cc";
              };
            };
          };
        };

        waybar = {
          enable = true;
          settings = {
            mainBar = {
              height = 30;
              layer = "top";
              modules-left = [ "hyprland/workspaces" ];
              modules-right = [ "network" "battery" "tray" ];
              output = [ "eDP-1" ];
              position = "top";
              spacing = 4;

              battery = {
                adapter = "ADP1";
                bat = "BAT1";
                format = "{icon}  {capacity}%";
                format-icons = ["" "" "" "" ""];
                states = {
                  critical = 15;
                  warning = 30;
                };
              };

              "hyprland/workspaces" = {
                format = "{icon}";
                on-scroll-up = "hyprctl dispatch workspace e+1";
                on-scroll-down = "hyprctl dispatch workspace e-1";
              };

              network = {
                format-disconnected = "";
                format-wifi = "";
                interface = "wlp0s20f3";
              };

              tray = {
                icon-size = 21;
                spacing = 10;
              };
            };
          };
          style = ''
          * {
            border: none;
            border-radius: 0;
            font-family: Plex Mono;
          }
          window#waybar {
            background: #1b182c;
            color: #cbe3e7;
          }
          #network.wifi {
            margin: 0 10px;
          }
          '';
        };
      };

      wayland.windowManager.hyprland = {
        settings = {
          exec-once = [
            "swww init &"
            "swww img $HOME/Pictures/Wallpapers/nixos.png &"
            "waybar &"
            "$term &"
            "firefox &"
          ];
          gestures = {
            workspace_swipe = true;
            workspace_swipe_invert = false;
          };
          input = {
            touchpad = {
              middle_button_emulation = true;
            };
          };
        };
      };
    };
  };
}
