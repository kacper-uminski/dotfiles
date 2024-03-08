{ config, pkgs, ... }:

{
  # Configure networking.
  networking = {
    hostName = "nixos";
    networkmanager.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.kacper = {
    extraGroups = [ "networkmanager" "video" "wheel" ];
    packages = with pkgs; [
      light
      networkmanagerapplet
      wireplumber
    ];
  };

  programs = {
    light.enable = true;
  };

  services = {
    # Flatpak
    flatpak.enable = false;
  };

  home-manager = {
    users.kacper = {pkgs, ...}: {
	    home = {
        stateVersion = "23.05";
	    };
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
                format-charging = " {capacity}%";
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
          bind = [
            # Keyboard layout switching
            "$mod ALT, D, exec, hyprctl switchxkblayout microsoft-surface-type-cover-keyboard 0"
            "$mod ALT, S, exec, hyprctl switchxkblayout microsoft-surface-type-cover-keyboard 1"
            "$mod ALT, P, exec, hyprctl switchxkblayout microsoft-surface-type-cover-keyboard 2"
          ];
          binde = [
            ", XF86MonBrightnessUp, exec, light -A 10"
            ", XF86MonBrightnessDown, exec, light -U 10"
            ", XF86AudioRaiseVolume, exec, wpctl set-volume -l 1.0 @DEFAULT_AUDIO_SINK@ 5%+"
            ", XF86AudioLowerVolume, exec, wpctl set-volume -l 1.0 @DEFAULT_AUDIO_SINK@ 5%-"
            ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
          ];
          exec-once = [
            "light -N 1"
            "swww init &"
            "swww img $HOME/Pictures/Wallpapers/nixos.png &"
            "waybar &"
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
          windowrule = [
            "workspace 1, Alacritty"
            "workspace 2, Emacs"
            "workspace 3, Firefox"
          ];
          workspace = [
            "1, on-created-empty:$term, default:true"
            "2, on-created-empty:emacs"
            "3, on-created-empty:firefox"
          ];
        };
      };
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
