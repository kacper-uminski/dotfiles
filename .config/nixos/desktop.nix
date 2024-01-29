{ config, pkgs, lib, ... }:

let
  stable = import <nixos-stable> { config = { allowUnfree = true; }; };
in {
  boot = {
    initrd.kernelModules = [ "amdgpu" ];
    kernelModules = [ "msr" "i2c-dev" "i2c-piix4" ];
  };
  networking = {
    hostName = "nixos";
    defaultGateway = "192.168.50.1";
    nameservers = ["192.168.50.200"];
    interfaces.enp14s0.ipv4.addresses = [{
      address = "192.168.50.250";
      prefixLength = 24;
    }];
  };

  fileSystems."/home/kacper/Media" = {
    device = "192.168.50.200:/mnt/Tank/Media";
    fsType = "nfs";
  };

  hardware = {
    opengl.extraPackages = with pkgs; [
      rocmPackages.clr.icd
    ];
    # Enable SANE to scan documents.
    sane.enable = false;
  };

  users.users.kacper = {
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
      clinfo
      feh
      flameshot
      minecraft
      puddletag
      pulsemixer
      qbittorrent
      stable.retroarchFull
      screen
      shntool
      skypeforlinux
      slack
      unclutter
      xorg.xset
      yuzu-early-access
    ];
  };

  home-manager = {
    users.kacper = {pkgs, ...}: {
	    home = {
        stateVersion = "23.11";
	    };
      programs = {
        alacritty = {
          settings = {
            colors = {
              primary = {
                background = "#000000";
                foreground = "#d8dee9";
                dim_foreground = "#a5abb6";
              };
              cursor = {
                background = "#d8dee9";
              };
              selection = {
                background = "#d8dee9";
              };
              search = {
                matches = {
                  background = "#d8dee9";
                };
              };
              normal = {
                black = "#000000";
                red = "#bf616a";
                green = "#a3be8c";
                yellow = "#ebcb8b";
                blue = "#81a1c1";
                magenta = "#b48ead";
                cyan = "#88c0d0";
                white = "#e5e9f0";
              };
              bright = {
                black = "#4c566a";
                red = "#bf616a";
                green = "#a3be8c";
                yellow = "#ebcb8b";
                blue = "#81a1c1";
                magenta = "#b48ead";
                cyan = "#88c0d0";
                white = "#e5e9f0";
              };
              dim = {
                black = "#000000";
                red = "#94545d";
                green = "#809575";
                yellow = "#b29e75";
                blue = "#68809a";
                magenta = "#8c738c";
                cyan = "#6d96a5";
                white = "#aeb3bb";
              };
            };
          };
        };
      };

      wayland.windowManager.hyprland = {
        settings = {
          bind = [
            # Keyboard layout switching
            "$mod ALT, D, exec, hyprctl switchxkblayout tom-wong-cornall/ellipse/wcass/purdea-andrei-modelfkeyboards.com-brand-new-f62/f77-model-f-keyboard-by-model-f-labs 0"
            "$mod ALT, S, exec, hyprctl switchxkblayout tom-wong-cornall/ellipse/wcass/purdea-andrei-modelfkeyboards.com-brand-new-f62/f77-model-f-keyboard-by-model-f-labs 1"
            "$mod ALT, P, exec, hyprctl switchxkblayout tom-wong-cornall/ellipse/wcass/purdea-andrei-modelfkeyboards.com-brand-new-f62/f77-model-f-keyboard-by-model-f-labs 2"
          ];
          exec-once = [
            "swww init &"
            "swww img $HOME/Pictures/Wallpapers/black.jpg &"
            "$term &"
            "firefox &"
            "skypeforlinux &"
            "telegram-desktop &"
            "xset s off &"
          ];
        };
      };
    };
  };

  services = {
    hardware.openrgb = {
      enable = true;
      package = pkgs.openrgb-with-all-plugins;
    };

    # Enable Roon Server
    roon-server = {
      enable = false;
      user = "kacper";
    };

    udev = {
      packages = with pkgs; [
        game-devices-udev-rules
      ];
    };

  }; # End Services.

  # Virtualization
  virtualisation = {
    anbox.enable = false;
    libvirtd.enable = false;
    spiceUSBRedirection.enable = false; # For livbirdt usb passthrough.
  };

  programs = {
    steam.enable = true;
    dconf.enable = true;
  };


  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
