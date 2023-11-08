{ config, pkgs, lib, ... }:

{
  networking = {
    hostName = "nixos";
    defaultGateway = "192.168.50.1";
    nameservers = ["192.168.50.200"];
    interfaces.eno2.ipv4.addresses = [{
      address = "192.168.50.250";
      prefixLength = 24;
    }];
  };

  fileSystems."/home/kacper/media" = {
    device = "192.168.50.200:/mnt/Tank/Media";
    fsType = "nfs";
  };

  services.xserver = {
    enable = true;
    
    # Set drivers and enable "TearFree"
    videoDrivers = [ "intel" ];
    #useGlamor = true;
    deviceSection = ''
      Option "AccelMethod" "sna"
      Option "TearFree" "true"
      Option "DRI" "2"
    ''; 

    # Enable XMonad.
    displayManager.startx.enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  
  };



  # Enable SANE to scan documents.
  hardware.sane.enable = true;

  users.users.kacper = {
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
      feh
      flameshot
      lxappearance
      minecraft
      mupdf
      puddletag
      pulsemixer
      qbittorrent
      #retroarchFull
      shntool
      skypeforlinux
      slack
      unclutter
      xorg.xinit
    ];
  };

  home-manager = {
    users.kacper = {pkgs, ...}: {
      programs = {

        alacritty = {
          enable = true;

          settings = {

            colors = {
              primary = {
                background = "#000000";
                foreground = "#d8dee9";
                dim_foreground = "#a5abb6";
              };
              cursor = {
                text = "#2e3440";
                background = "#d8dee9";
              };
              selection = {
                text = "#000000";
                background = "#d8dee9";
              };
              search = {
                matches = {
                  text = "#000000";
                  background = "#d8dee9";
                };
                footer_bar = {
                  foreground = "#d8dee9";
                  background = "#434c5e";
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

            font = {
              normal.family = "IBM Plex Mono";
              glyphs.family = "Font Awesome";
              size = 12;
            };

            window.padding = {
              x = 20;
              y = 20;
            };
          };
        };

      };

      services = {
        picom = {
          enable = false;
          fade = true;
          fadeDelta = 3;
          fadeSteps = [0.03 0.03];
        };
      };
        
    };
  };

  services = {

    # Enable Gnome Keyring
    gnome.gnome-keyring.enable = true;

    # Enable Roon Server
    roon-server = {
      enable = true;
      user = "kacper";
    };

  }; # End Services.

  # Virtualization
  virtualisation = {
    anbox.enable = false;
    libvirtd.enable = false;
    spiceUSBRedirection.enable = false;
  };

  programs = {
    steam.enable = true;
    dconf.enable = true;
  };
}
