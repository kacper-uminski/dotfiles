{ config, pkgs, lib, ... }:

{
  boot = {
    # Enable splash-screen
    plymouth.enable = true;

    # Use the systemd-boot EFI boot loader.
    loader = {
      systemd-boot.enable = true;
      efi = {
        efiSysMountPoint = "/efi";
        canTouchEfiVariables = true;
      };
    };
  };

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "dvorak";
  };

  # X11
  services.xserver = {
    # Enable the X11 windowing system.
    enable = true;
  
    # Configure keymap in X11
    layout = "us";
    xkbVariant = "dvorak";
    xkbOptions = "ctrl:swapcaps";
  };

  # Hardware video acceleration.
  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };
  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      vaapiVdpau
      libvdpau-va-gl
    ];

  };
    
  # Enable Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Pipewire
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = false;
  };
  security.rtkit.enable = true; # Optional, but recommended, for pipewire.

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.kacper = {
    description = "Kacper Uminski";
    isNormalUser = true;
    shell = pkgs.nushell;
  };

  # Home Manager
  home-manager = {
    useGlobalPkgs = true;
    users.kacper = {pkgs, ...}: {
      home.username = "kacper";
      home.homeDirectory = "/home/kacper";

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

        # Enable emacs daemon.
        emacs.enable = true;

        git = {
          enable = true;
          userName = "kacper-uminski";
          userEmail = "kacperum@gmail.com";
        };

        mpv = {
          enable = true;
          config = {
            hwdec = "auto-safe";
            vo = "gpu";
            profile = "gpu-hq";
          };
        };

        nushell = {
          enable = true;
        };

        starship = {
          enable = true;
          settings = {
            add_newline = false;
            format = lib.concatStrings [
              "$all"
              "$directory"
              "$character"
            ];
          };
        };
      };

      home.stateVersion = "22.11";
    };
  };

  # Set system fonts.
  fonts.fonts = with pkgs; [
    _3270font
    fira
    fira-code
    font-awesome
    ibm-plex
    iosevka
    jetbrains-mono
    julia-mono
    victor-mono
  ];

  # Allow non-free packages.
  nixpkgs.config.allowUnfree = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  environment = {

#    variables = {
#      QT_STYLE_OVERRIDE="kvantum";
#    };

    # List packages installed in system profile.
    systemPackages = with pkgs; [
      (let
        my-python-packages = python-packages: with python-packages; [
          flake8
          mypy
          #(opencv4.override { enableGtk2 = true; })
          #pygame
          pylsp-mypy
          #pymunk
          python-lsp-server
        ];
        python-with-my-packages = python3.withPackages my-python-packages;
      in
        python-with-my-packages)
      appimage-run
      bat
      cargo
      cbqn
      chromium
      cifs-utils
      clang_15
      darktable
      du-dust
      exa
      ffmpeg
      file
      firefox
      flatpak
      gcc
      ghc
      git
      gnuapl
      haskell-language-server
      htop
      imagemagick
      jdk
      jetbrains.idea-community
      leiningen
      libsForQt5.qtstyleplugin-kvantum
      neofetch
      nushell
      nvc
      p7zip
      rar
      ripgrep
      speedtest-cli
      swiProlog
      tdesktop
      texlive.combined.scheme-full
      unzip
      usbutils
      uutils-coreutils
      vifm
      vim
      wget
      wiki-tui
      xmrig
      xorg.xkill
    ];
  }; # End Environment

  programs = {

    adb.enable = true;

    steam.enable = true;

    zsh = {
      enable = true;

      shellAliases = {
        dotfiles = "git --git-dir=$HOME/software/dotfiles --work-tree=$HOME";
        emc = "emacsclient -nw";
        ls = "exa -al";
        mupdf = "mupdf-x11";
        sacd_extract = "$HOME/software/sacd_extract/result/bin/sacd_extract";
      };
    };
  };

  # List of services that you want to enable.
  services = {
   

    # Enable Gnome Keyring
    gnome.gnome-keyring.enable = true;

    # Enable the OpenSSH daemon.
    openssh.enable = true;

#    # Enable Picom
#    picom = {
#      enable = false;
#    };

    # Enable Unclutter
    unclutter = {
      enable = true;
      timeout = 3;
    };

  }; # End Services

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

}
