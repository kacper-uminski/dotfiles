{ config, pkgs, lib, ... }:

{
  boot = {
    # Enable splash-screen
    plymouth.enable = true;

    # Use the systemd-boot EFI boot loader
    loader = {
      systemd-boot.enable = true;
      efi = {
        efiSysMountPoint = "/boot";
        canTouchEfiVariables = true;
      };
    };
  };

  # Set your time zone
  time.timeZone = "Europe/Stockholm";

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "sv_SE.UTF-8";
      LC_IDENTIFICATION = "sv_SE.UTF-8";
      LC_MEASUREMENT = "sv_SE.UTF-8";
      LC_MONETARY = "sv_SE.UTF-8";
      LC_NAME = "sv_SE.UTF-8";
      LC_NUMERIC = "sv_SE.UTF-8";
      LC_PAPER = "sv_SE.UTF-8";
      LC_TELEPHONE = "sv_SE.UTF-8";
      LC_TIME = "sv_SE.UTF-8";
    };
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "dvorak";
  };

  # Home Manager
  home-manager = {
    useGlobalPkgs = true;
    users.kacper = {pkgs, ...}: {
      home = {
        username = "kacper";
        homeDirectory = "/home/kacper";
        stateVersion = "22.11";
      };
      programs = {

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
    };
  };

  # X11
  services.xserver = {
    # Enable the X11 windowing system
    enable = true;
  
    # Configure keymap in X11
    layout = "us";
    xkbVariant = "dvorak";
    xkbOptions = "ctrl:swapcaps";
  };

  # Hardware video acceleration
  nixpkgs.config = { 
    packageOverrides = pkgs: {
      vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
    };
    permittedInsecurePackages = [
      "openssl-1.1.1u"
    ];
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
          pylsp-mypy
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
      clang_16
      clojure-lsp
      darktable
      du-dust
      elixir
      erlang
      erlang-ls
      exa
      exercism
      ffmpeg
      file
      firefox
      flatpak
      gcc12
      ghc
      git
      gnuapl
      gnumake
      haskell-language-server
      htop
      imagemagick
      jdk
      jetbrains.idea-community
      leiningen
      libGL
      libsForQt5.qtstyleplugin-kvantum
      llvmPackages_15.bintools
      neofetch
      nushell
      nvc
      p7zip
      qt6.full
      qtcreator
      rar
      ripgrep
      rustc
      rustfmt
      speedtest-cli
      swiProlog
      tdesktop
      #texlive.combined.scheme-full
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
}
