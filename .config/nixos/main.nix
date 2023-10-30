{ config, pkgs, lib, ... }:

{
  nix.settings.experimental-features = [ "flakes" "nix-command" ];
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
      LC_TIME = "en_US.UTF-8";
    };
  };

  console = {
    keyMap = "dvorak";
  };

  # Home Manager
  home-manager = {

    useGlobalPkgs = true;

    users.kacper = {pkgs, ...}: {
      home = {
        username = "kacper";
        homeDirectory = "/home/kacper";
        stateVersion = "23.05";
        shellAliases = {
          cat = "bat";
          dotfiles = "git --git-dir=$HOME/software/dotfiles --work-tree=$HOME";
          emc = "emacsclient -nw";
          mupdf = "mupdf-x11";
          sacd_extract = "$HOME/software/sacd_extract/result/bin/sacd_extract";
          vim = "nvim";
        };
      };

      programs = {

        bash = {
          enable = true;
          enableCompletion = true;
          historyFile = ".config/bash/bash_history";
        };

        bat = {
          enable = true;
        };

        btop = {
          enable = true;
        };

        emacs = {
          enable = true;
          package = pkgs.emacs29;
        };

        eza = {
          enable = true;
          enableAliases = true;
          git = true;
          icons = true;
        };

        feh = {
          enable = true;
        };

        git = {
          enable = true;
          userName = "kacper-uminski";
          userEmail = "kacperum@gmail.com";
        };

        htop = {
          enable = true;
        };

        mpv = {
          enable = true;
          config = {
            hwdec = "auto-safe";
            vo = "gpu";
            profile = "gpu-hq";
          };
        };

        neovim = {
          enable = true;
        };

        nushell = {
          enable = true;
        };

        ripgrep = {
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

        zsh = {
          enable = true;
          enableAutosuggestions = true;
          enableCompletion = true;
          syntaxHighlighting.enable = true;
          dotDir = ".config/zsh";
          history.path = "$ZDOTDIR/zsh_history";
          loginExtra = "if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then startx; fi";
        };
      };

      services = {
        emacs = {
          # Enable emacs daemon.
          enable = true;
          defaultEditor = true;
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
    shell = pkgs.zsh;
  };

  # Set system fonts.
  fonts.packages = with pkgs; [
    fira
    fira-code
    font-awesome
    ibm-plex
    jetbrains-mono
    nerdfonts
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

    pathsToLink = [
      "/share/bash-completion"
      "/share/zsh"
    ];

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
      adw-gtk3
      bat
      cargo # Rust toolchain
      cbqn
      chromium
      cifs-utils
      clippy # Rust linter
      darktable
      elixir
      erlang
      erlang-ls
      exercism
      ffmpeg
      file
      firefox
      flatpak
      gcc13
      ghc
      gnuapl
      gnuplot
      gnumake
      gradience
      haskell-language-server
      imagemagick
      jdk
      jetbrains.idea-community
      julia-bin
      clang-tools_16
      neofetch
      nvc
      octaveFull
      p7zip
      qt6.qtbase
      qt6.qt5compat
      rar
      rust-analyzer
      rustc
      rustfmt
      speedtest-cli
      tdesktop
      texlive.combined.scheme-full
      uiua
      unzip
      usbutils
      uutils-coreutils
      valgrind
      vifm
      wget
      wiki-tui
      xmrig
      xorg.xkill
    ];
  }; # End Environment

  programs = {
    adb.enable = false;
    zsh.enable = true;
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

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
