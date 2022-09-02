# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

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

  environment.variables = {
    QT_STYLE_OVERRIDE="kvantum";
  };

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

    # Set drivers and enable "TearFree"
    videoDrivers = [ "intel" ];
    deviceSection = ''
      Option "DRI" "2"
      Option "TearFree" "true"
    ''; 

    # Monitor configuration.
    xrandrHeads = [
      {
        output = "DP2";
	primary = true;
        monitorConfig = ''
	  Option "Rotate" "Right"
	  Option "PreferredMode" "1920x1200"
	'';
      }
      {
        output = "DP1";
	monitorConfig = ''
	  Option "PreferredMode" "2560x1440"
	'';
      }
    ];


    # Enable XMonad.
    displayManager.startx.enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  
    # Configure keymap in X11
    layout = "us";
    xkbVariant = "dvorak";
    xkbOptions = "ctrl:swapcaps";
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.kacper = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
  };


  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    (let
      my-python-packages = python-packages: with python-packages; [
        opencv4
      ];
      python-with-my-packages = python3.withPackages my-python-packages;
    in
    python-with-my-packages)
    alacritty
    chromium
    cifs-utils
    darktable
    emacs
    exa
    feh
    ffmpeg
    firefox
    flameshot
    flameshot
    ghc
    git
    haskellPackages.xmobar
    htop
    imagemagick
    libsForQt5.qtstyleplugin-kvantum
    lxappearance
    minecraft
    mpv
    mupdf
    neofetch
    nitrogen
    p7zip
    picom
    puddletag
    pulsemixer
    qbittorrent
    retroarchFull
    shntool
    skypeforlinux
    tdesktop
    trayer
    unclutter
    unzip
    vifm
    vim
    wget
    xmrig
    xorg.xinit
    zathura
  ];

  # Set system fonts.
  fonts.fonts = with pkgs; [
    cascadia-code
    fira
    fira-code
    font-awesome
    ibm-plex
    jetbrains-mono
    julia-mono
    victor-mono
  ];

#  nixpkgs.config.permittedInsecurePackages = [
#    "python3.9-mistune-0.8.4"
#  ];

  # Allow non-free packages.
  nixpkgs.config.allowUnfree = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  programs = {
    steam.enable = true;
  };

  # List of services that you want to enable.
  services = {
   
  # Enable Emacs for all users.
    emacs = {
      enable = true;
      defaultEditor = true;
    };

    # Enable Gnome Keyring
    gnome.gnome-keyring.enable = true;

    # Enable the OpenSSH daemon.
    openssh.enable = true;

#    # Enable Picom
#    picom = {
#      enable = false;
#    };

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = false;
    };

    # Enable Roon Server
    roon-server = {
      enable = true;
      user = "kacper";
    };

    # Enable Unclutter
    unclutter = {
      enable = true;
      timeout = 3;
    };

  }; # End Services

  security.rtkit.enable = true; # Optional, but recommended, for pipewire.

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
  system.stateVersion = "22.05"; # Did you read the comment?

}

