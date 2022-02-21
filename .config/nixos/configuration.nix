# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  stable = import <nixos-stable> { config = { allowUnfree = true; }; };
in {
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      intel-media-driver # LIBVA_DRIVER_NAME=iHD
      vaapiIntel         # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking = {
    useDHCP = false;
    defaultGateway = "192.168.50.1";
    nameservers = ["192.168.50.200"];
    interfaces.eno2.ipv4.addresses = [{
      address = "192.168.50.250";
      prefixLength = 24;
    }];
  };

  # Mount network drives.
  fileSystems."/home/kacper/media/music" = {
    device = "192.168.50.200:/mnt/Tank/Music";
    fsType = "nfs";
  };
  fileSystems."/home/kacper/media/movies" = {
    device = "192.168.50.200:/mnt/Tank/Movies";
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
      extraPackages = haskellPackages: [
        haskellPackages.xmonad_0_17_0
        haskellPackages.xmonad-contrib_0_17_0
        haskellPackages.xmonad-extras_0_17_0
      ];
    };
  
    # Configure keymap in X11
    layout = "us";
    xkbVariant = "dvorak";
    xkbOptions = "ctrl:swapcaps";
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

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
    afetch
    alacritty
    brave
    cifs-utils
    darktable
    emacs
    exa
    ffmpeg
    firefox
    flac
    flameshot
    ghc
    git
    haskellPackages.xmobar
    hdparm
    htop
    imagemagick
    libsForQt5.qtstyleplugin-kvantum
    lm_sensors
    lxappearance
    minecraft
    mpv
    mupdf
    neofetch
    nitrogen
    picom
    pstree
    stable.puddletag
    pulsemixer
    qbittorrent
    retroarchFull
    shntool
    skypeforlinux
    sxiv
    tdesktop
    texlive.combined.scheme-full
    trayer
    unclutter
    unzip
    vifm
    vim
    wget
    xmrig
    xorg.xinit
    zoom-us
  ];
  

  # Set system fonts.
  fonts.fonts = with pkgs; [
  fira
  fira-code
  font-awesome
  ];

  # Allow non-free packages.
  nixpkgs.config.allowUnfree = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    }; 

    java.enable = true;

    steam.enable = true;
  };


  # List services that you want to enable:
  services = {

    # Enable Emacs for users.
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

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?

}

