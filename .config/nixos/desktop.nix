{ config, pkgs, ... }:

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
    
    # Set drivers and enable "TearFree"
    videoDrivers = [ "intel" ];
#    useGlamor = true;
    deviceSection = ''
      Option "AccelMethod" "sna"
      Option "TearFree" "true"
      Option "DRI" "3"
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
  
  };

  # Enable SANE to scan documents.
  hardware.sane.enable = true;

  users.users.kacper = {
    extraGroups = [ "libvirtd" "lp" "scanner" "wheel" ];
  };

  environment = {

    variables = {
      QT_STYLE_OVERRIDE="kvantum";
    };

    # List packages installed in system profile.
    systemPackages = with pkgs; [
      (let
        my-python-packages = python-packages: with python-packages; [
          flake8
          mypy
          #(opencv4.override { enableGtk2 = true; })
          pygame
          pylsp-mypy
          pymunk
          python-lsp-server
        ];
        python-with-my-packages = python3.withPackages my-python-packages;
      in
        python-with-my-packages)
      alacritty
      cargo
      cbqn
      chromium
      cifs-utils
      darktable
      emacs28Packages.bqn-mode
      exa
      feh
      ffmpeg
      file
      firefox
      fish
      flameshot
      flatpak
      gcc
      ghc
      git
      gnuapl
      haskell-language-server
      haskellPackages.xmobar
      htop
      imagemagick
      jdk
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
      ranger
      rar
      retroarchFull
      shntool
      skypeforlinux
      slack
      slack-term
      swiProlog
      tdesktop
      texlive.combined.scheme-full
      trayer
      unclutter
      unzip
      usbutils
      vifm
      vim
      virt-manager
      wget
      xmrig
      xorg.xkill
      xorg.xinit
    ];
  }; # End Environment

  services = {

    # Enable Gnome Keyring
    gnome.gnome-keyring.enable = true;

    # Enable Roon Server
    roon-server = {
      enable = false;
      user = "kacper";
    };
  }; # End Services.

  # Virtualization
  virtualisation = {
    libvirtd.enable = true;
    spiceUSBRedirection.enable = true;
  };
  programs.dconf.enable = true;

}
