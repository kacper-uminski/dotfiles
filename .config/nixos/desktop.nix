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
    #useGlamor = true;
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
    extraGroups = [ "adbusers" "libvirtd" "lp" "scanner" "wheel" ];
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
      appimage-run
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

    udev.extraRules = ''
SUBSYSTEM=="usb", ATTRS{idVendor}=="0e79", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="0502", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="0b05", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="413c", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="0489", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="091e", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="18d1", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="0bb4", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="12d1", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="24e3", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="2116", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="0482", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="17ef", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="1004", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="22b8", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="0409", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="2080", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="0955", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="2257", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="10a9", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="1d4d", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="0471", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="04da", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="05c6", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="1f53", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="04e8", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="04dd", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="0fce", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="0930", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="19d2", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="2ae5", MODE="0666", GROUP="plugdev"
SUBSYSTEM=="usb", ATTRS{idVendor}=="2a45", MODE="0666", GROUP="plugdev"
'';
  }; # End Services.

  # Virtualization
  virtualisation = {
    libvirtd.enable = true;
    spiceUSBRedirection.enable = true;
  };
  programs.dconf.enable = true;

}
