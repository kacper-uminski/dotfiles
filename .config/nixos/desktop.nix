{ config, pkgs, lib, ... }:

{
  boot.initrd.kernelModules = [ "amdgpu" ];
  networking = {
    hostName = "nixos";
    defaultGateway = "192.168.50.1";
    nameservers = ["192.168.50.200"];
    interfaces.enp11s0.ipv4.addresses = [{
      address = "192.168.50.250";
      prefixLength = 24;
    }];
  };

  fileSystems."/home/kacper/Media" = {
    device = "192.168.50.200:/mnt/Tank/Media";
    fsType = "nfs";
  };

  services.xserver = {
    enable = true;

    videoDrivers = [ "amdgpu" ];
    deviceSection = ''Option "TearFree" "true"'';
    
    # Enable XMonad.
    displayManager.startx.enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  
  };



  hardware = {
    # Enable SANE to scan documents.
    sane.enable = false;
  };

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
      retroarchFull
      ryujinx
      shntool
      skypeforlinux
      slack
      unclutter
      xorg.xinit
      yuzu-early-access
    ];
  };

  services = {
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

}
