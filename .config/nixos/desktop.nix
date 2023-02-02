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
    enable = true;
#    displayManager.gdm.enable = true;
#    desktopManager.gnome.enable = true;
    
    # Set drivers and enable "TearFree"
    videoDrivers = [ "intel" ];
    #useGlamor = true;
    deviceSection = ''
      Option "AccelMethod" "sna"
      Option "TearFree" "true"
      Option "DRI" "3"
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
    extraGroups = [ "adbusers" "libvirtd" "lp" "scanner" "wheel" ];
    packages = with pkgs; [
      alacritty
      feh
      lxappearance
      minecraft
      mpv
      mupdf
      nitrogen
      picom
      puddletag
      pulsemixer
      qbittorrent
      retroarchFull
      shntool
      skypeforlinux
      slack
      unclutter
      virt-manager
      xorg.xinit
    ];
  };


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
