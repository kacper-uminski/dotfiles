{ config, pkgs, ... }:

{
  # Bootloader.
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # Configure networking.
  networking = {
    hostName = "nixos";
    networkmanager.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.kacper = {
    description = "Kacper Uminski";
    extraGroups = [ "networkmanager" "wheel" ];
  };

  # X11
  services.xserver = {
    # Enable the GNOME Desktop Environment.
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;

    # Enable touchpad support (enabled default in most desktopManager).
    libinput.enable = true;
  };

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [
    (let
      my-python-packages = python-packages: with python-packages; [
        (opencv4.override { enableGtk2 = true; })
      ];
      python-with-my-packages = python3.withPackages my-python-packages;
    in
    python-with-my-packages)
    chromium
    darktable
    emacs
    exa
    feh
    ffmpeg
    firefox
    ghc
    git
    gnome.gnome-tweaks
    htop
    imagemagick
    libsForQt5.qtstyleplugin-kvantum
    # minecraft
    mpv
    mupdf
    neofetch
    p7zip
    puddletag
    qbittorrent
    retroarchFull
    shntool
    tdesktop
    texlive.combined.scheme-full
    unclutter
    unzip
    vifm
    vim
    wget
    xmrig
    zathura
  ];
}
