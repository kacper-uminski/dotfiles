{ config, pkgs, ... }:

{
  # Configure networking.
  networking = {
    hostName = "nixos";
    networkmanager.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.kacper = {
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
  environment = {
    systemPackages = with pkgs; [
      (let
        my-python-packages = python-packages: with python-packages; [
          (opencv4.override { enableGtk2 = true; })
          python-lsp-server
        ];
        python-with-my-packages = python3.withPackages my-python-packages;
      in
        python-with-my-packages)
      cargo
      chromium
      darktable
      exa
      feh
      ffmpeg
      firefox
      gcc
      ghc
      git
      gnome.gnome-tweaks
      gnomeExtensions.blur-my-shell
      htop
      imagemagick
      libsForQt5.qtstyleplugin-kvantum
      neofetch
      p7zip
      retroarchFull
      rustc
      shntool
      slack
      slack-term
      tdesktop
      texlive.combined.scheme-full
      unclutter
      unzip
      vifm
      vim
      wget
      xmrig
    ];

    gnome.excludePackages = (with pkgs; [
      baobab
      gnome-connections
      gnome-photos
      gnome-text-editor
      gnome-tour
    ]) ++ (with pkgs.gnome; [
      cheese # webcam tool
      epiphany # web browser
      #evince # document viewer
      geary # email reader
      gedit # text editor
      #gnome-calculator
      gnome-calendar
      gnome-characters
      gnome-clocks
      gnome-contacts
      gnome-disk-utility
      gnome-font-viewer
      gnome-logs
      gnome-maps
      gnome-music
      gnome-screenshot
      gnome-system-monitor
      #gnome-terminal
      gnome-weather
      simple-scan
      #totem # video player
      yelp # help viewer

      # Games
      atomix # puzzle game
      hitori # sudoku game
      iagno # go game
      tali # poker game
    ]);

  };
}
