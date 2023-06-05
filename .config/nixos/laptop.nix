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
    packages = with pkgs; [
      gnome.gnome-tweaks
      gnomeExtensions.blur-my-shell
    ];
  };

  services = {
    # Flatpak
    flatpak.enable = true;

    # X11
    xserver = {
      # Enable the GNOME Desktop Environment.
      displayManager.gdm.enable = true;
      desktopManager.gnome.enable = true;

      # Enable touchpad support (enabled default in most desktopManager).
      libinput.enable = true;
    };
  };

  # Excluded packages from gnome.
  environment.gnome.excludePackages = (with pkgs; [
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

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?
}
