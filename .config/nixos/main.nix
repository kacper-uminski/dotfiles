{ config, pkgs, ... }:

{
  # Use the systemd-boot EFI boot loader.
  boot.loader.efi.efiSysMountPoint = "/efi";
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

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
  fonts.fonts = with pkgs; [
    _3270font
    fira
    fira-code
    font-awesome
    ibm-plex
    iosevka
    jetbrains-mono
    julia-mono
    victor-mono
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

  programs = {

    steam.enable = true;

    zsh = {
      enable = true;

      shellAliases = {
        dotfiles = "git --git-dir=$HOME/software/dotfiles --work-tree=$HOME";
        emc = "emacsclient -nw";
        ls = "exa -al";
        mupdf = "mupdf-x11";
        sacd_extract = "$HOME/software/sacd_extract/result/bin/sacd_extract";
      };
    };
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
  system.stateVersion = "22.05"; # Did you read the comment?

}
