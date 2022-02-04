# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  # Configure AMD Radeon HD7850.
  boot.kernelParams = [ "radeon.si_support=0" "amdgpu.si_support=1" ];

  # Configure OpenCL.
  # hardware.opengl.extraPackages = with pkgs; [
  #  rocm-opencl-icd
  #  rocm-opencl-runtime
  # ];

  fileSystems."/home/kacper/media/music" = {
  device = "192.168.50.200:/mnt/Tank/Music";
  fsType = "nfs";
  };

  fileSystems."/home/kacper/media/movies" = {
  device = "192.168.50.200:/mnt/Fast/Movies";
  fsType = "nfs";
  };

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp3s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "dvorak";
  };
  
  services.xserver = {
    # Enable the X11 windowing system.
    enable = true;
    videoDrivers = [ "amdgpu" ];

    # Monitor Configuration
    xrandrHeads = [
      { output = "DisplayPort-0"; primary = true; }
    #   { output = "DVI-I-1"; monitorConfig = "Option \"leftof\" \"DisplayPort-0\""; }
    ];
    
    # Enable Xmonad
    displayManager.startx.enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

    # Enable Gnome
    # displayManager.gdm.enable = true;
    # desktopManager.gnome.enable = true;

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

  # Enable Bluetooth.
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.kacper = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    alacritty
    brave
    cifs-utils
    darktable
    emacs
    exa
    feh
    ffmpeg
    firefox
    flacon
    flameshot
    ghc
    git
    gnome.gnome-keyring
    haskellPackages.xmobar
    hdparm
    htop
    jdk
    killall
    lm_sensors
    lxappearance
    minecraft
    mpv
    mupdf
    neofetch
    nfs-utils
    picom
    puddletag
    pulsemixer
    qbittorrent
    radeontop
    retroarchFull
    rustc
    skypeforlinux
    sxiv
    tdesktop
    texlive.combined.scheme-full
    unclutter
    unzip
    vifm
    vim
    wget
    xmrig
    xorg.xinit
    xorg.xwd
    zoom-us
  ];

  fonts.fonts = with pkgs; [
    fira
    fira-code
    font-awesome
  ];

  nixpkgs.config.allowUnfree = true;
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

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
  system.stateVersion = "21.05"; # Did you read the comment?

}

