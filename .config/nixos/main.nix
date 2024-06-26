{ pkgs, lib, ... }:

{
  nix.settings.experimental-features = [ "flakes" "nix-command" ];
  boot = {
    # Enable splash-screen
    plymouth.enable = false;

    # Use the systemd-boot EFI boot loader
    loader = {
      systemd-boot.enable = true;
      efi = {
        efiSysMountPoint = "/boot";
        canTouchEfiVariables = true;
      };
    };
  };

  # Set your time zone
  time.timeZone = "Europe/Stockholm";

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "sv_SE.UTF-8";
      LC_IDENTIFICATION = "sv_SE.UTF-8";
      LC_MEASUREMENT = "sv_SE.UTF-8";
      LC_MONETARY = "sv_SE.UTF-8";
      LC_NAME = "sv_SE.UTF-8";
      LC_NUMERIC = "sv_SE.UTF-8";
      LC_PAPER = "sv_SE.UTF-8";
      LC_TELEPHONE = "sv_SE.UTF-8";
      LC_TIME = "en_US.UTF-8";
    };
  };

  console = {
    keyMap = "dvorak";
  };

  # Home Manager
  home-manager = {

    useGlobalPkgs = true;

    users.kacper = {pkgs, ...}: {
      home = {
        username = "kacper";
        homeDirectory = "/home/kacper";
        shellAliases = {
          cat = "bat";
          dotfiles = "git --git-dir=$HOME/Software/dotfiles --work-tree=$HOME";
          emc = "emacsclient -nw";
          vim = "nvim";
          week = "date +%V";
          xmrig = "sudo xmrig -c $HOME/.config/xmrig.json";
        };
      };

      gtk = {
        enable = true;
        cursorTheme.name = "Breeze";
        theme = {
          package = pkgs.adw-gtk3;
          name = "adw-gtk3-dark";
        };
      };

      programs = {

        alacritty = {
          enable = true;
          settings = {
            font = {
              normal.family = "IBM Plex Mono";
              size = 12;
            };
            window = {
              opacity = 0.8;
            };
          };
        };

        bash = {
          enable = true;
          enableCompletion = true;
          historyFile = ".config/bash/bash_history";
        };

        bat.enable = true;

        chromium = {
          enable = false;
          commandLineArgs = [
            "--ozone-platform-hint=auto"
            "--enable-features=TouchpadOverscrollHistoryNavigation"
          ];
          package = pkgs.chromium;
        };

        emacs = {
          enable = true;
          package = pkgs.emacs29-pgtk;
        };

        eza = {
          enable = true;
          git = true;
          icons = true;
        };

        feh.enable = true;

        git = {
          enable = true;
          userName = "kacper-uminski";
          userEmail = "kacperuminski@protonmail.com";
        };

        gitui.enable = true;

        helix = {
          enable = true;
          settings = {
            theme = "base16_transparent";
            editor = {
              lsp.display-messages = true;
              cursor-shape.insert = "bar";
            };
          };
        };

        htop.enable = true;

        mpv = {
          enable = true;
          config = {
            hwdec = "auto-safe";
            vo = "gpu";
            profile = "gpu-hq";
          };
        };

        neovim.enable = true;

        ripgrep.enable = true;

        rofi = {
          enable = true;
          package = pkgs.rofi-wayland;
          terminal = "${pkgs.alacritty}/bin/alacritty";
        };

        ssh = {
          enable = true;
          matchBlocks = {

            liu = {
              forwardX11 = true;
              forwardX11Trusted = true;
              hostname = "ssh.edu.liu.se";
              setEnv = {
                TERM = "xterm-256color";
              };
              user = "kacum383";
            };

            "muxen?-???" = {
              forwardX11 = true;
              forwardX11Trusted = true;
              hostname = "%h.ad.liu.se";
              proxyJump = "liu";
              setEnv = {
                TERM = "xterm-256color";
              };
              user = "kacum383";
            };

            "shipon totoro viridian" = {
              forwardX11 = true;
              forwardX11Trusted = true;
              hostname = "%h.lysator.liu.se";
              setEnv = {
                TERM = "xterm-256color";
              };
              user = "kacper";
            };

            "ssh.edu.liu.se" = {
              match = ''host ssh.edu.liu.se !exec "nc -zw1 %h 22"'';
              proxyJump = "viridian";
            };
          };
        };

        starship = {
          enable = true;
          settings = {
            add_newline = false;
            format = lib.concatStrings [
              "$all"
              "$directory"
              "$character"
            ];
          };
        };

        zathura = {
          enable = true;
          options = {
            default-bg = "#000000";
            default-fg = "#FFFFFF";
            guioptions = "none";
            recolor = true;
          };
        };

        zoxide = {
          enable = true;
          options = [
            "--cmd cd"
          ];
        };

        zsh = {
          enable = true;
          autosuggestion.enable = true;
          enableCompletion = true;
          syntaxHighlighting.enable = true;
          dotDir = ".config/zsh";
          history.path = "$ZDOTDIR/zsh_history";
          loginExtra = "if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then Hyprland; fi";
        };
      };

      qt = {
        enable = true;
        platformTheme = "gtk3";
      };

      services = {
        dunst.enable = true;

        emacs = {
          # Enable emacs daemon.
          enable = true;
          defaultEditor = true;
        };

        gnome-keyring.enable = true;
      };

      wayland.windowManager.hyprland = {
        enable = true;
        settings = {
          "$term" = "alacritty";
          "$mod" = "SUPER";
          bind = [
            # Go to workspace #n
            "$mod, 1, workspace, 1" 
	          "$mod, 2, workspace, 2" 
	          "$mod, 3, workspace, 3"
            "$mod, 4, workspace, 4"
            "$mod, 5, workspace, 5"
            "$mod, 6, workspace, 6"
            "$mod, 7, workspace, 7"
            "$mod, 8, workspace, 8"
            "$mod, 9, workspace, 9"
            "$mod, 0, workspace, 10"

            # Move to workspace #n
            "$mod SHIFT, 1, movetoworkspace, 1"
            "$mod SHIFT, 2, movetoworkspace, 2"
            "$mod SHIFT, 3, movetoworkspace, 3"
            "$mod SHIFT, 4, movetoworkspace, 4"
            "$mod SHIFT, 5, movetoworkspace, 5"
            "$mod SHIFT, 6, movetoworkspace, 6"
            "$mod SHIFT, 7, movetoworkspace, 7"
            "$mod SHIFT, 8, movetoworkspace, 8"
            "$mod SHIFT, 9, movetoworkspace, 9"
            "$mod SHIFT, 0, movetoworkspace, 10"

            # Window Cycling and Moving
            "$mod, J, cyclenext"
            "$mod, K, cyclenext, prev"
            "$mod SHIFT, J, swapnext"
            "$mod SHIFT, K, swapnext, prev"

            # Fullscreen
            "$mod, F, fullscreen"

            # Start Programs
            "$mod, R, exec, rofi -show drun -show-icons"
            "$mod, E, exec, emacs"
            "$mod, B, exec, firefox"
            "$mod, T, exec, $term"

            # Kill Current Window
            "$mod, W, killactive"

            # Exit
            "$mod SHIFT, Q, exit"
          ];

          bindm = [
            # Moving windows by mouse
            "$mod, mouse:272, movewindow"
            "$mod, mouse:273, resizewindow"
          ];

          general = {
            cursor_inactive_timeout = 5;
          };

          input = {
            kb_layout = "us, se, pl";
            kb_variant = "dvorak, dvorak, dvorak";
            kb_options = "caps:ctrl_modifier";
          };

          dwindle = {
            no_gaps_when_only = 1;
          };

          misc = {
            disable_hyprland_logo = true;
            disable_splash_rendering = true;
            enable_swallow = true;
            swallow_regex = "^(Alacritty)$";
          };
        };
      };
    };
  };

  # Enable Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Pipewire
  sound.enable = true;
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
  fonts.packages = with pkgs; [
    #barlow
    # fira
    # fira-code
    font-awesome
    #garamond-libre
    ibm-plex
    # jetbrains-mono
    nerdfonts
  ];

  # Allow non-free packages.
  nixpkgs.config = {
    allowUnfree = true; 
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  environment = {

    pathsToLink = [
      "/share/bash-completion"
      "/share/zsh"
    ];

    sessionVariables = {
      NIXOS_OZONE_WL = "1";
    };

    # List packages installed in system profile.
    systemPackages = with pkgs; [
      # Python packages
      (let my-python-packages = python-packages: with python-packages; [
             flake8
             matplotlib
             mypy
             pylsp-mypy
             python-lsp-server
             sympy
           ];
           python-with-my-packages = python3.withPackages my-python-packages;
       in
         python-with-my-packages)
      
      # Haskell packages
      (let myGhc = pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
             haskell-language-server
             # implicit-hie
             # lists
             # split
             # stack
             # Unique
           ]);
       in
         myGhc)
      aspell
      aspellDicts.sv
      bat
      bear # Allows LSP to find #include non-std files and headers.
      cargo-binutils
      cbqn
      # cifs-utils
      clang-tools_17
      elixir
      elixir-ls
      erlang
      erlang-ls
      evince
      exercism
      ffmpeg
      file
      firefox
      ghdl
      # gnuapl
      # gnuplot
      # gradience
      gradience
      home-manager
      jetbrains.idea-community
      julia-bin
      msr # Used by xmrig.
      nil
      octaveFull
      rebar3 # Erlang build system.
      rustup
      sshfs
      swww # Wallpaper daemon for wayland.
      typst
      typstfmt
      typst-lsp
      uiua
      unzip
      usbutils
      # valgrind # Memory profiler for C/C++
      vhdl-ls
      vifm
      wget
      wiki-tui
      wlr-randr
    ];
  }; # End Environment

  programs = {
    adb.enable = false;

    hyprland = {
      enable = true;
      xwayland.enable = true;
    };

    zsh.enable = true;
  };

  # List of services that you want to enable.
  services = {
    # Enable Gnome Keyring
    gnome.gnome-keyring.enable = true;

    # Enable the OpenSSH daemon.
    openssh.enable = true;
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
}
