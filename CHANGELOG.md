## Update 2023-06-06
```go
Version changes:
[U.]  #1  delta        0.15.1 -> 0.16.5
[U.]  #2  gopls        0.12.0 -> 0.12.2
[U.]  #3  imagemagick  7.1.1-10 -> 7.1.1-11
[U.]  #4  rootlesskit  1.1.0 -> 1.1.1
Closure size: 517 -> 517 (15 paths added, 15 paths removed, delta +0, disk usage -364.7KiB).
```
### lock change:
```
warning: updating lock file '/home/runner/work/nixpkgs/nixpkgs/flake.lock':
• Updated input 'asdf-plugins':
    'github:asdf-vm/asdf-plugins/a2d3dea325e30de10ee30adaf3988f24b327758f' (2023-05-31)
  → 'github:asdf-vm/asdf-plugins/c8d464cf72a71b4be5f89ed24beec9ecc18eefdf' (2023-06-04)
• Updated input 'darwin':
    'github:LnL7/nix-darwin/b8c286c82c6b47826a6c0377e7017052ad91353c' (2023-05-22)
  → 'github:LnL7/nix-darwin/4338bc869e9874d54a4c89539af72f16666b2abe' (2023-05-31)
• Updated input 'doomemacs':
    'github:doomemacs/doomemacs/b66ad7703134d8a91e8ae1fab84af3bb90fcca03' (2023-03-17)
  → 'github:doomemacs/doomemacs/07fca786154551f90f36535bfb21f8ca4abd5027' (2023-05-30)
• Updated input 'emacs-overlay':
    'github:nix-community/emacs-overlay/fe9ad516f6404f8ce5fffd3955a29418eb82347f' (2023-06-01)
  → 'github:nix-community/emacs-overlay/f8704de7d8dd727af1b180bcf6166f5c40e4063a' (2023-06-06)
• Updated input 'emacs-overlay/nixpkgs':
    'github:NixOS/nixpkgs/5e871d8aa6f57cc8e0dc087d1c5013f6e212b4ce' (2023-05-29)
  → 'github:NixOS/nixpkgs/7409480d5c8584a1a83c422530419efe4afb0d19' (2023-06-05)
• Updated input 'emacs-overlay/nixpkgs-stable':
    'github:NixOS/nixpkgs/9af373a61647257d16ae6062cddaa9094d24920c' (2023-05-30)
  → 'github:NixOS/nixpkgs/d4a9ff82fc18723219b60c66fb2ccb0734c460eb' (2023-06-04)
• Updated input 'forgit':
    'github:wfxr/forgit/e0ca1f40452c786c3a27a0861e6b0ed8a8827edb' (2022-09-13)
  → 'github:wfxr/forgit/665e3fd215fe68ad066af1ad732e8618990da5a6' (2023-05-31)
• Updated input 'home-manager':
    'github:nix-community/home-manager/58eb968c21d309a6c2b020ea8d64e25c38ceebba' (2023-05-23)
  → 'github:nix-community/home-manager/28614ed7a1e3ace824c122237bdc0e5e0b62c5c3' (2023-06-05)
• Updated input 'home-manager/nixpkgs':
    'github:nixos/nixpkgs/90cd5459a1fd707819b9a3fb9c852beaaac3b79a' (2022-06-11)
  → 'github:nixos/nixpkgs/e635192892f5abbc2289eaac3a73cdb249abaefd' (2023-06-01)
• Updated input 'nixpkgs-master':
    'github:NixOS/nixpkgs/06adb25e4776b6f9ed3450c331a0402c9cb6149f' (2023-06-01)
  → 'github:NixOS/nixpkgs/e0e32fbaf5f75cddfe8c3091d8b0c5acef323a2c' (2023-06-06)
• Updated input 'nixpkgs-stable':
    'github:NixOS/nixpkgs/ac5281cce24fac6d98773c55c038f654b8e84cd4' (2023-06-02)
  → 'github:NixOS/nixpkgs/c9ae0a972d9a5b489ed45f26b115028daa91ec41' (2023-06-06)
• Updated input 'nixpkgs-unstable':
    'github:NixOS/nixpkgs/bc3ec5eaa759d58e9fb1bdc9cfe94f74d0331b31' (2023-05-31)
  → 'github:NixOS/nixpkgs/31cd1b4afbaf0b1e81272ee9c31d1ab606503aed' (2023-06-05)
• Updated input 'powerlevel10k':
    'github:romkatv/powerlevel10k/3ce7bac4ff2e07b9f1182c7bf7a1cac7c7ffdf9e' (2023-05-27)
  → 'github:romkatv/powerlevel10k/79753faacb6dc511088cb0d136ec438873613932' (2023-06-04)
• Updated input 'spacemacs':
    'github:syl20bnr/spacemacs/d5348991aa97ec5e9e09083f20cd117d075fe59b' (2023-05-29)
  → 'github:syl20bnr/spacemacs/2cb4f092f2b8920cbb23d97757ac1802a39a1c82' (2023-06-03)
warning: Git tree '/home/runner/work/nixpkgs/nixpkgs' is dirty
```


## Update 2023-06-12
```go
Version changes:
[U.]  #01  coreutils    9.1 -> 9.3
[U.]  #02  cups         2.4.2-lib -> 2.4.3-lib
[C.]  #03  gcc          12.2.0, 12.2.0-lib x3, 12.2.0-libgcc x3 -> 12.2.0-lib, 12.2.0-libgcc, 12.3.0, 12.3.0-lib x2, 12.3.0-libgcc x2
[U.]  #04  gettext      0.21 -> 0.21.1
[U.]  #05  gfortran     12.2.0-lib, 12.2.0-libgcc -> 12.3.0-lib, 12.3.0-libgcc
[U.]  #06  go           1.19.9 -> 1.19.10
[U.]  #07  libapparmor  3.1.3 -> 3.1.4
[U.]  #08  libgccjit    12.2.0 -> 12.3.0
[C.]  #09  libssh2      1.10.0 x2 -> 1.10.0, 1.11.0
[C.]  #10  nix          2.13.3, 2.13.3-man, 2.14.1 x2, 2.14.1-man x2 -> 2.14.1 x2, 2.14.1-man x2, 2.15.1, 2.15.1-man
[C.]  #11  openssl      3.0.8 x3, 3.0.8-bin, 3.0.8-dev -> 3.0.8, 3.0.9 x2, 3.0.9-bin, 3.0.9-dev
[U.]  #12  pnpm         8.6.0 -> 8.6.1
[C.]  #13  python3      3.10.11, 3.10.11-env, 3.9.16, 3.9.16-env -> 3.10.11, 3.10.11-env, 3.9.17, 3.9.17-env
[C.]  #14  sqlite       3.41.2 x2 -> 3.41.2, 3.42.0
[C.]  #15  xgcc         12.2.0-libgcc x2 -> 12.2.0-libgcc, 12.3.0-libgcc
Closure size: 517 -> 517 (465 paths added, 465 paths removed, delta +0, disk usage +994.7KiB).
```
### lock change:
```
warning: updating lock file '/home/runner/work/nixpkgs/nixpkgs/flake.lock':
• Updated input 'asdf-plugins':
    'github:asdf-vm/asdf-plugins/c8d464cf72a71b4be5f89ed24beec9ecc18eefdf' (2023-06-04)
  → 'github:asdf-vm/asdf-plugins/88007e10ff4f67041e5000efc4373af125e83f74' (2023-06-11)
• Updated input 'darwin':
    'github:LnL7/nix-darwin/4338bc869e9874d54a4c89539af72f16666b2abe' (2023-05-31)
  → 'github:LnL7/nix-darwin/7c16d31383a90e0e72ace0c35d2d66a18f90fb4f' (2023-06-09)
• Updated input 'emacs-overlay':
    'github:nix-community/emacs-overlay/f8704de7d8dd727af1b180bcf6166f5c40e4063a' (2023-06-06)
  → 'github:nix-community/emacs-overlay/bfe8be184d1ca2c3bdfdb07b13d855d2e7fde040' (2023-06-12)
• Updated input 'emacs-overlay/nixpkgs':
    'github:NixOS/nixpkgs/7409480d5c8584a1a83c422530419efe4afb0d19' (2023-06-05)
  → 'github:NixOS/nixpkgs/21951114383770f96ae528d0ae68824557768e81' (2023-06-10)
• Updated input 'emacs-overlay/nixpkgs-stable':
    'github:NixOS/nixpkgs/d4a9ff82fc18723219b60c66fb2ccb0734c460eb' (2023-06-04)
  → 'github:NixOS/nixpkgs/85bcb95aa83be667e562e781e9d186c57a07d757' (2023-06-09)
• Updated input 'fzf-tab':
    'github:Aloxaf/fzf-tab/5a81e13792a1eed4a03d2083771ee6e5b616b9ab' (2023-03-19)
  → 'github:Aloxaf/fzf-tab/c2b4aa5ad2532cca91f23908ac7f00efb7ff09c9' (2023-06-11)
• Updated input 'home-manager':
    'github:nix-community/home-manager/28614ed7a1e3ace824c122237bdc0e5e0b62c5c3' (2023-06-05)
  → 'github:nix-community/home-manager/0144ac418ef633bfc9dbd89b8c199ad3a617c59f' (2023-06-10)
• Updated input 'home-manager/nixpkgs':
    'github:nixos/nixpkgs/e635192892f5abbc2289eaac3a73cdb249abaefd' (2023-06-01)
  → 'github:nixos/nixpkgs/7409480d5c8584a1a83c422530419efe4afb0d19' (2023-06-05)
• Updated input 'nixpkgs-master':
    'github:NixOS/nixpkgs/e0e32fbaf5f75cddfe8c3091d8b0c5acef323a2c' (2023-06-06)
  → 'github:NixOS/nixpkgs/76ad192fe16c879cedc161e20c1f7bccb906ea34' (2023-06-12)
• Updated input 'nixpkgs-stable':
    'github:NixOS/nixpkgs/c9ae0a972d9a5b489ed45f26b115028daa91ec41' (2023-06-06)
  → 'github:NixOS/nixpkgs/63e752bbac363c48481fe054339e35d636dc2694' (2023-06-12)
• Updated input 'nixpkgs-unstable':
    'github:NixOS/nixpkgs/31cd1b4afbaf0b1e81272ee9c31d1ab606503aed' (2023-06-05)
  → 'github:NixOS/nixpkgs/9401a0c780b49faf6c28adf55764f230301d0dce' (2023-06-11)
• Updated input 'powerlevel10k':
    'github:romkatv/powerlevel10k/79753faacb6dc511088cb0d136ec438873613932' (2023-06-04)
  → 'github:romkatv/powerlevel10k/944f52fc430259ff49f497f3516a3ddfb45a0a6b' (2023-06-11)
• Updated input 'spacemacs':
    'github:syl20bnr/spacemacs/2cb4f092f2b8920cbb23d97757ac1802a39a1c82' (2023-06-03)
  → 'github:syl20bnr/spacemacs/d0d0141600086f3717489a3925f85cdac927df23' (2023-06-11)
warning: Git tree '/home/runner/work/nixpkgs/nixpkgs' is dirty
```

## Update 2023-06-06
```go
Version changes:
[U.]  #1  delta        0.15.1 -> 0.16.5
[U.]  #2  gopls        0.12.0 -> 0.12.2
[U.]  #3  imagemagick  7.1.1-10 -> 7.1.1-11
[U.]  #4  rootlesskit  1.1.0 -> 1.1.1
Closure size: 517 -> 517 (15 paths added, 15 paths removed, delta +0, disk usage -364.7KiB).
```
### lock change:
```
warning: updating lock file '/home/runner/work/nixpkgs/nixpkgs/flake.lock':
• Updated input 'asdf-plugins':
    'github:asdf-vm/asdf-plugins/a2d3dea325e30de10ee30adaf3988f24b327758f' (2023-05-31)
  → 'github:asdf-vm/asdf-plugins/c8d464cf72a71b4be5f89ed24beec9ecc18eefdf' (2023-06-04)
• Updated input 'darwin':
    'github:LnL7/nix-darwin/b8c286c82c6b47826a6c0377e7017052ad91353c' (2023-05-22)
  → 'github:LnL7/nix-darwin/4338bc869e9874d54a4c89539af72f16666b2abe' (2023-05-31)
• Updated input 'doomemacs':
    'github:doomemacs/doomemacs/b66ad7703134d8a91e8ae1fab84af3bb90fcca03' (2023-03-17)
  → 'github:doomemacs/doomemacs/07fca786154551f90f36535bfb21f8ca4abd5027' (2023-05-30)
• Updated input 'emacs-overlay':
    'github:nix-community/emacs-overlay/fe9ad516f6404f8ce5fffd3955a29418eb82347f' (2023-06-01)
  → 'github:nix-community/emacs-overlay/f8704de7d8dd727af1b180bcf6166f5c40e4063a' (2023-06-06)
• Updated input 'emacs-overlay/nixpkgs':
    'github:NixOS/nixpkgs/5e871d8aa6f57cc8e0dc087d1c5013f6e212b4ce' (2023-05-29)
  → 'github:NixOS/nixpkgs/7409480d5c8584a1a83c422530419efe4afb0d19' (2023-06-05)
• Updated input 'emacs-overlay/nixpkgs-stable':
    'github:NixOS/nixpkgs/9af373a61647257d16ae6062cddaa9094d24920c' (2023-05-30)
  → 'github:NixOS/nixpkgs/d4a9ff82fc18723219b60c66fb2ccb0734c460eb' (2023-06-04)
• Updated input 'forgit':
    'github:wfxr/forgit/e0ca1f40452c786c3a27a0861e6b0ed8a8827edb' (2022-09-13)
  → 'github:wfxr/forgit/665e3fd215fe68ad066af1ad732e8618990da5a6' (2023-05-31)
• Updated input 'home-manager':
    'github:nix-community/home-manager/58eb968c21d309a6c2b020ea8d64e25c38ceebba' (2023-05-23)
  → 'github:nix-community/home-manager/28614ed7a1e3ace824c122237bdc0e5e0b62c5c3' (2023-06-05)
• Updated input 'home-manager/nixpkgs':
    'github:nixos/nixpkgs/90cd5459a1fd707819b9a3fb9c852beaaac3b79a' (2022-06-11)
  → 'github:nixos/nixpkgs/e635192892f5abbc2289eaac3a73cdb249abaefd' (2023-06-01)
• Updated input 'nixpkgs-master':
    'github:NixOS/nixpkgs/06adb25e4776b6f9ed3450c331a0402c9cb6149f' (2023-06-01)
  → 'github:NixOS/nixpkgs/e0e32fbaf5f75cddfe8c3091d8b0c5acef323a2c' (2023-06-06)
• Updated input 'nixpkgs-stable':
    'github:NixOS/nixpkgs/ac5281cce24fac6d98773c55c038f654b8e84cd4' (2023-06-02)
  → 'github:NixOS/nixpkgs/c9ae0a972d9a5b489ed45f26b115028daa91ec41' (2023-06-06)
• Updated input 'nixpkgs-unstable':
    'github:NixOS/nixpkgs/bc3ec5eaa759d58e9fb1bdc9cfe94f74d0331b31' (2023-05-31)
  → 'github:NixOS/nixpkgs/31cd1b4afbaf0b1e81272ee9c31d1ab606503aed' (2023-06-05)
• Updated input 'powerlevel10k':
    'github:romkatv/powerlevel10k/3ce7bac4ff2e07b9f1182c7bf7a1cac7c7ffdf9e' (2023-05-27)
  → 'github:romkatv/powerlevel10k/79753faacb6dc511088cb0d136ec438873613932' (2023-06-04)
• Updated input 'spacemacs':
    'github:syl20bnr/spacemacs/d5348991aa97ec5e9e09083f20cd117d075fe59b' (2023-05-29)
  → 'github:syl20bnr/spacemacs/2cb4f092f2b8920cbb23d97757ac1802a39a1c82' (2023-06-03)
warning: Git tree '/home/runner/work/nixpkgs/nixpkgs' is dirty
```
