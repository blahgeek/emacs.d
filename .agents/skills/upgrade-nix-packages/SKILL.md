---
name: upgrade-nix-packages
description: 更新nix home-manager管理的包版本（记录在nix/home.nix中），并build和输出更新怎解
---

当前系统使用nix home-manager管理，配置文件在`nix/home.nix`中。

这个配置文件中，为了保证可复现性，所有版本都是使用commit或者release锁死的。更新版本时，需要根据github上最新的信息，更新为最新的commit。

需要更新的部分：

1. nixpkgs
2. emacs-overlay
3. flake-compat
4. kimi-cli

不要更新其他部分！！

更新文件之后，使用`home-manager -f home.nix build`构建到一个临时目录；(build的时间可能很长（几十分钟），记得不要限制超时)

最后使用`nix store diff-closures`总结更新的状态并输出。

这样就完成了，不要switch。
