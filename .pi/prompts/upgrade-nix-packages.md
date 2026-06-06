---
description: 更新 Nix 包，并总结更新内容
---

帮我更新 Nix 目录下的版本。版本主要记录在 sources.json 中，是使用 niv 管理的。

具体的更新流程如下：
0. 开始之前，先 `home-manager -f home.nix build` 一遍生成 result 目录，并将其重命名为 result.old
1. 使用 niv 来更新 sources.json 里每一个 repo 的版本。
2. 注意，里面有一些包指定的是特定的 tag（例如 v1.1），我希望找到它最新的 tag 并更新过去，而不是直接切到 master。
3. 对于其他没有指定 tag 而是指向 branch 的包，直接更新到该 branch 的最新commit即可。
4. 用 niv 更新完 sources 之后，可以使用 Home Manager 尝试 build 一下。
5. build 过程中，如果出现 digest 或者 hash 不对的情况，就在 home.nix 里面照常更新。
6. 在新 build 运行完后，使用 nix store diff-closures 命令查看所有更新的结果。

最后，请帮我总结一下这次更新的主要内容，特别是大版本的更新、Breaking Changes 或者包大小变化非常大的部分，请详细列出来。
