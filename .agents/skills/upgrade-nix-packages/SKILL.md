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

## 查找最新commit的方法

使用 GitHub API 获取各仓库的最新 commit/tag：

```bash
# nixpkgs - 使用 master 分支获取最新
curl -s "https://api.github.com/repos/NixOS/nixpkgs/commits/master" | grep -E '"sha"|"date"' | head -10

# emacs-overlay - 使用 master 分支
curl -s "https://api.github.com/repos/nix-community/emacs-overlay/commits/master" | grep -E '"sha"|"date"' | head -10

# flake-compat - 使用 master 分支
curl -sL "https://api.github.com/repos/edolstra/flake-compat/commits/master" | grep -E '"sha"|"date"' | head -10

# kimi-cli - 获取最新 release tag
curl -s "https://api.github.com/repos/MoonshotAI/kimi-cli/releases/latest" | grep tag_name
```

获取 kimi-cli 的 sha256：
```bash
nix-prefetch-url "https://github.com/MoonshotAI/kimi-cli/archive/refs/tags/<version>.tar.gz"
```

## 更新 home.nix

更新以下部分的 commit hash、日期注释和 sha256（如果是 kimi-cli）：

1. nixpkgs 的 fetchTarball URL
2. emacs-overlay 的 fetchTarball URL
3. kimi-cli 的 version、url 和 sha256

## 构建

更新文件之后，使用 `home-manager -f home.nix build` 构建到一个临时目录。

**重要：构建时间可能很长（几十分钟到一小时），必须使用很长的超时，不要用默认的 300 秒超时！**

## 对比更新

最后使用 `nix store diff-closures` 总结更新的状态并输出。

由于 nix-command 是 experimental feature，需要加上 `--extra-experimental-features` 参数。

示例命令：
```bash
cd /home/blahgeek/.emacs.d/nix

# 获取当前 generation 的 store path（带 (current) 标记的行）
current_gen=$(home-manager generations | grep "(current)" | awk '{print $7}')

# 获取刚 build 的 store path
new_result=$(readlink -f ./result)

# 对比（注意添加 --extra-experimental-features 参数）
nix --extra-experimental-features "nix-command" store diff-closures "$current_gen" "$new_result"
```

或者一行命令：
```bash
nix --extra-experimental-features "nix-command" store diff-closures \
  $(home-manager generations | grep "(current)" | awk '{print $7}') \
  $(readlink -f ./result)
```

## 总结更新的要点

使用 `nix store diff-closures` 获取输出后，按以下优先级总结：

### 1. 命令行工具的大版本更新
关注直接使用的 CLI 工具的主版本或次版本变化：
- 如 fd, fzf, ripgrep, kubectl, docker, just, glab, ast-grep, rclone, uv 等
- 关注 kimi-cli, codex, claude-code, gemini-cli 等 AI 工具

### 2. 重要开发工具/语言的大版本
- Go, Rust, Python, Node.js, Clang, GCC 等编译器/运行时
- LLVM 版本变化
- 数据库（clickhouse, postgresql 等）

### 3. 显著的体积变化
- **+100MB 以上**：可能是新增大量依赖，需要关注
- **-100MB 以上或减小 50% 以上**：优化很好，值得关注
- **新增/删除的大包**：如 llvm 被整个移除等结构性变化

### 4. 其他有意思的改动
- 时区数据 (tzdata) 更新到新年份
- 底层库如 glibc、systemd 的版本跳跃
- 与 emacs-overlay 同步的 emacs-git 更新

### 输出格式建议
按优先级分组，用 emoji 标记重要性：
- ⭐ 大版本更新
- ⚠️ 体积暴增警告
- ✅ 体积优化/移除

这样就完成了，不要switch。
