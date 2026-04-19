# AGENTS.md

本目录是个人开发环境配置仓库，主要包含 Emacs、Xonsh 与 Nix Home Manager 配置。

## 目录说明

1. **Emacs 配置**
   - 根目录下：`init.el`、模块配置等
   - 包管理：`straight/`（由 straight.el 管理）
   - 包源码与文档：`straight/repos/<package-name>/`

2. **Xonsh 配置**
   - 路径：`./xonshconf/`
   - 用途：Emacs 终端中使用的 shell 配置

3. **Nix Home Manager 配置**
   - 主入口：`./nix/home.nix`。**没有启动flake，传统nix方案**
   - 版本来源：`nix/sources.json`、`nix/sources.nix`（由 `niv` 管理）
   - 附加资源：`nix/etc/`（会被 `home.nix` 引用并安装到系统）

---

## 修改 Elisp 代码注意事项

1. **始终检查括号匹配与 S 表达式结构**
   - 修改后建议至少执行一次结构化检查，避免因括号问题导致启动失败。
   - 推荐命令（batch 检查）：
     ```bash
     emacs --batch -Q -f batch-byte-compile <file.el>
     ```

2. **优先查包源码和 README 再改配置**
   - 先看：`straight/repos/<package-name>/README*`
   - 再看对应 `.el` 源码确认变量、hook、函数调用方式

3. **避免“看起来能跑”的猜测式配置**
   - 不确定 API 时，先检索包源码中的 `defcustom` / `defvar` / `defun`。
   - 旧配置迁移时，确认包版本是否已变更参数名或默认行为。

---

## 修改 Nix 配置注意事项

1. 修改 `nix/home.nix` 或相关配置后，可以在`./nix`目录下运行`home-manager -f home.nix build`构建，并可以从`result`目录中查看结果甚至运行构建出来的程序。但不要运行"switch"使得全局生效。

