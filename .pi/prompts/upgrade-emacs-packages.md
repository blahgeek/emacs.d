---
description: 更新 Emacs straight.el 依赖包，freeze versions，并总结每个包的更新内容
---

当前 Emacs 依赖包使用 straight.el 维护，包内容在 `straight/repos/` 中（每个包是一个 git repo/submodule）。请在当前仓库中完成一次依赖升级，并输出详细总结。

执行 Emacs 命令时使用：

```bash
emacs --batch -l ~/.emacs.d/early-init.el -l ~/.emacs.d/init.el --eval '...'
```

## 1. straight-pull-all

运行：

```elisp
(straight-pull-all)
```

可能出错，按以下规则处理：

1. 如果某个 repo 因 dirty worktree 失败，并且 dirty 内容是 trivial 的测试文件、submodule 测试目录或可忽略文件，那么进入对应 repo 清理 dirty 状态，然后重试。
2. 如果某个 repo 的主分支名称或 repo URL 变了，那么进入对应 repo 用 git 命令做相应更改，然后重试。
3. 如果是 `failed to run git fetch origin` / `failed to run git fetch fork`，可能是 remote 是 GitHub SSH 协议（如 `git@github.com:...`），而当前环境不支持 GitHub SSH。此时可以临时把对应 remote 改成 HTTPS 后重试。

对于任何其他原因的错误：不要继续，不要直接重试，也不要直接调用 `git pull`；停止并等待进一步指令。

## 2. straight-freeze-versions

运行：

```elisp
(straight-freeze-versions)
```

这会更新 `straight/versions/default.el`。

可能出错，按以下规则处理：

1. 如果提示没有 push upstream，并且对应 repo 的 URL 是 `blahgeek` 的，那么可以进入 repo 后 `git push`，然后重试。

对于任何其他原因的错误：不要继续；停止并等待进一步指令。

## 3. 总结版本更新

完成后，通过 git diff 查看 `straight/versions/default.el` 的变化。

对于每一个改动的包：

1. 根据 diff 中的 old commit 和 new commit，进入对应 `straight/repos/<repo>`。
2. 用 `git log old..new` / `git diff old..new` 查看具体改动。
3. 总结每个包的更新，重点关注：
   - 明确的新功能；
   - 大版本更新；
   - breaking changes / 最低 Emacs 版本变化 / 依赖变化；
   - 和我的 `init.el`、`custom.el`、theme 文件中实际用到或提到的包相关的影响。
4. 说明升级过程中做过的临时处理，例如 dirty 清理、remote 临时改动、submodule 恢复等。

最终输出：

- 本次成功执行了哪些命令；
- `straight/versions/default.el` 中更新了哪些包；
- 重点更新 / breaking changes / 新 feature；
- 对我当前配置的影响，必须包含准确代码位置（例如 `init.el:3217`）；
- 需要我回归测试的项目。
