---
name: upgrade-emacs-packages
description: 更新emacs的所有依赖包（通过straight管理），更新versions文件，并总结更新内容
---

当前emacs的依赖包使用straight.el维护，包的内容在straight/repos/中（每个包是一个git submodule）。

为了执行emacs命令，你应该使用 `emacs --batch -l ~/.emacs.d/early-init.el -l ~/.emacs.d/init.el --eval ...` 。

操作步骤：

## straight-pull-all

运行`(straight-pull-all)`

这个过程中可能出错。可能的几种错误的处理方式：

1. 如果某个repo因为dirty而失败，并且是一些trivial的测试文件或者可以忽略的文件，那么直接进入相应repo清除dirty状态，重试即可；
2. 如果某个repo的主分支名称或者repo url变了，那么进入相应repo使用git命令做相应的更改操作，然后重试；
3. 如果是failed to run git fetch origin，可能是因为repo的remote是git ssh协议的github.com（`git@github.com:...`），那么用git命令把他改成https协议的然后重试（这是因为当前环境下不支持使用git ssh协议）

对于**任何其他原因的错误**，请不要继续，而是停止并等待进一步指令，不要直接重试或者直接调用git pull等操作！！！

## straight-freeze-versions

运行 `(straight-freeze-versions)` 来更新版本至 straight/versions/default.el

这个过程中可能出错。几种错误的处理方式：

1. 如果是提示没有push upstream，并且相应repo的url是blahgeek的，那么可以进入repo后git push，然后重试；

对于**任何其他原因的错误**，请不要继续，而是停止并等待进一步指令！！！

## 总结版本

上述完成后，straight/versions/default.el中记录的版本将会被更新，可以通过git diff获得更新内容。

请对于**每一个**改动的包，根据改动前后的commit，进入其repo目录具体查看它的改动。主要关注新功能或者重大更新，特别是我的init.el中可能用到或者提到的。最后给我输出一个详细的更新总结。
